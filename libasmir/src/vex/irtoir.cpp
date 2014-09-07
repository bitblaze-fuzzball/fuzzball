#include <string>
#include <vector>
#include <iostream>
#include <assert.h>

#include "irtoir-internal.h"

// enable/disable lazy eflags computation.
// this is for transitional purposes, and should be removed soon.
bool use_eflags_thunks = 0;

bool translate_calls_and_returns = 0;

// Guest architecture we are translating from.
// Set in generate_vine_ir, accessed all over...
// It might be cleaner to pass this around, but that would require a lot of
// refactoring.
VexArch guest_arch = VexArch_INVALID;

using namespace std;

//
// For labeling untranslated VEX IR instructions
//
// Use macros and compile-time concatenation to reduce unneeded copies
// and eliminate a possibly spurious Memcheck warning.
// The argument to these macros must be a string constant. -SMcC
#define uTag(s) ("Unknown: " s)
#define sTag(s) ("Skipped: " s)

//
// Special Exp to record the AST for shl, shr
//

Exp * count_opnd = NULL;

//======================================================================
// Forward declarations
//======================================================================
void modify_flags( asm_program_t *prog, vine_block_t *block );
string get_op_str(asm_program_t *prog, Instruction *inst );
string inst_to_str(asm_program_t *prog, Instruction *inst );
//static void add_special_returns(vine_block_t *block);
static void insert_specials(vine_block_t * block);
void do_cleanups_before_processing();

//======================================================================
// 
// Helper functions for the translation
//
//======================================================================

/// Set whether to use the thunk code with function calls, or not.
void set_use_eflags_thunks(bool value){
  use_eflags_thunks = value;
}

// Terrible name, but to be consistent, named similar to above. 
// Return what the current eflags thunks values is 
bool get_use_eflags_thunks()
{
  return use_eflags_thunks;
}

void set_call_return_translation(int value)
{
  cerr <<"Warning: set_call_return_translation() is deprecated. Use replace_calls_and_returns instead.\n";
  translate_calls_and_returns = (bool) value;
}

//----------------------------------------------------------------------
// A panic function that prints a msg and terminates the program
//----------------------------------------------------------------------
__attribute((noreturn)) void panic( string msg )
{
  ostringstream os;
  os << "Panic: " << msg;
  throw os.str().c_str();
  /*
    cerr << "Panic: " << msg << endl;
    exit(1);
  */
}


//---------------------------------------------------------------------
// Helper wrappers around arch specific functions
//---------------------------------------------------------------------

vector<VarDecl *> get_reg_decls(VexArch arch)
{
  switch (arch) {
  case VexArchX86:
    return i386_get_reg_decls();
  case VexArchAMD64:
    return x64_get_reg_decls();
  case VexArchARM:
    return arm_get_reg_decls();
  default:
    panic("irtoir.cpp: translate_get: unsupported arch");
  }
}

vector<VarDecl *> get_reg_decls(void) {
  return get_reg_decls(guest_arch);
}

// Create a statement updating EIP, or your arch's equivalent PC
// register, to a costant value. Needs to be arch-specific because the
// size and offset of the EIP guest state may be different.
// Note that the calling code requires the statement to be a Put.
IRStmt *make_pc_put_stmt(VexArch arch, Addr64 addr) {
  switch (arch) {
  case VexArchX86:
    return i386_make_pc_put_stmt(addr);
  case VexArchAMD64:
    return x64_make_pc_put_stmt(addr);
  case VexArchARM:
    return arm_make_pc_put_stmt(addr);
  default:
    panic("irtoir.cpp: make_pc_put_stmt: unsupported arch");
  }
}

Exp *translate_get( IRExpr *expr, IRSB *irbb, vector<Stmt *> *irout )
{
    assert(expr);
    assert(irbb);
    assert(irout);

    switch (guest_arch) {
    case VexArchX86:
      return i386_translate_get(expr, irbb, irout);
    case VexArchAMD64:
      return x64_translate_get(expr, irbb, irout);
    case VexArchARM:
      return arm_translate_get(expr, irbb, irout);
    default:
      panic("irtoir.cpp: translate_get: unsupported arch");
    }
}

Stmt *translate_put( IRStmt *stmt, IRSB *irbb, vector<Stmt *> *irout )
{
    switch (guest_arch) {
    case VexArchX86:
      return i386_translate_put(stmt, irbb, irout);
    case VexArchAMD64:
      return x64_translate_put(stmt, irbb, irout);
    case VexArchARM:
      return arm_translate_put(stmt, irbb, irout);
    default:
      panic("translate_put");
    }
}

Stmt *translate_dirty( IRStmt *stmt, IRSB *irbb, vector<Stmt *> *irout )
{
    assert(stmt);
    assert(irbb);
    assert(irout);

    switch (guest_arch) {
    case VexArchX86:
      return i386_translate_dirty(stmt, irbb, irout);
    case VexArchAMD64:
      return x64_translate_dirty(stmt, irbb, irout);
    default:
      return new ExpStmt(new Unknown(uTag("Dirty")));
    }
}

Exp *translate_ccall( IRExpr *expr, IRSB *irbb, vector<Stmt *> *irout )
{
    assert(expr);
    assert(irbb);
    assert(irout);

    switch (guest_arch) {
    case VexArchX86:
      return i386_translate_ccall(expr, irbb, irout);
    case VexArchAMD64:
      return x64_translate_ccall(expr, irbb, irout);
     case VexArchARM:
      return arm_translate_ccall(expr, irbb, irout);
   default:
      panic("translate_ccall");
    }
}

void modify_flags( asm_program_t *prog, vine_block_t *block )
{
    assert(block);
    switch (guest_arch) {
    case VexArchX86:
      return i386_modify_flags(prog, block);
    case VexArchAMD64:
      return x64_modify_flags(prog, block);
    case VexArchARM:
      return arm_modify_flags(prog, block);
    default:
      panic("modify_flags");
    }
}




// Note:
//  VEX uses IRType to specify both the width(in terms of bits) and
//  type(signed/unsigned/float) of an expression. To translate an
//  IRType, we use get_reg_width() and get_reg_type() to extract
//  these two aspects of the expression type from the IRType.

reg_t IRType_to_reg_type( IRType type )
{
    reg_t t;

    switch ( type )
    {
    case Ity_I1:    t = REG_1; break;
    case Ity_I8:    t = REG_8; break; 
    case Ity_I16:   t = REG_16; break;
    case Ity_I32:   t = REG_32; break;
    case Ity_I64:   t = REG_64; break;
    case Ity_F32:   print_debug("warning", 
				"Float32 register encountered"); 
      t = REG_32; break;
    case Ity_F64:   print_debug("warning", 
				"Float64 register encountered");
      t = REG_64; break;
    case Ity_I128:   print_debug("warning", 
				 "Int128 register encountered");
      t = REG_64; break;
    case Ity_V128:   print_debug("warning", 
				 "SIMD128 register encountered");
      t = REG_64; break;
    default:
      assert(0);
    }

    return t;
}


Temp *mk_temp( string name, IRType ty )
{
    reg_t typ = IRType_to_reg_type(ty);
    Temp *ret = new Temp( typ, "T_" + name );
    return ret;
}

string vex_temp_name( IRTemp temp_num, reg_t r_typ )
{
    char name_buf[80];
    // Temps created by translation from VEX IRTemps used to have
    // names that looked like "T_32t6", where "32" is the bitwidth and
    // "6" was the sequential number assigned by VEX. But the size
    // information is now redundant, so I've shortened the names to
    // look like just T6.
    snprintf(name_buf, sizeof(name_buf), "T%d",
	     //get_type_size(r_typ),
	     temp_num);
    return string(name_buf);
}

Temp *mk_temp( IRTemp temp_num, IRType ty )
{
    reg_t r_typ = IRType_to_reg_type(ty);
    return new Temp(r_typ, vex_temp_name(temp_num, r_typ));
}

Temp *mk_temp( reg_t type, vector<Stmt *> *stmts )
{
    static int temp_counter = 0;
    Temp *ret =  new Temp( type, "T_" + int_to_str(temp_counter++) );
    stmts->push_back(new VarDecl(ret));
    return ret;
}


Temp *mk_temp( IRType ty, vector<Stmt *> *stmts )
{
    reg_t typ = IRType_to_reg_type(ty);
    return mk_temp(typ, stmts);
}

Exp *mk_u32(UInt val) {
    return new Constant(REG_32, val);
}

string addr_to_string( Addr64 dest )
{
    char buf[80];
    snprintf(buf, sizeof(buf), "pc_0x%x", (int)dest);
    return string(buf);
}

//----------------------------------------------------------------------
// Takes a destination address and makes a Label out of it. 
// Note that this function and mk_dest_name must produce the same
// string for the same given address!
//----------------------------------------------------------------------
Label *mk_dest_label( Addr64 dest )
{
    return new Label(addr_to_string(dest));
}

//----------------------------------------------------------------------
// Takes a destination address and makes a Name out of it. 
// Note that this function and mk_dest_label must produce the same
// string for the same given address!
//----------------------------------------------------------------------
Name *mk_dest_name( Addr64 dest )
{
    return new Name(addr_to_string(dest));
}


//======================================================================
// 
// Actual translation functions
//
//======================================================================

Exp *translate_16HLto32( Exp *arg1, Exp *arg2 )
{
    assert(arg1);
    assert(arg2);

    Exp *high = new Cast( arg1, REG_32, CAST_UNSIGNED );
    Exp *low = new Cast( arg2, REG_32, CAST_UNSIGNED );

    high = new BinOp( LSHIFT, high, ex_const(16) );

    return new BinOp( BITOR, high, low );
}

//----------------------------------------------------------------------
// arg1 and arg2 are both 32 bit expressions.
// This function returns a 64 bit expression with arg1 occupying the
// high 32 bits and arg2 occupying the low 32 bits.
//----------------------------------------------------------------------
Exp *translate_32HLto64( Exp *arg1, Exp *arg2 )
{
    assert(arg1);
    assert(arg2);

    Exp *high = new Cast( arg1, REG_64, CAST_UNSIGNED );
    Exp *low = new Cast( arg2, REG_64, CAST_UNSIGNED );

    high = new BinOp( LSHIFT, high, ex_const(32) );

    return new BinOp( BITOR, high, low );
}
Exp *translate_64HLto64( Exp *high, Exp *low )
{
    assert(high);
    assert(low);

    high = new Cast( high, REG_32, CAST_LOW );
    low = new Cast( low, REG_32, CAST_LOW );

    return translate_32HLto64( high, low );
}


Exp *translate_DivModU64to32( Exp *arg1, Exp *arg2 )
{
    assert(arg1);
    assert(arg2);
    arg2 = new Cast( arg2, REG_64, CAST_UNSIGNED );
    Exp *div = new BinOp( DIVIDE, arg1, arg2 );
    Exp *mod = new BinOp( MOD, ecl(arg1), ecl(arg2) );

    return translate_64HLto64( mod, div );
}

Exp *translate_DivModS64to32( Exp *arg1, Exp *arg2 )
{
    assert(arg1);
    assert(arg2);
    arg2 = new Cast( arg2, REG_64, CAST_SIGNED );
    Exp *div = new BinOp( SDIVIDE, arg1, arg2 );
    Exp *mod = new BinOp( SMOD, ecl(arg1), ecl(arg2) );

    return translate_64HLto64( mod, div );
}

Exp *translate_MullU8( Exp *arg1, Exp *arg2 )
{
    assert(arg1);
    assert(arg2);

    Exp *wide1 = new Cast( arg1, REG_16, CAST_UNSIGNED );
    Exp *wide2 = new Cast( arg2, REG_16, CAST_UNSIGNED );

    return new BinOp( TIMES, wide1, wide2 );
}

Exp *translate_MullS8( Exp *arg1, Exp *arg2 )
{
    assert(arg1);
    assert(arg2);

    Exp *wide1 = new Cast( arg1, REG_16, CAST_SIGNED );
    Exp *wide2 = new Cast( arg2, REG_16, CAST_SIGNED );

    return new BinOp( TIMES, wide1, wide2 );
}

Exp *translate_MullU16( Exp *arg1, Exp *arg2 )
{
    assert(arg1);
    assert(arg2);

    Exp *wide1 = new Cast( arg1, REG_32, CAST_UNSIGNED );
    Exp *wide2 = new Cast( arg2, REG_32, CAST_UNSIGNED );

    return new BinOp( TIMES, wide1, wide2 );
}

Exp *translate_MullS16( Exp *arg1, Exp *arg2 )
{
    assert(arg1);
    assert(arg2);

    Exp *wide1 = new Cast( arg1, REG_32, CAST_SIGNED );
    Exp *wide2 = new Cast( arg2, REG_32, CAST_SIGNED );

    return new BinOp( TIMES, wide1, wide2 );
}

Exp *translate_MullU32( Exp *arg1, Exp *arg2 )
{
    assert(arg1);
    assert(arg2);

    Exp *wide1 = new Cast( arg1, REG_64, CAST_UNSIGNED );
    Exp *wide2 = new Cast( arg2, REG_64, CAST_UNSIGNED );
    
    return new BinOp( TIMES, wide1, wide2 );
}

Exp *translate_MullS32( Exp *arg1, Exp *arg2 )
{
    assert(arg1);
    assert(arg2);

    Exp *wide1 = new Cast( arg1, REG_64, CAST_SIGNED );
    Exp *wide2 = new Cast( arg2, REG_64, CAST_SIGNED );

    return new BinOp( TIMES, wide1, wide2 );
}

Exp *translate_Clz32( IRExpr *expr, IRSB *irbb, vector<Stmt *> *irout )
{
    assert(expr);
    assert(irbb);
    assert(irout);

    Exp *arg = translate_expr( expr->Iex.Unop.arg, irbb, irout );

    Temp *counter = mk_temp( Ity_I32, irout );
    Temp *temp = mk_temp( typeOfIRExpr(irbb->tyenv, expr->Iex.Unop.arg),irout );

    Label *done = mk_label();

    irout->push_back( new Move( counter, ex_const(32) ) );
    irout->push_back( new Move( temp, arg ) );

    // The previous version of this code created a loop at the IR level.
    // But a number of other parts of our infrastructure can't deal
    // well with loops, so we'll take advantage of the fact we can 
    // unroll this one. This instruction should be rare enough that
    // the space penalty is insignificant.

    // Note that this loop has 33 iterations, corresponding to the 33
    // possible return values from 0 to 32.
    for (int i = 0; i < 33; i++) {
	Label *not_taken = mk_label();
	Exp *cond = new BinOp( EQ, new Temp(*temp), ex_const(0) );

	irout->push_back( new CJmp( cond, new Name(done->label),
				    new Name(not_taken->label) ) );
	irout->push_back( not_taken );

	irout->push_back( new Move( new Temp(*temp),
				    new BinOp( RSHIFT, new Temp(*temp),
					       ex_const(1) ) ) );
	irout->push_back( new Move( new Temp(*counter),
				    new BinOp( MINUS, new Temp(*counter),
					       ex_const(1) ) ) );
    }
    irout->push_back( done );

    return new Temp(*counter);
}

Exp *translate_Ctz32( IRExpr *expr, IRSB *irbb, vector<Stmt *> *irout )
{
    assert(expr);
    assert(irbb);
    assert(irout);

    Exp *arg = translate_expr( expr->Iex.Unop.arg, irbb, irout );

    Temp *counter = mk_temp( Ity_I32, irout );
    Temp *temp = mk_temp( typeOfIRExpr(irbb->tyenv, expr->Iex.Unop.arg),irout );

    Label *done = mk_label();

    irout->push_back( new Move( temp, arg ) );
    irout->push_back( new Move( counter, ex_const(32) ) );

    // See Clz32 above for comments about a similar loop that also
    // apply to this one.
    for (int i = 0; i < 33; i++) {
	Label *not_taken = mk_label();
	Exp *cond = new BinOp( EQ, new Temp(*temp), ex_const(0) );
	
	irout->push_back( new CJmp( cond, new Name(done->label),
				    new Name(not_taken->label) ) );
	irout->push_back( not_taken );
	irout->push_back( new Move( new Temp(*temp),
				    new BinOp( LSHIFT, new Temp(*temp),
					       ex_const(1) ) ) );
	irout->push_back( new Move( new Temp(*counter),
				    new BinOp( MINUS, new Temp(*counter),
					       ex_const(1) ) ) );
    }
    irout->push_back( done );

    return new Temp(*counter);
}

Exp *translate_CmpF64( IRExpr *expr, IRSB *irbb, vector<Stmt *> *irout )
{
    assert(expr);
    assert(irbb);
    assert(irout); 

    Exp *arg1 = translate_expr(expr->Iex.Binop.arg1, irbb, irout);
    Exp *arg2 = translate_expr(expr->Iex.Binop.arg2, irbb, irout);

    Exp *condEQ = new BinOp( EQ, arg1, arg2 );
    Exp *condGT = new BinOp( GT, ecl(arg1), ecl(arg2) );
    Exp *condLT = new BinOp( LT, ecl(arg1), ecl(arg2) );

    Label *labelEQ = mk_label();
    Label *labelGT = mk_label();
    Label *labelLT = mk_label();
    Label *labelUN = mk_label();
    Label *labelNext0 = mk_label();
    Label *labelNext1 = mk_label();
    Label *done = mk_label();

    Temp *temp = mk_temp( Ity_I32,irout );
   
    irout->push_back( new CJmp( condEQ, new Name(labelEQ->label), new Name(labelNext0->label) ) );
    irout->push_back( labelNext0 );
    irout->push_back( new CJmp( condGT, new Name(labelGT->label), new Name(labelNext1->label) ) );
    irout->push_back( labelNext1 );
    irout->push_back( new CJmp( condLT, new Name(labelLT->label), new Name(labelUN->label) ) );
    irout->push_back( labelUN );
    irout->push_back( new Move( new Temp(*temp), ex_const( 0x45 ) ) );
    irout->push_back( new Jmp( new Name(done->label) ) );
    irout->push_back( labelEQ );
    irout->push_back( new Move( new Temp(*temp), ex_const( 0x40 ) ) );
    irout->push_back( new Jmp( new Name(done->label) ) );
    irout->push_back( labelGT );
    irout->push_back( new Move( new Temp(*temp), ex_const( 0x00 ) ) );
    irout->push_back( new Jmp( new Name(done->label) ) );
    irout->push_back( labelLT );
    irout->push_back( new Move( new Temp(*temp), ex_const( 0x01 ) ) );
    irout->push_back( done );

    return temp;
}

Exp *translate_const( IRExpr *expr )
{
    assert(expr);

    IRConst *co = expr->Iex.Const.con;

    const_val_t value;
    reg_t width;

    switch ( co->tag )
      {
        // Your typical unsigned ints
        case Ico_U1:    width = REG_1;    value = co->Ico.U1;     break;
        case Ico_U8:    width = REG_8;    value = co->Ico.U8;     break;
        case Ico_U16:   width = REG_16;   value = co->Ico.U16;    break;
        case Ico_U32:   width = REG_32;   value = co->Ico.U32;    break;
        case Ico_U64:   width = REG_64;   value = co->Ico.U64;    break;

        // Not sure what the diff here is, VEX comments say F64 is IEEE754 floating
        // and F64i is 64 bit unsigned int interpreted literally at IEEE754 double
      case Ico_F64: //  width = I64;   value.floatval = co->Ico.F64;   kind = REG_FLOAT; break;
      case Ico_F64i: // width = I64;   value.floatval = co->Ico.F64i;  kind = REG_FLOAT; break; 
	//return new Unknown("Untranslatable float constant.");
 	                width = REG_64;   value = co->Ico.F64i;   break;
	
      case Ico_V128:
        // These are used in SIMD instructions. We won't be able to handle
	// them correctly without 128-bit types.
	return new Unknown("Untranslatable Ico_V128 constant");
      default:
            panic("Unrecognized constant type");
    }

    Constant *result = new Constant(width, value);

    return result;
}

Exp *translate_simple_unop( IRExpr *expr, IRSB *irbb, vector<Stmt *> *irout )
{

    Exp *arg = translate_expr( expr->Iex.Unop.arg, irbb, irout );

    switch ( expr->Iex.Unop.op )
    {

        case Iop_Not8:
	case Iop_Not16:
	case Iop_Not32:
	case Iop_Not64:    return new UnOp( NOT, arg );
#if VEX_VERSION < 1770
        case Iop_Neg8:
	case Iop_Neg16:
	case Iop_Neg32:
	case Iop_Neg64:    return new UnOp( NEG, arg ); 
#endif
          //        case Iop_NegF64:                return new UnOp( NEG, arg );
        case Iop_NegF64:
	  Exp::destroy(arg);
          return new Unknown("NegF64");
        case Iop_Not1:                  return new UnOp( NOT, arg );

        case Iop_8Uto16:    return new Cast( arg, REG_16, CAST_UNSIGNED );
        case Iop_8Sto16:    return new Cast( arg, REG_16, CAST_SIGNED );
        case Iop_8Uto32:    return new Cast( arg, REG_32, CAST_UNSIGNED );
        case Iop_8Sto32:    return new Cast( arg, REG_32, CAST_SIGNED );
        case Iop_8Uto64:    return new Cast( arg, REG_64, CAST_UNSIGNED );
        case Iop_8Sto64:    return new Cast( arg, REG_64, CAST_SIGNED );
        case Iop_16Uto32:   return new Cast( arg, REG_32, CAST_UNSIGNED );
        case Iop_16Sto32:   return new Cast( arg, REG_32, CAST_SIGNED );
        case Iop_16Uto64:   return new Cast( arg, REG_64, CAST_UNSIGNED );
        case Iop_16Sto64:   return new Cast( arg, REG_64, CAST_SIGNED );
        case Iop_16to8:     return new Cast( arg, REG_8,  CAST_LOW);
        case Iop_16HIto8:   return new Cast( arg, REG_8,  CAST_HIGH);
        case Iop_32Sto64:   return new Cast( arg, REG_64, CAST_SIGNED );
        case Iop_32Uto64:   return new Cast( arg, REG_64, CAST_UNSIGNED );
        case Iop_32to8:     return new Cast( arg, REG_8,  CAST_LOW );
        case Iop_32to16:    return new Cast( arg, REG_16, CAST_LOW );
        case Iop_32HIto16:  return new Cast( arg, REG_16, CAST_HIGH );
        case Iop_64to8:     return new Cast( arg, REG_8,  CAST_LOW );
        case Iop_64to16:    return new Cast( arg, REG_16, CAST_LOW );
        case Iop_64to32:    return new Cast( arg, REG_32, CAST_LOW );
        case Iop_64HIto32:  return new Cast( arg, REG_32, CAST_HIGH );
        case Iop_32to1:     return new Cast( arg, REG_1,  CAST_LOW );
        case Iop_64to1:     return new Cast( arg, REG_1,  CAST_LOW );
        case Iop_1Uto8:     return new Cast( arg, REG_8,  CAST_UNSIGNED );
        case Iop_1Uto32:    return new Cast( arg, REG_32, CAST_UNSIGNED );
        case Iop_1Uto64:    return new Cast( arg, REG_64, CAST_UNSIGNED );
        case Iop_1Sto8:     return new Cast( arg, REG_8,  CAST_SIGNED );
        case Iop_1Sto16:    return new Cast( arg, REG_16, CAST_SIGNED );
        case Iop_1Sto32:    return new Cast( arg, REG_32, CAST_SIGNED );
        case Iop_1Sto64:    return new Cast( arg, REG_64, CAST_SIGNED );
//         case Iop_F32toF64:  return new Cast( arg, REG_64, CAST_FLOAT );
//         case Iop_I32toF64:  return new Cast( arg, REG_64, CAST_FLOAT );

//         case Iop_ReinterpI64asF64:  return new Cast( arg, REG_64, CAST_RFLOAT );
//         case Iop_ReinterpF64asI64:  return new Cast( arg, REG_64, CAST_RINTEGER );

        case Iop_F32toF64: 
#if VEX_VERSION < 1949
        case Iop_I32toF64:
#else
        case Iop_I32StoF64:
        case Iop_I32UtoF64:
#endif
        case Iop_ReinterpI32asF32:
        case Iop_ReinterpF32asI32:
        case Iop_ReinterpI64asF64:
        case Iop_ReinterpF64asI64:
	  Exp::destroy(arg);
          return new Unknown("floatcast");

        default:
            break;
    }

    Exp::destroy(arg);
    return NULL;
}

Exp *translate_unop( IRExpr *expr, IRSB *irbb, vector<Stmt *> *irout )
{
    assert(irbb);
    assert(expr);
    assert(irout);

    Exp *result;

    if ( (result = translate_simple_unop(expr, irbb, irout)) != NULL )
        return result;

    switch ( expr->Iex.Unop.op )
    {
        
        case Iop_Clz32:     return translate_Clz32( expr, irbb, irout );
        case Iop_Ctz32:     return translate_Ctz32( expr, irbb, irout );

        case Iop_AbsF64:
            return new Unknown("Floating point op");

        default:    
	    return new Unknown("Unrecognized unary op");
    }

    return NULL;
}

Exp *translate_simple_binop( IRExpr *expr, IRSB *irbb, vector<Stmt *> *irout )
{
    Exp *arg1 = translate_expr(expr->Iex.Binop.arg1, irbb, irout);
    Exp *arg2 = translate_expr(expr->Iex.Binop.arg2, irbb, irout);
    
    switch ( expr->Iex.Binop.op ) 
    {
        case Iop_Add8:
	case Iop_Add16:
	case Iop_Add32:
	case Iop_Add64:        return new BinOp(PLUS, arg1, arg2);
        case Iop_Sub8:
	case Iop_Sub16:
	case Iop_Sub32: 
	case Iop_Sub64:        return new BinOp(MINUS, arg1, arg2);
        case Iop_Mul8:
	case Iop_Mul16:
	case Iop_Mul32:
	case Iop_Mul64:        return new BinOp(TIMES, arg1, arg2);
        case Iop_Or8:
	case Iop_Or16:
	case Iop_Or32:
	case Iop_Or64:          return new BinOp(BITOR, arg1, arg2);
        case Iop_And8:
	case Iop_And16:
	case Iop_And32:
	case Iop_And64:        return new BinOp(BITAND, arg1, arg2);
        case Iop_Xor8:
	case Iop_Xor16:
	case Iop_Xor32:
	case Iop_Xor64:        return new BinOp(XOR, arg1, arg2);
        case Iop_Shl8:
	case Iop_Shl16:
	case Iop_Shl32:
        case Iop_Shl64:   {  if (!count_opnd && !use_eflags_thunks)  count_opnd = arg2;
                        	return new BinOp(LSHIFT, arg1, arg2);}
        case Iop_Shr8:
	case Iop_Shr16:
	case Iop_Shr32:
	case Iop_Shr64:   {  if (!count_opnd && !use_eflags_thunks)  count_opnd = arg2;
                           	return new BinOp(RSHIFT, arg1, arg2); }
        case Iop_Sar8:
	case Iop_Sar16:
	case Iop_Sar32:
        case Iop_Sar64:   {  if (!count_opnd && !use_eflags_thunks)  count_opnd = arg2;
                           	return new BinOp(ARSHIFT, arg1, arg2); }
        case Iop_CmpEQ8:
	case Iop_CmpEQ16:
	case Iop_CmpEQ32:
	case Iop_CmpEQ64:
#if VEX_VERSION >= 1907
        case Iop_CasCmpEQ8:
	case Iop_CasCmpEQ16:
	case Iop_CasCmpEQ32:
	case Iop_CasCmpEQ64:
#endif
	  return new BinOp(EQ, arg1, arg2);
        case Iop_CmpNE8:
	case Iop_CmpNE16:
	case Iop_CmpNE32:
	case Iop_CmpNE64:
#if VEX_VERSION >= 1907
        case Iop_CasCmpNE8:
	case Iop_CasCmpNE16:
	case Iop_CasCmpNE32:
	case Iop_CasCmpNE64:
#endif
#if VEX_VERSION >= 2599
        case Iop_ExpCmpNE8:
	case Iop_ExpCmpNE16:
	case Iop_ExpCmpNE32:
	case Iop_ExpCmpNE64:
#endif
	  return new BinOp(NEQ, arg1, arg2);

        case Iop_CmpLT32U: return new BinOp(LT, arg1, arg2);
        case Iop_CmpLE32U: return new BinOp(LE, arg1, arg2);

        case Iop_16HLto32:          return translate_16HLto32(arg1, arg2);
        case Iop_32HLto64:          return translate_32HLto64(arg1, arg2);
        case Iop_MullU8:            return translate_MullU8(arg1, arg2);
        case Iop_MullS8:            return translate_MullS8(arg1, arg2);
        case Iop_MullU16:           return translate_MullU16(arg1, arg2);
        case Iop_MullS16:           return translate_MullS16(arg1, arg2);
        case Iop_MullU32:           return translate_MullU32(arg1, arg2);
        case Iop_MullS32:           return translate_MullS32(arg1, arg2);
        case Iop_DivModU64to32:     return translate_DivModU64to32(arg1, arg2);
        case Iop_DivModS64to32:     return translate_DivModS64to32(arg1, arg2);

    case Iop_DivU32: return new BinOp(DIVIDE, arg1, arg2);
    case Iop_DivS32: return new BinOp(SDIVIDE, arg1, arg2);
    case Iop_DivU64: return new BinOp(DIVIDE, arg1, arg2);
    case Iop_DivS64: return new BinOp(SDIVIDE, arg1, arg2);
        default:
            break;
    }

    Exp::destroy(arg1);
    Exp::destroy(arg2);

    return NULL;
}

Exp *translate_binop( IRExpr *expr, IRSB *irbb, vector<Stmt *> *irout )
{
    assert(irbb);
    assert(expr);
    assert(irout);

    //Exp *arg2;
    Exp *result;

    if ( (result = translate_simple_binop(expr, irbb, irout)) != NULL )
        return result;

    switch ( expr->Iex.Binop.op ) 
    {
        case Iop_CmpF64:
        // arg1 in this case, specifies rounding mode, and so is ignored
#if VEX_VERSION < 1949
        case Iop_I64toF64:
        case Iop_F64toI64:
        case Iop_F64toI32:
        case Iop_F64toI16:
#else
        case Iop_I64StoF64:
        case Iop_F64toI64S:
        case Iop_F64toI32S:
        case Iop_F64toI32U:
        case Iop_F64toI16S:
#endif
        case Iop_F64toF32:          
        case Iop_RoundF64toInt:
        case Iop_2xm1F64:
            return new Unknown("Floating point binop");

//         case Iop_CmpF64:            return translate_CmpF64(expr, irbb, irout);

//         // arg1 in this case, specifies rounding mode, and so is ignored
//         case Iop_I64toF64:
//             arg2 = translate_expr(expr->Iex.Binop.arg2, irbb, irout);
//             return new Cast( arg2, REG_64, CAST_FLOAT );
//         case Iop_F64toI64:
//             arg2 = translate_expr(expr->Iex.Binop.arg2, irbb, irout);
//             return new Cast( arg2, REG_64, CAST_INTEGER );
//         case Iop_F64toF32:          
//             arg2 = translate_expr(expr->Iex.Binop.arg2, irbb, irout);
//             return new Cast( arg2, REG_32, CAST_FLOAT );
//         case Iop_F64toI32:
//             arg2 = translate_expr(expr->Iex.Binop.arg2, irbb, irout);
//             return new Cast( arg2, REG_32, CAST_INTEGER );
//         case Iop_F64toI16:
//             arg2 = translate_expr(expr->Iex.Binop.arg2, irbb, irout);
//             return new Cast( arg2, REG_16, CAST_INTEGER );

//         case Iop_RoundF64toInt:
//         case Iop_2xm1F64:
//             return new Unknown("Floating point op");

        default:  
	    return new Unknown("Unrecognized binary op");
    }

    return NULL;
}

Exp *translate_triop( IRExpr *expr, IRSB *irbb, vector<Stmt *> *irout )
{
    assert(irbb);
    assert(expr);
    assert(irout);

    //
    // Tri-ops tend to be analogs of binary arithmetic ops but for 
    // floating point operands instead of plain integers. As such, 
    // the first argument specifies the rounding mode, which we have
    // chosen to ignore for now. See Notes for detailed explanation.
    //
#if VEX_VERSION < 2366
    Exp *arg2 = translate_expr(expr->Iex.Triop.arg2, irbb, irout);
    Exp *arg3 = translate_expr(expr->Iex.Triop.arg3, irbb, irout);
#else
    Exp *arg2 = translate_expr(expr->Iex.Triop.details->arg2, irbb, irout);
    Exp *arg3 = translate_expr(expr->Iex.Triop.details->arg3, irbb, irout);
#endif

    switch (
#if VEX_VERSION < 2366
	    expr->Iex.Triop.op
#else
	    expr->Iex.Triop.details->op
#endif
	    ) 
    {

        case Iop_AddF64:
        case Iop_SubF64:
        case Iop_MulF64:
        case Iop_DivF64:
//         case Iop_AddF64:    return new BinOp(PLUS, arg2, arg3);
//         case Iop_SubF64:    return new BinOp(MINUS, arg2, arg3);
//         case Iop_MulF64:    return new BinOp(TIMES, arg2, arg3);
//         case Iop_DivF64:    return new BinOp(DIVIDE, arg2, arg3);

        case Iop_Yl2xF64:   
        case Iop_Yl2xp1F64:
        case Iop_ScaleF64:
	    Exp::destroy(arg2); Exp::destroy(arg3);
            return new Unknown("Floating point triop");

        default:  
	    Exp::destroy(arg2); Exp::destroy(arg3);
            return new Unknown("Unrecognized ternary op");
    }

    return NULL;
}


/* Formerly "emit_mux0x", but that name is misleading as to the
   argument ordering convention. This code uses the same convention as
   an "if-then-else" statement (hence the name), or C's "?:" operator:
   the expression returns the second argument if the first argument is
   true, and the third argument if the first argument is false. VEX's
   Mux0X is similar, but uses the opposite convention (or, depending
   on how you look at it, incorporates an implicit "== 0" check as
   part of the condition). Compare translate_mux0x below. */
/* Later versions of VEX renamed Mux0X to ITE and switched the
   argument order to the same one as ?: */
Exp *emit_ite( vector<Stmt *> *irout, reg_t type,
	       Exp *cond, Exp *exp_t, Exp *exp_f )
{
    assert(cond);
    assert(exp_t);
    assert(exp_f);

    // Every instance of temps and labels should have their own object,
    // i.e. always new Label(label) instead of just label. So the labels
    // and temps created above are used only once after which they need
    // to be cloned for each subsequent use. This keeps the expression tree
    // a tree instead of a graph.

    Temp *temp = mk_temp(type,irout);

#ifdef MUX_AS_BITS
    Exp *widened_cond;
    widened_cond = mk_temp(type,irout);
    irout->push_back(new Move(ecl(widened_cond),
			      new Cast(cond, type,
				       CAST_SIGNED)));

    // tmp = x&c | y&~c
    irout->push_back(new Move(ecl(temp), 
			      new BinOp(BITOR,
					new BinOp(BITAND,
						  exp_t,
						  ecl(widened_cond)),
					new BinOp(BITAND,
						  exp_f,
						  new UnOp(NOT, 
							   widened_cond)))));
#elif defined(MUX_AS_CJMP)
    Label *label_f = mk_label();
    Label *done = mk_label();

    // match_ite depends on the order/types of these statements
    // if changing them here, make sure to make the corresponding changes there
    irout->push_back( new Move( new Temp(*temp), exp_t ) );
    irout->push_back( new CJmp( cond, new Name(done->label), new Name(label_f->label) ) );
    irout->push_back( label_f );
    irout->push_back( new Move( new Temp(*temp), exp_f ) );
    irout->push_back( done );
#else
    irout->push_back( new Move( new Temp(*temp),
				_ex_ite(cond, exp_t, exp_f)));

#endif

    return temp;
}


Exp *translate_mux0x( IRExpr *expr, IRSB *irbb, vector<Stmt *> *irout )
{
    assert(expr);
    assert(irbb);
    assert(irout);

#if VEX_VERSION < 2668
    IRExpr *cond = expr->Iex.Mux0X.cond;
    IRExpr *expr_f = expr->Iex.Mux0X.expr0;
    IRExpr *expr_t = expr->Iex.Mux0X.exprX;
#else
    IRExpr *cond = expr->Iex.ITE.cond;
    IRExpr *expr_t = expr->Iex.ITE.iftrue;
    IRExpr *expr_f = expr->Iex.ITE.iffalse;
#endif

    assert(cond);
    assert(expr_t);
    assert(expr_f);

    // It's assumed that both true and false expressions have the
    // same type so that when we create a temp to assign it to, we
    // can just use one type
    reg_t type = IRType_to_reg_type( typeOfIRExpr(irbb->tyenv, expr_f) );
    reg_t cond_type = IRType_to_reg_type (typeOfIRExpr(irbb->tyenv, cond) );

    Exp *condE = translate_expr(cond, irbb, irout);
    Exp *exp_t = translate_expr(expr_t, irbb, irout);
    Exp *exp_f = translate_expr(expr_f, irbb, irout);

    if (cond_type == REG_1) {
      // Condition is already a boolean: simple.
      return emit_ite(irout, type, condE, exp_t, exp_f);
    } else {
      // Condition is wider. Add "!= 0" check. We used to add a "== 0"
      // check and flip the two sides of the branch, but flipping makes
      // things more confusing later.
      condE = _ex_neq(condE, ex_const(cond_type, 0));
      return emit_ite(irout, type, condE, exp_t, exp_f);
    }
}

Exp *translate_load( IRExpr *expr, IRSB *irbb, vector<Stmt *> *irout )
{
    assert(expr);
    assert(irbb);
    assert(irout);

    Exp *addr;
    Mem *mem;
    reg_t rtype;

    rtype = IRType_to_reg_type(expr->Iex.Load.ty);

    addr = translate_expr(expr->Iex.Load.addr, irbb, irout);
    mem = new Mem(addr, rtype);

    return mem;
}

Exp *translate_tmp( IRTemp temp, IRSB *irbb, vector<Stmt *> *irout )
{
    assert(temp != IRTemp_INVALID);
    assert(irbb);
    assert(irout);

    IRType type = typeOfIRTemp(irbb->tyenv, temp);
    return mk_temp(temp, type);

}

Exp *translate_tmp_ex( IRExpr *expr, IRSB *irbb, vector<Stmt *> *irout ) {
    assert(expr);
    return translate_tmp(expr->Iex.RdTmp.tmp, irbb, irout);
}


//----------------------------------------------------------------------
// Translate a single expression
//----------------------------------------------------------------------
Exp *translate_expr( IRExpr *expr, IRSB *irbb, vector<Stmt *> *irout )
{
    assert(irout);

    Exp *result = NULL;
    string func;

    switch ( expr->tag )
    {
        case Iex_Binder:
            result = new Unknown(sTag("Binder"));
            break;
        case Iex_Get:
            result = translate_get(expr, irbb, irout);
            break;
        case Iex_GetI:
	    result = new Unknown(uTag("GetI"));
            break;
        case Iex_RdTmp:
            result = translate_tmp_ex(expr, irbb, irout);
            break;
        case Iex_Triop:
            result = translate_triop(expr, irbb, irout);
            break;
        case Iex_Binop:
            result = translate_binop(expr, irbb, irout);
            break;
        case Iex_Unop:
            result = translate_unop(expr, irbb, irout);
            break;
        case Iex_Load:
            result = translate_load(expr, irbb, irout);
            break;
        case Iex_Const:
            result = translate_const(expr);
            break;
#if VEX_VERSION < 2668
        case Iex_Mux0X:
#else
        case Iex_ITE:
#endif
            result = translate_mux0x(expr, irbb, irout);
            break;
        case Iex_CCall:
            result = translate_ccall(expr, irbb, irout);
            break;
        default:
	    assert(0);
    }

    return result;
}

Stmt *mk_assign_tmp(IRTemp tmp, Exp *rhs_e, IRSB *irbb,
		    vector<Stmt *> *irout ) {
    assert(tmp != IRTemp_INVALID);
    assert(rhs_e);
    assert(irbb);
    assert(irout);

    IRType type = typeOfIRTemp(irbb->tyenv, tmp);
    return new Move( mk_temp(tmp, type), rhs_e );
}

Stmt *translate_tmp_st( IRStmt *stmt, IRSB *irbb, vector<Stmt *> *irout )
{
    return mk_assign_tmp(stmt->Ist.WrTmp.tmp,
			 translate_expr(stmt->Ist.WrTmp.data, irbb, irout),
			 irbb, irout);
}



Stmt *translate_store( IRStmt *stmt, IRSB *irbb, vector<Stmt *> *irout )
{
    assert(stmt);
    assert(irbb);
    assert(irout);

    Exp *dest;
    Exp *data;
    Mem *mem;
    IRType itype;
    reg_t rtype;

    dest = translate_expr(stmt->Ist.Store.addr, irbb, irout);
    itype = typeOfIRExpr(irbb->tyenv, stmt->Ist.Store.data);
    rtype = IRType_to_reg_type(itype);
    mem = new Mem(dest, rtype);
    data = translate_expr(stmt->Ist.Store.data, irbb, irout);

    return new Move(mem, data); 
}

Stmt *translate_imark( IRStmt *stmt, IRSB *irbb )
{
    assert(stmt);
    
    return mk_dest_label( stmt->Ist.IMark.addr );
}

Stmt *translate_exit( IRStmt *stmt, IRSB *irbb, vector<Stmt *> *irout )
{
    assert(stmt);
    assert(irbb);
    assert(irout);

    // We assume the jump destination is always ever a 32 bit uint
    assert(stmt->Ist.Exit.dst->tag == Ico_U32);

    Exp *cond = translate_expr( stmt->Ist.Exit.guard, irbb, irout );

    if (stmt->Ist.Exit.jk == Ijk_MapFail) {
      // MapFail is only used for bailing out, and the target is always
      // the beginning of the same instruction, right? At least that seems
      // to currently be true in VEX r1774.
      
      return new Assert(new UnOp(NOT,cond));
    }
    
    Name *dest = mk_dest_name( stmt->Ist.Exit.dst->Ico.U32 );
    Label *next = mk_label();

    irout->push_back( new CJmp( cond, dest, new Name(next->label) ) );

    return next;
}

#if VEX_VERSION >= 1901
Stmt *translate_cas( IRStmt *stmt, IRSB *irbb, vector<Stmt *> *irout ) {
    assert(stmt);
    assert(irbb);
    assert(irout);

    IRCAS *cas = stmt->Ist.CAS.details;

    assert(cas->end == Iend_LE); // Assume little-endian
    int hi_offset = 0;

    IRType ty = typeOfIRTemp(irbb->tyenv, cas->oldLo);
    reg_t rty = IRType_to_reg_type(ty);

    if (cas->expdHi) {
	switch (rty) {
	case  REG_8: hi_offset = 1; break;
	case REG_16: hi_offset = 2; break;
	case REG_32: hi_offset = 4; break;
	case REG_64: hi_offset = 8; break;
	default: panic("Unexpected type in double CAS");
	}
    }

    // The original value in .addr is copied to .oldLo, unconditionally
    Exp *addr = translate_expr(cas->addr, irbb, irout);
    Mem *mem = new Mem(addr, rty);
    irout->push_back( mk_assign_tmp(cas->oldLo, mem, irbb, irout) );
    // (and similarly for oldHi, if present)
    if (hi_offset) {
	assert(cas->oldHi != IRTemp_INVALID);
	Exp *base_addr = translate_expr(cas->addr, irbb, irout);
	Exp *offset = mk_u32(hi_offset);
	Exp *addr = new BinOp(PLUS, base_addr, offset);
	Mem *mem = new Mem(addr, rty);
	irout->push_back( mk_assign_tmp(cas->oldHi, mem, irbb, irout) );
    }
    
    // check "old == expected"
    Exp *eq_lhs = translate_tmp(cas->oldLo, irbb, irout);
    Exp *eq_rhs = translate_expr(cas->expdLo, irbb, irout);
    Exp *equality = new BinOp(EQ, eq_lhs, eq_rhs);
    // (and similarly for oldHi and expdHi, if present)
    if (hi_offset) {
	Exp *eq_lhs = translate_tmp(cas->oldHi, irbb, irout);
	Exp *eq_rhs = translate_expr(cas->expdHi, irbb, irout);
	equality = new BinOp(BITAND, equality,
			     new BinOp(EQ, eq_lhs, eq_rhs));
    }    
    Temp *cond_temp = mk_temp(REG_1, irout);
    irout->push_back(new Move(cond_temp, equality));

    // If .addr contains the same value as .expdLo,
    // then .dataLo is written there
    // We essentially do "*addr = (oldLo == expdLo) ? dataLo : oldLo"
    Exp *addr2 = translate_expr(cas->addr, irbb, irout);
    Mem *mem2 = new Mem(addr2, rty);
    Exp *store_val = emit_ite(irout, rty, ecl(cond_temp),
			      translate_expr(cas->dataLo, irbb, irout),
			      translate_tmp(cas->oldLo, irbb, irout));
    Stmt *last_st = new Move(mem2, store_val);
    // (and similarly expdHi, if present)
    if (hi_offset) {
	Exp *base_addr = translate_expr(cas->addr, irbb, irout);
	Exp *offset = mk_u32(hi_offset);
	Exp *addr = new BinOp(PLUS, base_addr, offset);
	Mem *mem = new Mem(addr, rty);
	Exp *store_val = emit_ite(irout, rty, ecl(cond_temp),
				  translate_expr(cas->dataHi, irbb, irout),
				  translate_tmp(cas->oldHi, irbb, irout));
	irout->push_back(last_st);
	last_st = new Move(mem, store_val);
    }
    return last_st;
}
#endif
    
#if VEX_VERSION >= 1930
/* As with translate_cas above, we assume the no-contention case and
   don't translate any of the actual synchronization aspects: for
   instance, the store conditional actually always succeeds. */
Stmt *translate_llsc( IRStmt *stmt, IRSB *irbb, vector<Stmt *> *irout ) {
    assert(stmt);
    assert(irbb);
    assert(irout);

    assert(stmt->Ist.LLSC.end == Iend_LE); // Assume little-endian
    Exp *addr = translate_expr(stmt->Ist.LLSC.addr, irbb, irout);
    
    if (stmt->Ist.LLSC.storedata == NULL) {
	/* Load linked. C.f. translate_load above */
	reg_t rtype =
	    IRType_to_reg_type(typeOfIRTemp(irbb->tyenv,
					    stmt->Ist.LLSC.result));
	Mem *mem = new Mem(addr, rtype);
	return mk_assign_tmp(stmt->Ist.LLSC.result, mem, irbb, irout);
    } else {
	/* Store conditional. C.f. translate_store above */
	reg_t rtype =
	    IRType_to_reg_type(typeOfIRExpr(irbb->tyenv,
					    stmt->Ist.LLSC.storedata));

	Exp *data = translate_expr(stmt->Ist.LLSC.storedata, irbb, irout);
	Mem *mem = new Mem(addr, rtype);
	irout->push_back(new Move(mem, data));
	return mk_assign_tmp(stmt->Ist.LLSC.result, new Constant(REG_1, 1),
			     irbb, irout);
    }
}
#endif

//----------------------------------------------------------------------
// Translate a single statement
//----------------------------------------------------------------------
Stmt *translate_stmt( IRStmt *stmt, IRSB *irbb, vector<Stmt *> *irout )
{
    assert(stmt);
    assert(irout);

    Stmt *result = NULL;

    switch ( stmt->tag )
    {
        case Ist_NoOp:
            result = new Comment("NoOp");
            break;
        case Ist_IMark:
            result = translate_imark(stmt, irbb);
            break;
        case Ist_AbiHint:
            result = new Comment(sTag("AbiHint"));
            break;
        case Ist_Put:
            result = translate_put(stmt, irbb, irout);
            break;
        case Ist_PutI:
            result = new ExpStmt(new Unknown(uTag("PutI"))); 
            break;
        case Ist_WrTmp:
            result = translate_tmp_st(stmt, irbb, irout);
            break;
        case Ist_Store:
            result = translate_store(stmt, irbb, irout);
            break;
        case Ist_Dirty:
            result = translate_dirty(stmt, irbb, irout);
            break;
        case Ist_MFence:
            result = new Comment(sTag("MFence")); 
            break;
        case Ist_Exit:
            result = translate_exit(stmt, irbb, irout);
            break;
#if VEX_VERSION >= 1901
        case Ist_CAS:
	    result = translate_cas(stmt, irbb, irout);
	    break;
#endif
#if VEX_VERSION >= 1930
        case Ist_LLSC:
	    result = translate_llsc(stmt, irbb, irout);
	    break;
#endif
        default:
	    assert(0);
    }

    assert(result);

    return result;
}

Stmt *translate_jumpkind( IRSB *irbb, vector<Stmt *> *irout )
{
  assert(irbb);

  Stmt *result = NULL;

  Exp *dest = NULL;
  if ( irbb->next->tag == Iex_Const )
    dest = mk_dest_name( irbb->next->Iex.Const.con->Ico.U32 );
  else
    dest = translate_expr( irbb->next, irbb, irout );

  switch ( irbb->jumpkind )
    {
    case Ijk_Boring: 
    case Ijk_Yield:
      result = new Jmp(dest);
      break; 
    case Ijk_Call:
      if(!translate_calls_and_returns)
	result = new Jmp(dest);
      else
	result = new Call(NULL, dest, vector<Exp *>());
      break;
    case Ijk_Ret:
      if(!translate_calls_and_returns)
	result = new Jmp(dest);
      else
	result = new Return(NULL);
      break;
    case Ijk_Sys_int128:
      irout->push_back( new Special("int 0x80") );
      irout->push_back(mk_label());
      result = new Jmp(dest);
      break;
#if VEX_VERSION >= 1874
    case Ijk_Sys_int129:
      irout->push_back( new Special("int 0x81") );
      irout->push_back(mk_label());
      result = new Jmp(dest);
      break;
    case Ijk_Sys_int130:
      irout->push_back( new Special("int 0x81") );
      irout->push_back(mk_label());
      result = new Jmp(dest);
      break;
#endif
    case Ijk_Sys_sysenter:
      irout->push_back( new Special("sysenter") );
      irout->push_back(mk_label());
      result = new Jmp(dest);
      break;
#if VEX_VERSION >= 1949
    case Ijk_Sys_syscall:
      irout->push_back( new Special("syscall") );
      irout->push_back(mk_label());
      result = new Jmp(dest);
      break;
#endif
    case Ijk_NoDecode:
      result = new Special("VEX decode error");
      Exp::destroy(dest);
      break;
#if VEX_VERSION >= 1571
#if VEX_VERSION >= 1786
    case Ijk_SigTRAP:
#else
    case Ijk_Trap:
#endif
      result = new Special("trap");
      Exp::destroy(dest);
      break;
#endif
#if VEX_VERSION >= 1786
    case Ijk_SigSEGV:
      result = new Special("SIGSEGV");
      Exp::destroy(dest);
      break;
#endif
#if VEX_VERSION >= 1320
    case Ijk_TInval:
      irout->push_back( new Special("TInval") );
      result = new Jmp(dest);
      break; 
#endif
    default:
      assert(0);
    }

  assert(result);

  return result;
}


// FIXME: call arch specific functions
bool is_special( Instruction *inst )
{
  return false;
}

vector<Stmt *> *translate_special( Instruction *inst )
{
  panic("Why did this get called? We are now saying that no instruction is a special.");
}

//----------------------------------------------------------------------
// Translate an IRSB into a vector of Stmts in our IR
//----------------------------------------------------------------------
vector<Stmt *> *translate_irbb( Instruction *inst, IRSB *irbb )
{

    //
    // It's assumed that each irbb only contains the translation for
    // 1 instruction
    //
    //    assert(inst);

    //
    // Certain instructions are special and need to be 
    // handled separately
    //
    if (inst != NULL && is_special(inst))
        return translate_special(inst);
    
    // For some instructions, the eflag affecting IR needs out-of-band
    // arguments. This function cleans up those operands.
    do_cleanups_before_processing ();
    
    assert(irbb);

    vector<Stmt *> *irout = new vector<Stmt *>();    

    assert(irout);

    int i;
    Stmt *st = NULL;

    //
    // Translate all the statements
    //
    IRStmt *stmt;
    stmt = irbb->stmts[0];
    assert(stmt->tag == Ist_IMark);

    st = translate_stmt(stmt, irbb, irout);
    assert(st->stmt_type == LABEL);
    irout->push_back(st);

    for(int i = 0; i < irbb->tyenv->types_used; i++){
      IRType ty = irbb->tyenv->types[i];
      reg_t typ = IRType_to_reg_type(ty);
      string name = vex_temp_name(i, typ);

      irout->push_back(new VarDecl(name, typ));
    }
    
    for ( i = 1; i < irbb->stmts_used; i++ )
    {
        IRStmt *stmt = irbb->stmts[i];

	st = translate_stmt(stmt, irbb, irout);

        irout->push_back(st);
    }

    //
    // Translate the jump at the end of the BB
    //
    st = translate_jumpkind(irbb, irout);

    irout->push_back(st);

    return irout;
}

VexArch vexarch_of_prog(asm_program_t *prog) {
  switch (prog->asmir_arch) {
  case asmir_arch_x86: return VexArchX86;
  case asmir_arch_x64: return VexArchAMD64;
  case asmir_arch_arm: return VexArchARM;
  default:
    return VexArch_INVALID;
  }
}

//======================================================================
// 
// Utility functions that wrap the raw translation functions.
// These are what should be used to do the actual translation.
// See print-ir.cpp in ../../apps for examples of how to use these.
//
//======================================================================


vine_block_t* generate_vex_ir(VexArch guest, Instruction *inst)
{
  vine_block_t *vblock = new vine_block_t;

  vblock->inst = inst;
  
  // Skip the VEX translation of special instructions because these
  // are also the ones that VEX does not handle
  if ( !is_special( inst ))
    vblock->vex_ir = translate_insn(guest, inst->bytes, inst->address,
				    inst->arch_flags);
  else
    vblock->vex_ir = NULL;

  return vblock;
}

//----------------------------------------------------------------------
// Take a vector of instrs function and translate it into VEX IR blocks
// and store them in the vector of vine blocks
//----------------------------------------------------------------------
vector<vine_block_t *> generate_vex_ir(asm_program_t *prog,
				       const vector<Instruction *> &instrs)
{
  vector<vine_block_t *> results;
  VexArch guest = vexarch_of_prog(prog);

  Instruction *inst;
  for(vector<Instruction *>::const_iterator it = instrs.begin();
      it != instrs.end(); it++){
    inst = *it;
    vine_block_t *vblock = generate_vex_ir(guest, inst);
    results.push_back(vblock);
  }
  return results;
}


//----------------------------------------------------------------------
// Take a disassembled function and translate it into VEX IR blocks
// and store them in the vector of vine blocks
//----------------------------------------------------------------------
vector<vine_block_t *> generate_vex_ir(asm_program_t *prog,
				       asm_function_t *func)
{
  vector<vine_block_t *> results;
  VexArch guest = vexarch_of_prog(prog);

  Instruction *inst = NULL;
  for(map<address_t, Instruction *>::const_iterator i = 
	func->instmap.begin(); i != func->instmap.end(); i++){
    address_t addr = i->first;
    assert(!inst || inst->address+inst->length == addr);
    inst = i->second;
    if (!inst) {
      cerr << "Warning: No instruction for " << addr << " in func "
	   <<func->name <<", assuming end of function.\n";
      break;
    }
    
    assert(inst);

    // Allocate a vine block for this instruction and it's translation
    vine_block_t *vblock = new vine_block_t;
    assert(vblock);
    
    vblock->inst = inst;
    

    // Skip the VEX translation of special instructions because these
    // are also the ones that VEX does not handle
    if ( !is_special( inst ) )
      vblock->vex_ir = translate_insn(guest, inst->bytes, addr,
				      inst->arch_flags);
    else
      vblock->vex_ir = NULL;
    
    results.push_back(vblock);
    
  }
  return results;
}


//----------------------------------------------------------------------
// Take a disassembled program and translate it into VEX IR blocks
// and store them in the vector of vine blocks
//----------------------------------------------------------------------
vector<vine_block_t *> generate_vex_ir( asm_program_t *prog )
{
    assert(prog);

    // Init the translation library
    translate_init();

    vector<vine_block_t *> results;

    // For each function in this program...
    for ( map<address_t, asm_function_t *>::const_iterator i = prog->functions.begin(); 
          i != prog->functions.end(); i++ )
    {
        asm_function_t *func = i->second;

	vector<vine_block_t *> tmpres = generate_vex_ir(prog, func);
	results.insert(results.end(), tmpres.begin(), tmpres.end());
    }

    return results;
}

/*
static void add_special_returns(vine_block_t *block)
{
  if(block->inst == NULL) return;
  // If this is a return statement, make note of it
  if(block->inst->opcode[0] == 0xC2 ||
     block->inst->opcode[0] == 0xC3 ||
     block->inst->opcode[0] == 0xCA ||
     block->inst->opcode[0] == 0xCB){
    block->vine_ir->push_back(new Special("ret"));
    block->vine_ir->push_back(mk_label());
  }

}
*/

/**
 * Insert both special("call") and special("ret") into the code.
 * This function should be replacing add_special_returns()
 */
void insert_specials(vine_block_t * block) {
    IRSB* bb = block->vex_ir;
    if (bb == NULL)
        return;
    IRJumpKind kind = bb->jumpkind;
    switch (kind) {
        case Ijk_Call:
	  if(!translate_calls_and_returns)
            block->vine_ir->push_back(new Special("call"));
	  break;
        case Ijk_Ret:
	  if(!translate_calls_and_returns) {
            block->vine_ir->push_back(new Special("ret"));
	    block->vine_ir->push_back(mk_label());
	  }
	  break;
        default:
            // do nothing
            break;
    }
}


// Translate a single block to Vine. guest_arch must be set already
void generate_vine_ir_block( asm_program_t *prog, vine_block_t *block )
{
  static unsigned int ir_addr = 100;

  
  // Translate the block
  block->vine_ir = translate_irbb( block->inst, block->vex_ir );
  assert(block->vine_ir);
  vector<Stmt *> *vir = block->vine_ir;
  
  // Go through block and add Special's for ret
  //add_special_returns(block);
  insert_specials(block);
  
  // Go through the block and add on eflags modifications
  if(!use_eflags_thunks)
    modify_flags(prog, block);
  
  // Delete EFLAGS get thunks
  //del_get_thunk(block->vine_ir);
  
  // Add the asm and ir addresses
  for ( unsigned int j = 0; j < vir->size(); j++ )
    {
      if(block->inst)
	vir->at(j)->asm_address = block->inst->address;
      vir->at(j)->ir_address = ir_addr++;
    }
}

vector<vine_block_t *>
generate_vine_ir( asm_program_t *prog, vector<vine_block_t *> vblocks )
{
    unsigned int vblocksize = vblocks.size();

    // Set the global everyone else will look at.
    guest_arch = vexarch_of_prog(prog);

    for ( unsigned int i = 0; i < vblocksize; i++ )
    {
        vine_block_t *block = vblocks.at(i);
        assert(block);

	if(is_debug_on("vex")) {
	  // FIXME: memory leak?
	  if(block->inst)
	    fprintf(stdout, "vex: asm: %s",
		    inst_to_str(prog, block->inst).c_str());
	  FILE *old_fp = change_vex_debug_out(stdout);
	  ppIRSB(block->vex_ir);
	  change_vex_debug_out(old_fp);
	}

	generate_vine_ir_block(prog, block);
    }
    return vblocks;
}


vector<Stmt *> merge_ir( vector<vine_block_t *> vblocks )
{
    unsigned int i;
    vector<Stmt *> result;

    for ( i = 0; i < vblocks.size(); i++ )
    {
        vine_block_t *block = vblocks.at(i);
        assert(block);

        vector<Stmt *> *inner = block->vine_ir;

        result.insert( result.end(), inner->begin(), inner->end() );
    }

    return result;
}

//----------------------------------------------------------------------
// 
// Helpers
//
//----------------------------------------------------------------------

string inst_to_str(asm_program_t *prog, Instruction *inst )
{
    assert(inst);

    ostringstream stream;

    ostream_insn(prog, inst, stream);

    return stream.str();
}

string get_op_str(asm_program_t *prog, Instruction *inst )
{
    assert(inst);

    string str = inst_to_str(prog, inst);

    istringstream stream(str);
    string token;
    getline(stream, token, '\t');
    getline(stream, token, '\t');

    return token;
}

// Needed to be able to delete the Mux0X statements in shift instructions
// (And so formerly known as "match_mux0x", but renamed to be clearer
// about the ordering convention)
// Checks whether the statements starting at ir->at(i) are a pattern
// equivalent to "res = cond ? exp_t : exp_f". If there is a match,
// writes the matching subexpressions in the expression pointer
// pointers, and returns 0. If there is no match, returns -1 and the
// expression pointers are unchanged.
int match_ite(vector<Stmt*> *ir, unsigned int i,
	      Exp **cond, Exp **exp_t,	Exp **exp_f, Exp **res)
{
  // this code depends on the order of statements from emit_ite()

  if (i < 0 || i >= ir->size())
    return -1;
  if (ir->at(i)->stmt_type == MOVE &&
      ((Move *)(ir->at(i)))->rhs->exp_type == ITE) {
    Move *s0 = (Move*)ir->at(i);
    if (s0->lhs->exp_type != TEMP)
      return -1;
    Ite *ite = (Ite *)s0->rhs;
    //cout <<"match_ite (ite case) matched!\n";
    if (cond)
      *cond = ite->cond;
    if (exp_t)
      *exp_t = ite->true_e;
    if (exp_f)
      *exp_f = ite->false_e;
    if (res)
      *res = s0->lhs;
  } else if (ir->at(i)->stmt_type == MOVE
      && ir->at(i+3)->stmt_type == MOVE
      && ir->at(i+2)->stmt_type == LABEL
      && ir->at(i+4)->stmt_type == LABEL
      && ir->at(i+1)->stmt_type == CJMP) {
    Move *s0 = (Move*)ir->at(i);
    CJmp *s1 = (CJmp*)ir->at(i+1);
    Label *s2 = (Label*)ir->at(i+2);
    Move *s3 = (Move*)ir->at(i+3);
    Label *s4 = (Label*)ir->at(i+4);

    if (s0->lhs->exp_type != TEMP
	|| s3->lhs->exp_type != TEMP
	|| ((Temp*)s0->lhs)->name != ((Temp*)s3->lhs)->name)
      return -1;

    if (s1->t_target->exp_type != NAME
	|| s1->f_target->exp_type != NAME
	|| ((Name*)s1->f_target)->name != s2->label
	|| ((Name*)s1->t_target)->name != s4->label
	)
      return -1;

    //cout <<"match_ite (cjmp case) matched!\n";
    if (cond)
      *cond = s1->cond;
    if (exp_t)
      *exp_t = s0->rhs;
    if (exp_f)
      *exp_f = s3->rhs;
    if (res)
      *res = s0->lhs;
  } else if (ir->at(i)->stmt_type == MOVE
	     && ir->at(i+1)->stmt_type == MOVE) {
    // Check for the pattern:
    //   i: wc = cast(cond)S;
    // i+1: res = (exp_t & wc) | (exp1 & !wc)
    Move *s0 = (Move*)ir->at(i);
    Move *s1 = (Move*)ir->at(i+1);
    if (s0->lhs->exp_type != TEMP
	|| s0->rhs->exp_type != CAST
	|| s1->lhs->exp_type != TEMP
	|| s1->rhs->exp_type != BINOP)
      return -1;
    Cast *cast = (Cast*)s0->rhs;
    BinOp *bin_or = (BinOp*)s1->rhs;
    if (cast->cast_type != CAST_SIGNED ||
	bin_or->binop_type != BITOR ||
	bin_or->lhs->exp_type != BINOP ||
	bin_or->rhs->exp_type != BINOP)
      return -1;
    BinOp *bin_and_t = (BinOp*)bin_or->lhs;
    BinOp *bin_and_f = (BinOp*)bin_or->rhs;
    if (bin_and_t->binop_type != BITAND ||
	bin_and_t->rhs->exp_type != TEMP ||
	bin_and_f->binop_type != BITAND ||
	bin_and_f->rhs->exp_type != UNOP)
      return -1;
    UnOp *un_not = (UnOp*)bin_and_f->rhs;
    if (un_not->unop_type != NOT ||
	un_not->exp->exp_type != TEMP)
      return -1;
    Temp *wc1 = (Temp*)s0->lhs;
    Temp *wc2 = (Temp*)bin_and_t->rhs;
    Temp *wc3 = (Temp*)un_not->exp;
    if (wc1->name != wc2->name || wc1->name != wc3->name)
      return -1;
    //cout <<"match_ite (bitop case) matched!\n";
    if (cond)
      *cond = cast->exp;
    if (exp_t)
      *exp_t = bin_and_t->lhs;
    if (exp_f)
      *exp_f = bin_and_f->lhs;
    if (res)
      *res = s1->lhs;
  } else {
    return -1;
  }
  return 0;
}

reg_t get_exp_type_from_cast( Cast *cast )
{
    assert(cast);

    return cast->typ;

}

reg_t get_exp_type( Exp *exp )
{
    assert(exp);

    reg_t type;

    if ( exp->exp_type == TEMP )
    {
        type = ((Temp *)exp)->typ;
    }
    else if ( exp->exp_type == CONSTANT )
    {
        type = ((Constant *)exp)->typ;
    }
    else
    {
        panic("Expression has no type info: " + exp->tostring());
    }

    return type;
}


void 
do_cleanups_before_processing()
{
    if (count_opnd) {
	count_opnd = NULL;
    }
}

// where does this really want to live?
char* string_blockinsn(asm_program_t *prog, vine_block_t *block) {
  return string_of_insn(prog, block->inst);
}

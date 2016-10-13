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
// register, to a constant value. Needs to be arch-specific because the
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

Exp *translate_geti( IRExpr *expr, IRSB *irbb, vector<Stmt *> *irout )
{
    assert(expr);
    assert(irbb);
    assert(irout);

    switch (guest_arch) {
    case VexArchX86:
      return i386_translate_geti(expr, irbb, irout);
    case VexArchAMD64:
      return x64_translate_geti(expr, irbb, irout);
    case VexArchARM:
      return new Unknown(uTag("GetI"));
    default:
      panic("irtoir.cpp: translate_geti: unsupported arch");
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

Stmt *translate_puti( IRStmt *stmt, IRSB *irbb, vector<Stmt *> *irout )
{
    switch (guest_arch) {
    case VexArchX86:
      return i386_translate_puti(stmt, irbb, irout);
    case VexArchAMD64:
      return x64_translate_puti(stmt, irbb, irout);
    case VexArchARM:
      return new ExpStmt(new Unknown(uTag("PutI")));
    default:
      panic("irtoir.cpp: translate_puti: unsupported arch");
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

void mk_temps128(IRTemp temp_num, Temp **high_p, Temp **low_p) {
    string name = vex_temp_name(temp_num, REG_64);
    string name_h = name + "h";
    string name_l = name + "l";
    *high_p = new Temp(REG_64, name_h);
    *low_p = new Temp(REG_64, name_l);
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

Temp *mk_temp_def(reg_t type, Exp *val, vector<Stmt *> *stmts) {
    Temp *t = mk_temp(type, stmts);
    stmts->push_back(new Move(t, val));
    return t;
}

Exp *mk_u32(UInt val) {
    return new Constant(REG_32, val);
}

Exp *mk_u64(uint64_t val) {
    return new Constant(REG_64, val);
}

string addr_to_string( Addr64 dest )
{
    char buf[80];
    if (guest_arch == VexArchAMD64)
	snprintf(buf, sizeof(buf), "pc_0x%llx", (unsigned long long)dest);
    else
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

/* My convention about the "AxB" notation is the opposite of VEX's: in
   function names like the following, 2 is the number of lanes and 8
   bits is their size. */

Exp *assemble2x8(Exp *b1, Exp *b0) {
    Exp *b1w = _ex_u_cast(b1, REG_16);
    Exp *b0w = _ex_u_cast(b0, REG_16);
    return _ex_or(_ex_shl(b1w, 8), b0w);
}

Exp *translate_16HLto32( Exp *arg1, Exp *arg2 )
{
    assert(arg1);
    assert(arg2);

    Exp *high = new Cast( arg1, REG_32, CAST_UNSIGNED );
    Exp *low = new Cast( arg2, REG_32, CAST_UNSIGNED );

    high = new BinOp( LSHIFT, high, ex_const(16) );

    return new BinOp( BITOR, high, low );
}

Exp *assemble4x8(Exp *b3, Exp *b2, Exp *b1, Exp *b0) {
    Exp *b3w = _ex_u_cast(b3, REG_16);
    Exp *b2w = _ex_u_cast(b2, REG_16);
    Exp *b1w = _ex_u_cast(b1, REG_16);
    Exp *b0w = _ex_u_cast(b0, REG_16);
    Exp *high = _ex_or(_ex_shl(b3w, 8), b2w);
    Exp *low = _ex_or(_ex_shl(b1w, 8), b0w);
    return translate_16HLto32(high, low);
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

Exp *assemble4x16(Exp *w3, Exp *w2, Exp *w1, Exp *w0) {
    Exp *w3w = _ex_u_cast(w3, REG_32);
    Exp *w2w = _ex_u_cast(w2, REG_32);
    Exp *w1w = _ex_u_cast(w1, REG_32);
    Exp *w0w = _ex_u_cast(w0, REG_32);
    Exp *high = _ex_or(_ex_shl(w3w, 16), w2w);
    Exp *low = _ex_or(_ex_shl(w1w, 16), w0w);
    return translate_32HLto64(high, low);
}

Exp *assemble8x8(Exp *b7, Exp *b6, Exp *b5, Exp *b4,
		 Exp *b3, Exp *b2, Exp *b1, Exp *b0) {
    return translate_32HLto64(assemble4x8(b7, b6, b5, b4),
			      assemble4x8(b3, b2, b1, b0));
}

Exp *translate_64HLto128( Exp *arg1, Exp *arg2 )
{
    assert(arg1);
    assert(arg2);

    return new Vector(arg1, arg2);
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

/* This is complicated because we want to implement it using only
   32-to-64-bit multiplication, so we have to do four different
   partial products and add them together being careful of carries. */
Exp *translate_MullU64( Exp *arg1, Exp *arg2, vector<Stmt *> *irout )
{
    assert(arg1);
    assert(arg2);

    Exp *x1 = _ex_u_cast(_ex_h_cast(arg1, REG_32), REG_64);
    Exp *x0 = _ex_u_cast( ex_l_cast(arg1, REG_32), REG_64);
    Exp *y1 = _ex_u_cast(_ex_h_cast(arg2, REG_32), REG_64);
    Exp *y0 = _ex_u_cast( ex_l_cast(arg2, REG_32), REG_64);

    x1 = mk_temp_def(REG_64, x1, irout);
    x0 = mk_temp_def(REG_64, x0, irout);
    y1 = mk_temp_def(REG_64, y1, irout);
    y0 = mk_temp_def(REG_64, y0, irout);

    Exp *p11 = ex_mul(x1, y1);
    Exp *p10 = ex_mul(x1, y0);
    Exp *p01 = ex_mul(x0, y1);
    Exp *p00 = ex_mul(x0, y0);

    p11 = mk_temp_def(REG_64, p11, irout);
    p10 = mk_temp_def(REG_64, p10, irout);
    p01 = mk_temp_def(REG_64, p01, irout);
    p00 = mk_temp_def(REG_64, p00, irout);

    Exp *s0_a = mk_temp_def(REG_64, _ex_add(ecl(p00), ex_shl(p01, 32)), irout);
    Exp *c0_a = _ex_u_cast(ex_lt(s0_a, p00), REG_64);
    Exp *s0_b = mk_temp_def(REG_64, _ex_add(ecl(s0_a), ex_shl(p10, 32)),irout);
    Exp *c0_b = _ex_u_cast(ex_lt(s0_b, s0_a), REG_64);

    Exp *s1 = _ex_add(_ex_add(ecl(p11), _ex_add(c0_a, c0_b)),
		      _ex_add(ex_shr(p01, 32), ex_shr(p10, 32)));

    return translate_64HLto128(s1, ecl(s0_b));
}

void split_vector(Exp *exp_v, Exp **high, Exp **low) {
    if (exp_v->exp_type == VECTOR) {
	// Expected case
	Vector *v = (Vector *)exp_v;
	*high = v->lanes[1];
	*low = v->lanes[0];
	delete v; // Shallow delete, since we reuse high and low
    } else if (exp_v->exp_type == UNKNOWN) {
	// An error or unhandled case. Propagate times two.
	*high = exp_v;
	*low = ecl(exp_v);
    } else {
	assert(exp_v->exp_type == VECTOR);
    }
}

Exp *mk_temps128_def(Exp *val, vector<Stmt *> *stmts) {
    Temp *t_high = mk_temp(REG_64, stmts);
    Temp *t_low = mk_temp(REG_64, stmts);
    Exp *v_high, *v_low;
    split_vector(val, &v_high, &v_low);
    stmts->push_back(new Move(t_high, v_high));
    stmts->push_back(new Move(t_low, v_low));
    return new Vector(ecl(t_high), ecl(t_low));
}

Exp *translate_Neg128(Exp *arg, vector<Stmt *> *irout) {
    Exp *arg_high, *arg_low;
    split_vector(arg, &arg_high, &arg_low);

    Exp *not_high = _ex_not(arg_high);
    Exp *not_low = _ex_not(arg_low);

    Exp *inc_low = mk_temp_def(REG_64, _ex_add(not_low, mk_u64(1)), irout);
    Exp *inc_high =
	_ex_add(not_high, _ex_u_cast(_ex_eq(ecl(inc_low), mk_u64(0)), REG_64));

    return translate_64HLto128(inc_high, ecl(inc_low));
}

Exp *vec_ite(Exp *cond, Exp *exp_t, Exp *exp_f) {
    Exp *t_high, *t_low;
    split_vector(exp_t, &t_high, &t_low);

    Exp *f_high, *f_low;
    split_vector(exp_f, &f_high, &f_low);

    return new Vector(_ex_ite(ecl(cond), t_high, f_high),
		      _ex_ite(ecl(cond), t_low, f_low));
}

/* Again this can't be translated directly without REG_128s. Implement
   by splitting on cases for the signs and then using the unsigned
   version. */
Exp *translate_MullS64( Exp *arg1, Exp *arg2, vector<Stmt *> *irout )
{
    assert(arg1);
    assert(arg2);

    Exp *neg1 = mk_temp_def(REG_1, _ex_lt(arg1, mk_u64(0)), irout);
    Exp *neg2 = mk_temp_def(REG_1, _ex_lt(arg2, mk_u64(0)), irout);

    Exp *abs1 = _ex_ite(ecl(neg1), ex_neg(arg1), ecl(arg1));
    Exp *abs2 = _ex_ite(ecl(neg2), ex_neg(arg2), ecl(arg2));

    Exp *absprod =
	mk_temps128_def(translate_MullU64(abs1, abs2, irout), irout);

    Exp *negprod = mk_temp_def(REG_1, ex_xor(neg1, neg2), irout);

    return vec_ite(negprod, translate_Neg128(absprod, irout), ecl(absprod));
}

/* Here we just ignore the high half of the dividend. That happens to
   work right in the common x64 case in which div on a 64-bit register
   implements 64-bit / or %. However in other cases it's just wrong. A
   true 128-bit divide would be too complex to practically represent
   in IR. */
Exp *translate_DivModU128to64( Exp *arg1, Exp *arg2 )
{
    assert(arg1);
    assert(arg2);
    Exp *arg1h, *arg1l;
    split_vector(arg1, &arg1h, &arg1l);
    delete arg1h;

    Exp *div = new BinOp( DIVIDE, arg1l, arg2 );
    Exp *mod = new BinOp( MOD, ecl(arg1l), ecl(arg2) );

    return translate_64HLto128( mod, div );
}

Exp *translate_DivModS128to64( Exp *arg1, Exp *arg2 )
{
    assert(arg1);
    assert(arg2);
    Exp *arg1h, *arg1l;
    split_vector(arg1, &arg1h, &arg1l);
    delete arg1h;

    Exp *div = new BinOp( SDIVIDE, arg1l, arg2 );
    Exp *mod = new BinOp( SMOD, ecl(arg1l), ecl(arg2) );

    return translate_64HLto128( mod, div );
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

Exp *translate_Clz64( IRExpr *expr, IRSB *irbb, vector<Stmt *> *irout )
{
    assert(expr);
    assert(irbb);
    assert(irout);

    Exp *arg = translate_expr( expr->Iex.Unop.arg, irbb, irout );

    Temp *counter = mk_temp( Ity_I64, irout );
    Temp *temp = mk_temp( typeOfIRExpr(irbb->tyenv, expr->Iex.Unop.arg),irout );

    Label *done = mk_label();

    irout->push_back( new Move( counter, mk_u64(64) ) );
    irout->push_back( new Move( temp, arg ) );

    // Note that this loop has 65 iterations, corresponding to the 65
    // possible return values from 0 to 64.
    for (int i = 0; i < 65; i++) {
	Label *not_taken = mk_label();
	Exp *cond = new BinOp( EQ, new Temp(*temp), mk_u64(0) );

	irout->push_back( new CJmp( cond, new Name(done->label),
				    new Name(not_taken->label) ) );
	irout->push_back( not_taken );

	irout->push_back( new Move( new Temp(*temp),
				    new BinOp( RSHIFT, new Temp(*temp),
					       ex_const(1) ) ) );
	irout->push_back( new Move( new Temp(*counter),
				    new BinOp( MINUS, new Temp(*counter),
					       mk_u64(1) ) ) );
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

Exp *translate_Ctz64( IRExpr *expr, IRSB *irbb, vector<Stmt *> *irout )
{
    assert(expr);
    assert(irbb);
    assert(irout);

    Exp *arg = translate_expr( expr->Iex.Unop.arg, irbb, irout );

    Temp *counter = mk_temp( Ity_I64, irout );
    Temp *temp = mk_temp( typeOfIRExpr(irbb->tyenv, expr->Iex.Unop.arg),irout );

    Label *done = mk_label();

    irout->push_back( new Move( temp, arg ) );
    irout->push_back( new Move( counter, mk_u64(64) ) );

    // See Clz64 above for comments about a similar loop that also
    // apply to this one.
    for (int i = 0; i < 65; i++) {
	Label *not_taken = mk_label();
	Exp *cond = new BinOp( EQ, new Temp(*temp), mk_u64(0) );

	irout->push_back( new CJmp( cond, new Name(done->label),
				    new Name(not_taken->label) ) );
	irout->push_back( not_taken );
	irout->push_back( new Move( new Temp(*temp),
				    new BinOp( LSHIFT, new Temp(*temp),
					       ex_const(1) ) ) );
	irout->push_back( new Move( new Temp(*counter),
				    new BinOp( MINUS, new Temp(*counter),
					       mk_u64(1) ) ) );
    }
    irout->push_back( done );

    return new Temp(*counter);
}


Exp *translate_CmpF(Exp *arg1, Exp *arg2, reg_t sz) {
    /* arg1 == arg2 ? 0x40 :
         (arg1 < arg2 ? 0x01 :
	     (arg1 > arg2 ? 0x00 : 0x45)) */
    /* This weird combination of operations does a comparison of
       floating-point values, including the unordered case that can
       happen with NaNs, and represents the results as bits that could
       go into x86-style EFLAGS. VEX has standardized on this encoding,
       presumably for historical reasons. */
    (void)sz; // Not needed for the current implementation
    Exp *cmp_eq = new FBinOp(FEQ, ROUND_NEAREST, arg1, arg2);
    Exp *cmp_lt = new FBinOp(FLT, ROUND_NEAREST, ecl(arg1), ecl(arg2));
    Exp *cmp_gt = new FBinOp(FLT, ROUND_NEAREST, ecl(arg2), ecl(arg1));
    return _ex_ite(cmp_eq, ex_const(0x40),
		   _ex_ite(cmp_lt, ex_const(0x01),
			   _ex_ite(cmp_gt, ex_const(0), ex_const(0x45))));
}

// Low-lane single-precision FP, as in the x86 SSE addss, for instance.
// The low 32-bits are operated on, and the high 96 bits are passed through
// from the left operand.
Exp *translate_low32fp_128_binop(fbinop_type_t op, Exp *left, Exp *right) {
    Exp *left_high, *left_low;
    split_vector(left, &left_high, &left_low);
    Exp *left32 = _ex_l_cast(left_low, REG_32);

    Exp *right_high, *right_low;
    split_vector(right, &right_high, &right_low);
    Exp *right32 = _ex_l_cast(right_low, REG_32);
    Exp::destroy(right_high); // unusued

    Exp *result32 = new FBinOp(op, ROUND_NEAREST, left32, right32);
    Exp *lane3 = ex_h_cast(left_low, REG_32);
    Exp *result64 = translate_32HLto64(lane3, result32);
    return translate_64HLto128(left_high, result64);
}

// Low-lane double-precision FP, as in the x86 SSE addsd, for instance.
// The low 64-bits are operated on, and the high 64 bits are passed through
// from the left operand.
Exp *translate_low64fp_128_binop(fbinop_type_t op, Exp *left, Exp *right) {
    Exp *left_high, *left_low;
    split_vector(left, &left_high, &left_low);

    Exp *right_high, *right_low;
    split_vector(right, &right_high, &right_low);
    Exp::destroy(right_high); // unusued

    Exp *result64 = new FBinOp(op, ROUND_NEAREST, left_low, right_low);
    return translate_64HLto128(left_high, result64);
}

// Bitwise concatenate 4 32-bit values, high to low, into a 128-bit
// value.
Exp *assemble4x32(Exp *e1, Exp *e2, Exp *e3, Exp *e4) {
    Exp *high = translate_32HLto64(e1, e2);
    Exp *low = translate_32HLto64(e3, e4);

    return new Vector(high, low);
}

// SIMD FP on 4 single-precision values at once, as in the x86 addps.
Exp *translate_par32fp_128_binop(fbinop_type_t op, Exp *left, Exp *right) {
    Exp *left_high, *left_low;
    split_vector(left, &left_high, &left_low);
    Exp *left1 = _ex_h_cast(left_high, REG_32);
    Exp *left2 = ex_l_cast(left_high, REG_32);
    Exp *left3 = _ex_h_cast(left_low, REG_32);
    Exp *left4 = ex_l_cast(left_low, REG_32);

    Exp *right_high, *right_low;
    split_vector(right, &right_high, &right_low);
    Exp *right1 = _ex_h_cast(right_high, REG_32);
    Exp *right2 = ex_l_cast(right_high, REG_32);
    Exp *right3 = _ex_h_cast(right_low, REG_32);
    Exp *right4 = ex_l_cast(right_low, REG_32);

    Exp *result1 = new FBinOp(op, ROUND_NEAREST, left1, right1);
    Exp *result2 = new FBinOp(op, ROUND_NEAREST, left2, right2);
    Exp *result3 = new FBinOp(op, ROUND_NEAREST, left3, right3);
    Exp *result4 = new FBinOp(op, ROUND_NEAREST, left4, right4);

    return assemble4x32(result1, result2, result3, result4);
}

// SIMD FP on 2 double-precision values at once, as in the x86 addpd.
Exp *translate_par64fp_128_binop(fbinop_type_t op, Exp *left, Exp *right) {
    Exp *left_h, *left_l;
    split_vector(left, &left_h, &left_l);

    Exp *right_h, *right_l;
    split_vector(right, &right_h, &right_l);

    Exp *result_h = new FBinOp(op, ROUND_NEAREST, left_h, right_h);
    Exp *result_l = new FBinOp(op, ROUND_NEAREST, left_l, right_l);

    return translate_64HLto128(result_h, result_l);
}

void split4x8(Exp *x32, Exp **b3, Exp **b2, Exp **b1, Exp **b0) {
    *b3 = _ex_h_cast( ex_h_cast(x32, REG_16), REG_8);
    *b2 = _ex_l_cast( ex_h_cast(x32, REG_16), REG_8);
    *b1 = _ex_h_cast( ex_l_cast(x32, REG_16), REG_8);
    *b0 = _ex_l_cast(_ex_l_cast(x32, REG_16), REG_8);
}

void split8x8(Exp *x64,
	      Exp **b7, Exp **b6, Exp **b5, Exp **b4,
	      Exp **b3, Exp **b2, Exp **b1, Exp **b0) {
    split4x8(_ex_h_cast(x64, REG_32), b7, b6, b5, b4);
    split4x8( ex_l_cast(x64, REG_32), b3, b2, b1, b0);
}

void split4x16(Exp *x64, Exp **w3, Exp **w2, Exp **w1, Exp **w0) {
    *w3 = _ex_h_cast( ex_h_cast(x64, REG_32), REG_16);
    *w2 = _ex_l_cast( ex_h_cast(x64, REG_32), REG_16);
    *w1 = _ex_h_cast( ex_l_cast(x64, REG_32), REG_16);
    *w0 = _ex_l_cast(_ex_l_cast(x64, REG_32), REG_16);
}

void split2x32(Exp *x64, Exp **w1, Exp **w0) {
    *w1 =  ex_h_cast(x64, REG_32);
    *w0 = _ex_l_cast(x64, REG_32);
}

Exp *translate_CmpEQ32x2(Exp *a, Exp *b) {
    Exp *a1, *a0;
    split2x32(a, &a1, &a0);
    Exp *b1, *b0;
    split2x32(b, &b1, &b0);
    Exp *r1 = _ex_s_cast(_ex_eq(a1, b1), REG_32);
    Exp *r0 = _ex_s_cast(_ex_eq(a0, b0), REG_32);
    return translate_32HLto64(r1, r0);
}

Exp *translate_CmpEQ32x4(Exp *a, Exp *b) {
    Exp *a_high, *a_low;
    split_vector(a, &a_high, &a_low);

    Exp *b_high, *b_low;
    split_vector(b, &b_high, &b_low);

    Exp *r_high = translate_CmpEQ32x2(a_high, b_high);
    Exp *r_low = translate_CmpEQ32x2(a_low, b_low);

    return translate_64HLto128(r_high, r_low);
}

Exp *translate_CmpEQ8x8(Exp *a, Exp *b) {
    Exp *a7, *a6, *a5, *a4, *a3, *a2, *a1, *a0;
    split8x8(a, &a7, &a6, &a5, &a4, &a3, &a2, &a1, &a0);
    Exp *b7, *b6, *b5, *b4, *b3, *b2, *b1, *b0;
    split8x8(b, &b7, &b6, &b5, &b4, &b3, &b2, &b1, &b0);
    Exp *r7 = _ex_s_cast(_ex_eq(a7, b7), REG_8);
    Exp *r6 = _ex_s_cast(_ex_eq(a6, b6), REG_8);
    Exp *r5 = _ex_s_cast(_ex_eq(a5, b5), REG_8);
    Exp *r4 = _ex_s_cast(_ex_eq(a4, b4), REG_8);
    Exp *r3 = _ex_s_cast(_ex_eq(a3, b3), REG_8);
    Exp *r2 = _ex_s_cast(_ex_eq(a2, b2), REG_8);
    Exp *r1 = _ex_s_cast(_ex_eq(a1, b1), REG_8);
    Exp *r0 = _ex_s_cast(_ex_eq(a0, b0), REG_8);
    return assemble8x8(r7, r6, r5, r4, r3, r2, r1, r0);
}

Exp *translate_CmpEQ8x16(Exp *a, Exp *b) {
    Exp *a_high, *a_low;
    split_vector(a, &a_high, &a_low);

    Exp *b_high, *b_low;
    split_vector(b, &b_high, &b_low);

    Exp *r_high = translate_CmpEQ8x8(a_high, b_high);
    Exp *r_low = translate_CmpEQ8x8(a_low, b_low);

    return translate_64HLto128(r_high, r_low);
}

Exp *translate_CmpGT8Sx8(Exp *a, Exp *b) {
    Exp *a7, *a6, *a5, *a4, *a3, *a2, *a1, *a0;
    split8x8(a, &a7, &a6, &a5, &a4, &a3, &a2, &a1, &a0);
    Exp *b7, *b6, *b5, *b4, *b3, *b2, *b1, *b0;
    split8x8(b, &b7, &b6, &b5, &b4, &b3, &b2, &b1, &b0);
    Exp *r7 = _ex_s_cast(_ex_slt(b7, a7), REG_8);
    Exp *r6 = _ex_s_cast(_ex_slt(b6, a6), REG_8);
    Exp *r5 = _ex_s_cast(_ex_slt(b5, a5), REG_8);
    Exp *r4 = _ex_s_cast(_ex_slt(b4, a4), REG_8);
    Exp *r3 = _ex_s_cast(_ex_slt(b3, a3), REG_8);
    Exp *r2 = _ex_s_cast(_ex_slt(b2, a2), REG_8);
    Exp *r1 = _ex_s_cast(_ex_slt(b1, a1), REG_8);
    Exp *r0 = _ex_s_cast(_ex_slt(b0, a0), REG_8);
    return assemble8x8(r7, r6, r5, r4, r3, r2, r1, r0);
}

Exp *translate_CmpGT8Sx16(Exp *a, Exp *b) {
    Exp *a_high, *a_low;
    split_vector(a, &a_high, &a_low);

    Exp *b_high, *b_low;
    split_vector(b, &b_high, &b_low);

    Exp *r_high = translate_CmpGT8Sx8(a_high, b_high);
    Exp *r_low = translate_CmpGT8Sx8(a_low, b_low);

    return translate_64HLto128(r_high, r_low);
}

Exp *translate_CmpGT32Sx2(Exp *a, Exp *b) {
    Exp *a1, *a0;
    split2x32(a, &a1, &a0);
    Exp *b1, *b0;
    split2x32(b, &b1, &b0);
    Exp *r1 = _ex_s_cast(_ex_slt(b1, a1), REG_32);
    Exp *r0 = _ex_s_cast(_ex_slt(b0, a0), REG_32);
    return translate_32HLto64(r1, r0);
}

Exp *translate_CmpGT32Sx4(Exp *a, Exp *b) {
    Exp *a_high, *a_low;
    split_vector(a, &a_high, &a_low);

    Exp *b_high, *b_low;
    split_vector(b, &b_high, &b_low);

    Exp *r_high = translate_CmpGT32Sx2(a_high, b_high);
    Exp *r_low = translate_CmpGT32Sx2(a_low, b_low);

    return translate_64HLto128(r_high, r_low);
}

Exp *assemble8x1(Exp *b7, Exp *b6, Exp *b5, Exp *b4,
		 Exp *b3, Exp *b2, Exp *b1, Exp *b0) {
    b7 = _ex_shl(_ex_u_cast(b7, REG_8), 7);
    b6 = _ex_shl(_ex_u_cast(b6, REG_8), 6);
    b5 = _ex_shl(_ex_u_cast(b5, REG_8), 5);
    b4 = _ex_shl(_ex_u_cast(b4, REG_8), 4);
    b3 = _ex_shl(_ex_u_cast(b3, REG_8), 3);
    b2 = _ex_shl(_ex_u_cast(b2, REG_8), 2);
    b1 = _ex_shl(_ex_u_cast(b1, REG_8), 1);
    b0 = _ex_u_cast(b0, REG_8);
    return _ex_or(_ex_or(b7, b6, b5, b4),
		  _ex_or(b3, b2, b1, b0));
}

Exp *translate_GetMSBs8x8(Exp *x) {
    Exp *b7 = _ex_h_cast(x, REG_1);
    Exp *b6 = ex_get_bit(x, 55);
    Exp *b5 = ex_get_bit(x, 47);
    Exp *b4 = ex_get_bit(x, 39);
    Exp *b3 = ex_get_bit(x, 31);
    Exp *b2 = ex_get_bit(x, 23);
    Exp *b1 = ex_get_bit(x, 15);
    Exp *b0 = ex_get_bit(x,  7);
    return assemble8x1(b7, b6, b5, b4, b3, b2, b1, b0);
}

Exp *translate_GetMSBs8x16(Exp *x) {
    Exp *x_h, *x_l;
    split_vector(x, &x_h, &x_l);
    Exp *r_h = translate_GetMSBs8x8(x_h);
    Exp *r_l = translate_GetMSBs8x8(x_l);
    return assemble2x8(r_h, r_l);
}

Exp *interleave2_2x32(Exp *a, Exp *b) {
    Exp *a1, *a0;
    split2x32(a, &a1, &a0);
    Exp *b1, *b0;
    split2x32(b, &b1, &b0);
    return assemble4x32(a1, b1, a0, b0);
}

Exp *translate_InterleaveLO32x4(Exp *a, Exp *b) {
    Exp *a_high, *a_low;
    split_vector(a, &a_high, &a_low);
    delete a_high;

    Exp *b_high, *b_low;
    split_vector(b, &b_high, &b_low);
    delete b_high;

    return interleave2_2x32(a_low, b_low);
}

Exp *translate_InterleaveHI32x4(Exp *a, Exp *b) {
    Exp *a_high, *a_low;
    split_vector(a, &a_high, &a_low);
    delete a_low;

    Exp *b_high, *b_low;
    split_vector(b, &b_high, &b_low);
    delete b_low;

    return interleave2_2x32(a_high, b_high);
}

Exp *translate_InterleaveLO64x2(Exp *a, Exp *b) {
    Exp *a_high, *a_low;
    split_vector(a, &a_high, &a_low);
    delete a_high;

    Exp *b_high, *b_low;
    split_vector(b, &b_high, &b_low);
    delete b_high;

    return translate_64HLto128(a_low, b_low);
}

Exp *translate_InterleaveHI64x2(Exp *a, Exp *b) {
    Exp *a_high, *a_low;
    split_vector(a, &a_high, &a_low);
    delete a_low;

    Exp *b_high, *b_low;
    split_vector(b, &b_high, &b_low);
    delete b_low;

    return translate_64HLto128(a_high, b_high);
}

Exp *interleave2_4x16(Exp *a, Exp *b) {
    Exp *a3, *a2, *a1, *a0;
    split4x16(a, &a3, &a2, &a1, &a0);
    Exp *b3, *b2, *b1, *b0;
    split4x16(b, &b3, &b2, &b1, &b0);
    Exp *r_h = assemble4x16(a3, b3, a2, b2);
    Exp *r_l = assemble4x16(a1, b1, a0, b0);
    return translate_64HLto128(r_h, r_l);
}

Exp *translate_InterleaveLO16x8(Exp *a, Exp *b) {
    Exp *a_high, *a_low;
    split_vector(a, &a_high, &a_low);
    delete a_high;

    Exp *b_high, *b_low;
    split_vector(b, &b_high, &b_low);
    delete b_high;

    return interleave2_4x16(a_low, b_low);
}

Exp *translate_InterleaveHI16x8(Exp *a, Exp *b) {
    Exp *a_high, *a_low;
    split_vector(a, &a_high, &a_low);
    delete a_low;

    Exp *b_high, *b_low;
    split_vector(b, &b_high, &b_low);
    delete b_low;

    return interleave2_4x16(a_high, b_high);
}

Exp *interleave2_8x8(Exp *a, Exp *b) {
    Exp *a7, *a6, *a5, *a4, *a3, *a2, *a1, *a0;
    split8x8(a, &a7, &a6, &a5, &a4, &a3, &a2, &a1, &a0);
    Exp *b7, *b6, *b5, *b4, *b3, *b2, *b1, *b0;
    split8x8(b, &b7, &b6, &b5, &b4, &b3, &b2, &b1, &b0);
    Exp *r_h = assemble8x8(a7, b7, a6, b6, a5, b5, a4, b4);
    Exp *r_l = assemble8x8(a3, b3, a2, b2, a1, b1, a0, b0);
    return translate_64HLto128(r_h, r_l);
}

Exp *translate_InterleaveLO8x16(Exp *a, Exp *b) {
    Exp *a_high, *a_low;
    split_vector(a, &a_high, &a_low);
    delete a_high;

    Exp *b_high, *b_low;
    split_vector(b, &b_high, &b_low);
    delete b_high;

    return interleave2_8x8(a_low, b_low);
}

Exp *translate_InterleaveHI8x16(Exp *a, Exp *b) {
    Exp *a_high, *a_low;
    split_vector(a, &a_high, &a_low);
    delete a_low;

    Exp *b_high, *b_low;
    split_vector(b, &b_high, &b_low);
    delete b_low;

    return interleave2_8x8(a_high, b_high);
}

Exp *translate_par8x8_binop(binop_type_t op, Exp *a, Exp *b) {
    Exp *a7, *a6, *a5, *a4, *a3, *a2, *a1, *a0;
    split8x8(a, &a7, &a6, &a5, &a4, &a3, &a2, &a1, &a0);
    Exp *b7, *b6, *b5, *b4, *b3, *b2, *b1, *b0;
    split8x8(b, &b7, &b6, &b5, &b4, &b3, &b2, &b1, &b0);
    Exp *r7 = new BinOp(op, a7, b7);
    Exp *r6 = new BinOp(op, a6, b6);
    Exp *r5 = new BinOp(op, a5, b5);
    Exp *r4 = new BinOp(op, a4, b4);
    Exp *r3 = new BinOp(op, a3, b3);
    Exp *r2 = new BinOp(op, a2, b2);
    Exp *r1 = new BinOp(op, a1, b1);
    Exp *r0 = new BinOp(op, a0, b0);
    return assemble8x8(r7, r6, r5, r4, r3, r2, r1, r0);
}

Exp *translate_par16x8_binop(binop_type_t op, Exp *a, Exp *b) {
    Exp *a_high, *a_low;
    split_vector(a, &a_high, &a_low);
    Exp *b_high, *b_low;
    split_vector(b, &b_high, &b_low);
    Exp *r_h = translate_par8x8_binop(op, a_high, b_high);
    Exp *r_l = translate_par8x8_binop(op, a_low, b_low);
    return translate_64HLto128(r_h, r_l);
}

Exp *translate_minmax8x8(binop_type_t op, bool is_max, Exp *a64, Exp *b64) {
    Exp *a[8], *b[8];
    split8x8(a64, &a[7], &a[6], &a[5], &a[4], &a[3], &a[2], &a[1], &a[0]);
    split8x8(b64, &b[7], &b[6], &b[5], &b[4], &b[3], &b[2], &b[1], &b[0]);
    Exp *r[8];
    for (int i = 7; i >= 0; i--) {
	if (is_max) {
	    r[i] = _ex_ite(new BinOp(op, b[i], a[i]), ecl(a[i]), ecl(b[i]));
	} else {
	    r[i] = _ex_ite(new BinOp(op, a[i], b[i]), ecl(a[i]), ecl(b[i]));
	}
    }
    return assemble8x8(r[7], r[6], r[5], r[4], r[3], r[2], r[1], r[0]);
}

Exp *translate_minmax16x8(binop_type_t op, bool is_max, Exp *a, Exp *b) {
    Exp *a_high, *a_low;
    split_vector(a, &a_high, &a_low);
    Exp *b_high, *b_low;
    split_vector(b, &b_high, &b_low);
    Exp *r_h = translate_minmax8x8(op, is_max, a_high, b_high);
    Exp *r_l = translate_minmax8x8(op, is_max, a_low, b_low);
    return translate_64HLto128(r_h, r_l);
}

Exp *translate_par2x32_binop(binop_type_t op, Exp *a, Exp *b) {
    Exp *a1, *a0;
    split2x32(a, &a1, &a0);
    Exp *b1, *b0;
    split2x32(b, &b1, &b0);
    Exp *r1 = new BinOp(op, a1, b1);
    Exp *r0 = new BinOp(op, a0, b0);
    return translate_32HLto64(r1, r0);
}

Exp *translate_par4x32_binop(binop_type_t op, Exp *a, Exp *b) {
    Exp *a_high, *a_low;
    split_vector(a, &a_high, &a_low);
    Exp *b_high, *b_low;
    split_vector(b, &b_high, &b_low);
    Exp *r_h = translate_par2x32_binop(op, a_high, b_high);
    Exp *r_l = translate_par2x32_binop(op, a_low, b_low);
    return translate_64HLto128(r_h, r_l);
}

Exp *translate_par2x64_binop(binop_type_t op, Exp *a, Exp *b) {
    Exp *a_high, *a_low;
    split_vector(a, &a_high, &a_low);
    Exp *b_high, *b_low;
    split_vector(b, &b_high, &b_low);
    Exp *r_h = new BinOp(op, a_high, b_high);
    Exp *r_l = new BinOp(op, a_low, b_low);
    return translate_64HLto128(r_h, r_l);
}

const_val_t expand_lane_const(int bits) {
    const_val_t x = 0;
    if (bits & 1)
	x |= 0xff;
    if (bits & 2)
	x |= 0xff00;
    if (bits & 4)
	x |= 0xff0000;
    if (bits & 8)
	x |= 0xff000000ULL;
    if (bits & 16)
	x |= 0xff00000000ULL;
    if (bits & 32)
	x |= 0xff0000000000ULL;
    if (bits & 64)
	x |= 0xff000000000000ULL;
    if (bits & 128)
	x |= 0xff00000000000000ULL;
    return x;
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
        // These are used in SIMD instructions, but VEX uses a slightly
        // weird compressed representation.
	{
	    const_val_t high = expand_lane_const(co->Ico.V128 >> 8);
	    const_val_t low = expand_lane_const(co->Ico.V128 & 0xff);
	    return new Vector(new Constant(REG_64, high),
			      new Constant(REG_64, low));
	}
      default:
            panic("Unrecognized constant type");
    }

    Constant *result = new Constant(width, value);

    return result;
}

Exp *distribute_unop128(unop_type_t op, Exp *arg_v) {
    Exp *arg_high, *arg_low;
    split_vector(arg_v, &arg_high, &arg_low);

    return new Vector(new UnOp(op, arg_high), new UnOp(op, arg_low));
}

Exp *translate_simple_unop( IRExpr *expr, IRSB *irbb, vector<Stmt *> *irout )
{

    Exp *arg = translate_expr( expr->Iex.Unop.arg, irbb, irout );

    switch ( expr->Iex.Unop.op )
    {

        case Iop_Not8:
	case Iop_Not16:
	case Iop_Not32:
	case Iop_Not64:
	    return new UnOp( NOT, arg );
#if VEX_VERSION < 1770
        case Iop_Neg8:
	case Iop_Neg16:
	case Iop_Neg32:
	case Iop_Neg64:    return new UnOp( NEG, arg ); 
#endif
#if VEX_VERSION >= 1949
        case Iop_NegF32:
#endif
        case Iop_NegF64:   return new FUnOp(FNEG, ROUND_NEAREST, arg);

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

        case Iop_32UtoV128:
	    return new Vector(mk_u64(0),
			      new Cast(arg, REG_64, CAST_UNSIGNED));
        case Iop_64UtoV128:
	    return new Vector(mk_u64(0), arg);

        case Iop_V128to32: {
	    Exp *arg_high, *arg_low;
	    split_vector(arg, &arg_high, &arg_low);
	    Exp::destroy(arg_high);
	    return new Cast(arg_low, REG_32, CAST_LOW);
	}

        case Iop_V128to64:
        case Iop_128to64: {
	    Exp *arg_high, *arg_low;
	    split_vector(arg, &arg_high, &arg_low);
	    Exp::destroy(arg_high);
	    return arg_low;
	}

        case Iop_V128HIto64:
        case Iop_128HIto64: {
	    Exp *arg_high, *arg_low;
	    split_vector(arg, &arg_high, &arg_low);
	    Exp::destroy(arg_low);
	    return arg_high;
	}

	// These next few are the rare FP conversions that never have
	// to round, so they take no rounding mode in VEX. The others
	// are VEX binops to take a rounding mode.
        case Iop_F32toF64:
           return new FCast(arg, REG_64, CAST_FWIDEN, ROUND_NEAREST);

#if VEX_VERSION < 1949
        case Iop_I32toF64:
#else
        case Iop_I32StoF64:
#endif
	    return new FCast(arg, REG_64, CAST_SFLOAT, ROUND_NEAREST);
#if VEX_VERSION >= 1949
        case Iop_I32UtoF64:
	    return new FCast(arg, REG_64, CAST_UFLOAT, ROUND_NEAREST);
#endif

        case Iop_ReinterpI32asF32:
        case Iop_ReinterpF32asI32:
        case Iop_ReinterpI64asF64:
        case Iop_ReinterpF64asI64:
	    return arg; // We don't make any distinction here

#if VEX_VERSION >= 2559
        case Iop_GetMSBs8x8:
	    return translate_GetMSBs8x8(arg);
#endif
#if VEX_VERSION >= 2663
        case Iop_GetMSBs8x16:
	    return translate_GetMSBs8x16(arg);
#endif

        case Iop_NotV128:
	    return distribute_unop128(NOT, arg);

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
        case Iop_Clz64:     return translate_Clz64( expr, irbb, irout );
        case Iop_Ctz64:     return translate_Ctz64( expr, irbb, irout );

        case Iop_AbsF64:
            return new Unknown("Floating point op");

        default:    
	    return new Unknown("Unrecognized unary op");
    }

    return NULL;
}

Exp *distribute_binop128(binop_type_t op, Exp *left_v, Exp *right_v) {
    Exp *left_high, *left_low;
    split_vector(left_v, &left_high, &left_low);

    Exp *right_high, *right_low;
    split_vector(right_v, &right_high, &right_low);

    return new Vector(new BinOp(op, left_high, right_high),
		      new BinOp(op, left_low, right_low));
}

Exp *translate_vs2x64_shift(binop_type_t op, Exp *left_v, Exp *right) {
    Exp *left_high, *left_low;
    split_vector(left_v, &left_high, &left_low);

    return new Vector(new BinOp(op, left_high, right),
		      new BinOp(op, left_low, ecl(right)));
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
	case Iop_Or64:
	    return new BinOp(BITOR, arg1, arg2);
        case Iop_OrV128:
	    return distribute_binop128(BITOR, arg1, arg2);
        case Iop_And8:
	case Iop_And16:
	case Iop_And32:
	case Iop_And64:
	    return new BinOp(BITAND, arg1, arg2);
        case Iop_AndV128:
	    return distribute_binop128(BITAND, arg1, arg2);
        case Iop_Xor8:
	case Iop_Xor16:
	case Iop_Xor32:
	case Iop_Xor64:
	    return new BinOp(XOR, arg1, arg2);
        case Iop_XorV128:
	    return distribute_binop128(XOR, arg1, arg2);
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
        case Iop_64HLto128:         return translate_64HLto128(arg1, arg2);
        case Iop_64HLtoV128:        return translate_64HLto128(arg1, arg2);
        case Iop_MullU8:            return translate_MullU8(arg1, arg2);
        case Iop_MullS8:            return translate_MullS8(arg1, arg2);
        case Iop_MullU16:           return translate_MullU16(arg1, arg2);
        case Iop_MullS16:           return translate_MullS16(arg1, arg2);
        case Iop_MullU32:           return translate_MullU32(arg1, arg2);
        case Iop_MullS32:           return translate_MullS32(arg1, arg2);
        case Iop_DivModU64to32:     return translate_DivModU64to32(arg1, arg2);
        case Iop_DivModS64to32:     return translate_DivModS64to32(arg1, arg2);
        case Iop_DivModU128to64:    return translate_DivModU128to64(arg1,arg2);
        case Iop_DivModS128to64:    return translate_DivModS128to64(arg1,arg2);

	case Iop_DivU32: return new BinOp(DIVIDE, arg1, arg2);
	case Iop_DivS32: return new BinOp(SDIVIDE, arg1, arg2);
	case Iop_DivU64: return new BinOp(DIVIDE, arg1, arg2);
	case Iop_DivS64: return new BinOp(SDIVIDE, arg1, arg2);

        case Iop_Add32F0x4:
	    return translate_low32fp_128_binop(FPLUS, arg1, arg2);
        case Iop_Sub32F0x4:
	    return translate_low32fp_128_binop(FMINUS, arg1, arg2);
        case Iop_Mul32F0x4:
	    return translate_low32fp_128_binop(FTIMES, arg1, arg2);
        case Iop_Div32F0x4:
	    return translate_low32fp_128_binop(FDIVIDE, arg1, arg2);

        case Iop_Add32Fx4:
	    return translate_par32fp_128_binop(FPLUS, arg1, arg2);
        case Iop_Sub32Fx4:
	    return translate_par32fp_128_binop(FMINUS, arg1, arg2);
        case Iop_Mul32Fx4:
	    return translate_par32fp_128_binop(FTIMES, arg1, arg2);
        case Iop_Div32Fx4:
	    return translate_par32fp_128_binop(FDIVIDE, arg1, arg2);

        case Iop_Add64F0x2:
	    return translate_low64fp_128_binop(FPLUS, arg1, arg2);
        case Iop_Sub64F0x2:
	    return translate_low64fp_128_binop(FMINUS, arg1, arg2);
        case Iop_Mul64F0x2:
	    return translate_low64fp_128_binop(FTIMES, arg1, arg2);
        case Iop_Div64F0x2:
	    return translate_low64fp_128_binop(FDIVIDE, arg1, arg2);

        case Iop_Add64Fx2:
	    return translate_par64fp_128_binop(FPLUS, arg1, arg2);
        case Iop_Sub64Fx2:
	    return translate_par64fp_128_binop(FMINUS, arg1, arg2);
        case Iop_Mul64Fx2:
	    return translate_par64fp_128_binop(FTIMES, arg1, arg2);
        case Iop_Div64Fx2:
	    return translate_par64fp_128_binop(FDIVIDE, arg1, arg2);

#if VEX_VERSION >= 2105
        case Iop_CmpF32:
	    return translate_CmpF(arg1, arg2, REG_32);
#endif
        case Iop_CmpF64:
           return translate_CmpF(arg1, arg2, REG_64);

	// arg1 is a rounding mode, currently unsupported. Pretend it's
	// always ROUND_NEAREST.
	// Float to int:
#if VEX_VERSION < 1949
        case Iop_F64toI16:
#else
        case Iop_F64toI16S:
#endif
	    return new FCast(arg2, REG_16, CAST_SFIX, ROUND_NEAREST);
#if VEX_VERSION < 1949
        case Iop_F64toI32:
#else
        case Iop_F64toI32S:
#endif
            return new FCast(arg2, REG_32, CAST_SFIX, ROUND_NEAREST);
#if VEX_VERSION >= 2105
        case Iop_F32toI32S:
            return new FCast(arg2, REG_32, CAST_SFIX, ROUND_NEAREST);
        case Iop_F32toI64S:
#endif
#if VEX_VERSION < 1949
        case Iop_F64toI64:
#else
        case Iop_F64toI64S:
#endif
            return new FCast(arg2, REG_64, CAST_SFIX, ROUND_NEAREST);
#if VEX_VERSION >= 2496
        case Iop_F32toI32U:
#endif
#if VEX_VERSION >= 1949
        case Iop_F64toI32U:
            return new FCast(arg2, REG_32, CAST_UFIX, ROUND_NEAREST);
#endif
#if VEX_VERSION >= 2496
        case Iop_F32toI64U:
#endif
#if VEX_VERSION >= 2184
        case Iop_F64toI64U:
            return new FCast(arg2, REG_64, CAST_UFIX, ROUND_NEAREST);
#endif

        // Int to float:
        // Iop_I32StoF64: see unops above
#if VEX_VERSION < 1949
        case Iop_I64toF64:
#else
        case Iop_I64StoF64:
#endif
            return new FCast(arg2, REG_64, CAST_SFLOAT, ROUND_NEAREST);
#if VEX_VERSION >= 2127
        case Iop_I64UtoF64:
            return new FCast(arg2, REG_64, CAST_UFLOAT, ROUND_NEAREST);
#endif
#if VEX_VERSION >= 2496
        case Iop_I32UtoF32:
#endif
#if VEX_VERSION >= 2127
        case Iop_I64UtoF32:
            return new FCast(arg2, REG_32, CAST_UFLOAT, ROUND_NEAREST);
#endif
#if VEX_VERSION >= 2105
        case Iop_I32StoF32:
        case Iop_I64StoF32:
            return new FCast(arg2, REG_32, CAST_SFLOAT, ROUND_NEAREST);
#endif
	// Iop_I32UtoF64: see unops above
	// Float narrowing (for widening see unops)
        case Iop_F64toF32:
           return new FCast(arg2, REG_32, CAST_FNARROW, ROUND_NEAREST);

        case Iop_InterleaveLO64x2:
	    return translate_InterleaveLO64x2(arg1, arg2);
        case Iop_InterleaveLO32x4:
	    return translate_InterleaveLO32x4(arg1, arg2);
	case Iop_InterleaveLO16x8:
	    return translate_InterleaveLO16x8(arg1, arg2);
	case Iop_InterleaveLO8x16:
	    return translate_InterleaveLO8x16(arg1, arg2);

        case Iop_InterleaveHI64x2:
	    return translate_InterleaveHI64x2(arg1, arg2);
        case Iop_InterleaveHI32x4:
	    return translate_InterleaveHI32x4(arg1, arg2);
	case Iop_InterleaveHI16x8:
	    return translate_InterleaveHI16x8(arg1, arg2);
	case Iop_InterleaveHI8x16:
	    return translate_InterleaveHI8x16(arg1, arg2);

#if VEX_VERSION >= 636
        case Iop_CmpEQ8x16:
	    return translate_CmpEQ8x16(arg1, arg2);

        case Iop_CmpEQ32x4:
	    return translate_CmpEQ32x4(arg1, arg2);
#endif
#if VEX_VERSION >= 1984
        case Iop_CmpGT8Sx16:
	    return translate_CmpGT8Sx16(arg1, arg2);
        case Iop_CmpGT32Sx4:
	    return translate_CmpGT32Sx4(arg1, arg2);
#endif

        case Iop_Add8x8:
	    return translate_par8x8_binop(PLUS, arg1, arg2);
        case Iop_Add8x16:
	    return translate_par16x8_binop(PLUS, arg1, arg2);

        case Iop_Sub8x8:
	    return translate_par8x8_binop(MINUS, arg1, arg2);
        case Iop_Sub8x16:
	    return translate_par16x8_binop(MINUS, arg1, arg2);

        case Iop_Mul8x8:
	    return translate_par8x8_binop(TIMES, arg1, arg2);
        case Iop_Mul8x16:
	    return translate_par16x8_binop(TIMES, arg1, arg2);

        case Iop_Min8Ux8:
	    return translate_minmax8x8(LT, false, arg1, arg2);
        case Iop_Min8Ux16:
	    return translate_minmax16x8(LT, false, arg1, arg2);

#if VEX_VERSION >= 2016
        case Iop_Min8Sx8:
	    return translate_minmax8x8(SLT, false, arg1, arg2);
#endif
        case Iop_Min8Sx16:
	    return translate_minmax16x8(SLT, false, arg1, arg2);

        case Iop_Max8Ux8:
	    return translate_minmax8x8(LT, true, arg1, arg2);
        case Iop_Max8Ux16:
	    return translate_minmax16x8(LT, true, arg1, arg2);

#if VEX_VERSION >= 2016
        case Iop_Max8Sx8:
	    return translate_minmax8x8(SLT, true, arg1, arg2);
#endif
        case Iop_Max8Sx16:
	    return translate_minmax16x8(SLT, true, arg1, arg2);

        case Iop_Add32x2:
	    return translate_par2x32_binop(PLUS, arg1, arg2);
        case Iop_Add32x4:
	    return translate_par4x32_binop(PLUS, arg1, arg2);

        case Iop_Sub32x2:
	    return translate_par2x32_binop(MINUS, arg1, arg2);
        case Iop_Sub32x4:
	    return translate_par4x32_binop(MINUS, arg1, arg2);

        case Iop_Mul32x2:
	    return translate_par2x32_binop(TIMES, arg1, arg2);
        case Iop_Mul32x4:
	    return translate_par4x32_binop(TIMES, arg1, arg2);

        case Iop_Add64x2:
	    return translate_par2x64_binop(PLUS, arg1, arg2);

        case Iop_Sub64x2:
	    return translate_par2x64_binop(MINUS, arg1, arg2);

        case Iop_ShlN64x2:
	    return translate_vs2x64_shift(LSHIFT, arg1, arg2);
        case Iop_ShrN64x2:
	    return translate_vs2x64_shift(RSHIFT, arg1, arg2);
#if VEX_VERSION >= 2016
        case Iop_SarN64x2:
	    return translate_vs2x64_shift(ARSHIFT, arg1, arg2);
#endif

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
        case Iop_MullU64: {
	    Exp *arg1 = translate_expr(expr->Iex.Binop.arg1, irbb, irout);
	    Exp *arg2 = translate_expr(expr->Iex.Binop.arg2, irbb, irout);
	    return translate_MullU64(arg1, arg2, irout);
	}

        case Iop_MullS64: {
	    Exp *arg1 = translate_expr(expr->Iex.Binop.arg1, irbb, irout);
	    Exp *arg2 = translate_expr(expr->Iex.Binop.arg2, irbb, irout);
	    return translate_MullS64(arg1, arg2, irout);
	}

        // arg1 in this case, specifies rounding mode, and so is ignored
        case Iop_RoundF64toInt:
        case Iop_2xm1F64:
            return new Unknown("Floating point binop");

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

#if VEX_VERSION >= 1949
        case Iop_AddF32:
#endif
        case Iop_AddF64:
	  return new FBinOp(FPLUS, ROUND_NEAREST, arg2, arg3);
#if VEX_VERSION >= 1949
        case Iop_SubF32:
#endif
        case Iop_SubF64:
	  return new FBinOp(FMINUS, ROUND_NEAREST, arg2, arg3);
#if VEX_VERSION >= 1949
        case Iop_MulF32:
#endif
        case Iop_MulF64:
	  return new FBinOp(FTIMES, ROUND_NEAREST, arg2, arg3);
#if VEX_VERSION >= 1949
        case Iop_DivF32:
#endif
        case Iop_DivF64:
	  return new FBinOp(FDIVIDE, ROUND_NEAREST, arg2, arg3);

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

    IRType type = expr->Iex.Load.ty;

    if (type == Ity_I128 || type == Ity_V128) {
	// Split into two adjacent 64-bit loads
	Exp *addr_l = translate_expr(expr->Iex.Load.addr, irbb, irout);
	Exp *addr_h = _ex_add(ecl(addr_l), ex_addr_const(8));
	Mem *mem_l = new Mem(addr_l, REG_64);
	Mem *mem_h = new Mem(addr_h, REG_64);
	return new Vector(mem_h, mem_l);
    } else {
	reg_t rtype = IRType_to_reg_type(expr->Iex.Load.ty);

	Exp *addr = translate_expr(expr->Iex.Load.addr, irbb, irout);
	return new Mem(addr, rtype);
    }
}

Exp *translate_tmp( IRTemp temp, IRSB *irbb, vector<Stmt *> *irout )
{
    assert(temp != IRTemp_INVALID);
    assert(irbb);
    assert(irout);

    IRType type = typeOfIRTemp(irbb->tyenv, temp);

    if (type == Ity_I128 || type == Ity_V128) {
	Temp *t_high, *t_low;
	mk_temps128(temp, &t_high, &t_low);
	return new Vector(t_high, t_low);
    }

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
            result = translate_geti(expr, irbb, irout);
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

    if (type == Ity_I128 || type == Ity_V128) {
	// Split into two 64-bit temps
	Exp *rhs_high, *rhs_low;
	split_vector(rhs_e, &rhs_high, &rhs_low);
	Temp *t_high, *t_low;
	mk_temps128(tmp, &t_high, &t_low);
	irout->push_back(new Move(t_high, rhs_high));
	return new Move(t_low, rhs_low);
    }

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
    IRType itype;

    dest = translate_expr(stmt->Ist.Store.addr, irbb, irout);
    itype = typeOfIRExpr(irbb->tyenv, stmt->Ist.Store.data);
    data = translate_expr(stmt->Ist.Store.data, irbb, irout);

    if (itype == Ity_I128 || itype == Ity_V128) {
	// Split into two 64-bit stores
	Exp *data_high, *data_low;
	split_vector(data, &data_high, &data_low);
	Exp *dest_h = _ex_add(ecl(dest), ex_addr_const(8));
	irout->push_back(new Move(new Mem(dest, REG_64), data_low));
	return new Move(new Mem(dest_h, REG_64), data_high);
    } else {
	reg_t rtype = IRType_to_reg_type(itype);
	return new Move(new Mem(dest, rtype), data);
    }
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

    Addr64 dest_addr;

    if (stmt->Ist.Exit.dst->tag == Ico_U32) {
	// 32 bits, expected on 32-bit archs
	dest_addr = stmt->Ist.Exit.dst->Ico.U32;
    } else if (stmt->Ist.Exit.dst->tag == Ico_U64) {
	// 64 bits, expected on x64
	dest_addr = stmt->Ist.Exit.dst->Ico.U64;
    } else {
	panic("Unexpected destination address constant type");
    }

    Exp *cond = translate_expr( stmt->Ist.Exit.guard, irbb, irout );

    if (stmt->Ist.Exit.jk == Ijk_MapFail) {
      // MapFail is only used for bailing out, and the target is always
      // the beginning of the same instruction, right? At least that seems
      // to currently be true in VEX r1774.
      
      return new Assert(new UnOp(NOT,cond));
    }
    
    Name *dest = mk_dest_name( dest_addr );
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

#if VEX_VERSION >= 2642
Stmt *translate_loadg( IRStmt *stmt, IRSB *irbb, vector<Stmt *> *irout ) {
    assert(stmt);
    assert(irbb);
    assert(irout);

    assert(stmt->Ist.LoadG.details->end == Iend_LE); // Assume little-endian
    Exp *addr = translate_expr(stmt->Ist.LoadG.details->addr, irbb, irout);
    Exp *alt = translate_expr(stmt->Ist.LoadG.details->alt, irbb, irout);
    Exp *guard = translate_expr(stmt->Ist.LoadG.details->guard, irbb, irout);
    IRLoadGOp op = stmt->Ist.LoadG.details->cvt;

    reg_t load_type;
    switch (op) {
#if VEX_VERSION >= 3169
    case ILGop_IdentV128:
	panic("Unsupported V128 type in LoadG");
	break;
#endif
#if VEX_VERSION >= 3047
    case ILGop_Ident64:
	load_type = REG_64;
	break;
#endif
    case ILGop_Ident32:
	load_type = REG_32;
	break;
    case ILGop_16Uto32:
    case ILGop_16Sto32:
	load_type = REG_16;
	break;
    case ILGop_8Uto32:
    case ILGop_8Sto32:
	load_type = REG_8;
	break;
    default:
	panic("Unexpected conversion type in LoadG");
    }
    Mem *mem = new Mem(addr, load_type);

    Exp *cvt_val;
    switch (op) {
#if VEX_VERSION >= 3169
    case ILGop_IdentV128:
#endif
#if VEX_VERSION >= 3047
    case ILGop_Ident64:
#endif
    case ILGop_Ident32:
	cvt_val = mem;
	break;
    case ILGop_16Uto32:
    case ILGop_8Uto32:
	cvt_val = _ex_u_cast(mem, REG_32);
	break;
    case ILGop_16Sto32:
    case ILGop_8Sto32:
	cvt_val = _ex_s_cast(mem, REG_32);
	break;
    default:
	panic("Unexpected conversion type (2) in LoadG");
    }

    Exp *choice = _ex_ite(guard, cvt_val, alt);
    IRTemp dst = stmt->Ist.LoadG.details->dst;
    IRType dst_type = typeOfIRTemp(irbb->tyenv, dst);
    return new Move( mk_temp(dst, dst_type), choice );
}

Stmt *translate_storeg( IRStmt *stmt, IRSB *irbb, vector<Stmt *> *irout ) {
    assert(stmt);
    assert(irbb);
    assert(irout);

    assert(stmt->Ist.StoreG.details->end == Iend_LE); // Assume little-endian
    Exp *addr = translate_expr(stmt->Ist.StoreG.details->addr, irbb, irout);
    Exp *data = translate_expr(stmt->Ist.StoreG.details->data, irbb, irout);
    Exp *guard = translate_expr(stmt->Ist.StoreG.details->guard, irbb, irout);
    IRType itype = typeOfIRExpr(irbb->tyenv, stmt->Ist.StoreG.details->data);
    reg_t rtype = IRType_to_reg_type(itype);

    Mem *mem = new Mem(addr, rtype);
    Exp *choice = _ex_ite(guard, data, ecl(mem));
    return new Move(mem, choice);
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
            result = translate_puti(stmt, irbb, irout);
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
#if VEX_VERSION >= 2642
        case Ist_LoadG:
	    result = translate_loadg(stmt, irbb, irout);
	    break;
        case Ist_StoreG:
	    result = translate_storeg(stmt, irbb, irout);
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
      irout->push_back( new Special("int 0x82") );
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
#if VEX_VERSION >= 1320 && VEX_VERSION < 2852
    case Ijk_TInval:
      irout->push_back( new Special("TInval") );
      result = new Jmp(dest);
      break; 
#endif
#if VEX_VERSION >= 2852
    case Ijk_InvalICache:
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

      if (ty == Ity_I128 || ty == Ity_V128) {
	  // "Vector" type: print as a pair of smaller variables
	  string name = vex_temp_name(i, REG_64);
	  irout->push_back(new VarDecl(name + "h", REG_64));
	  irout->push_back(new VarDecl(name + "l", REG_64));
      } else {
	  reg_t typ = IRType_to_reg_type(ty);
	  string name = vex_temp_name(i, typ);
	  irout->push_back(new VarDecl(name, typ));
      }
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
  } else if (i >= 2 &&
	     ir->at(i-2)->stmt_type == MOVE &&
	     ir->at(i-1)->stmt_type == MOVE &&
	     ir->at(i)->stmt_type == MOVE) {
      // Similar to the previous case, but with two intervening temps
      Move *s0 = (Move *)ir->at(i-2);
      Move *s1 = (Move *)ir->at(i-1);
      Move *s2 = (Move *)ir->at(i);
      if (s0->lhs->exp_type != TEMP ||
	  s0->rhs->exp_type != ITE ||
	  s1->lhs->exp_type != TEMP ||
	  s1->rhs->exp_type != TEMP ||
	  s2->lhs->exp_type != TEMP ||
	  s2->rhs->exp_type != TEMP)
	  return -1;
      Temp *temp0l = (Temp *)s0->lhs;
      Temp *temp1l = (Temp *)s1->lhs;
      Temp *temp1r = (Temp *)s1->rhs;
      Temp *temp2l = (Temp *)s2->lhs;
      Temp *temp2r = (Temp *)s2->rhs;
      if (temp0l->name != temp1r->name ||
	  temp1l->name != temp2r->name)
	  return -1;
      Ite *ite = (Ite *)s0->rhs;
      if (cond)
	  *cond = ite->cond;
      if (exp_t)
	  *exp_t = ite->true_e;
      if (exp_f)
	  *exp_f = ite->false_e;
      if (res)
	  *res = temp2l;
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

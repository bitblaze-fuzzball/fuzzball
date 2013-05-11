#include "ir_printer.h"


void print_globals(){

  vector<VarDecl *> globals = get_reg_decls();
  for(vector<VarDecl *>::const_iterator it = globals.begin();
	it != globals.end(); it++){
	VarDecl *s = *it;
	cout << s->tostring() << endl;
  }

  cout << "var mem:reg8_t[reg32_t];" << endl;

  vector<Stmt *> helpers = gen_eflags_helpers();
  for(vector<Stmt *>::const_iterator it = helpers.begin();
      it != helpers.end(); it++){
    Stmt *s = *it;
    cout << s->tostring() << endl;
  }
}


void print_vine_ir(asm_program_t *prog, vector<vine_block_t *> vblocks )
{
    unsigned int i, j;
    
    for ( i = 0; i < vblocks.size(); i++ )
    {
        vine_block_t *block = vblocks.at(i);
        assert(block);
        
        vector<Stmt *> *inner = block->vine_ir;

	//        cout << "Vine Block " << i << endl;
        cout << "  {" << endl;
// 	declvis vis;
// 	vis.compute(inner);
// 	print_decls(vis.decls);
	//        cout << "    ";
	ostringstream os;
	ostream_insn(prog, block->inst, os);
	cout << "   // " << os.str() << endl;


	vector<VarDecl *> globals = get_reg_decls();
	map<string,reg_t> context;
        for(vector<VarDecl *>::const_iterator gi = globals.begin();
	    gi != globals.end(); gi++){
           VarDecl *vd = *gi;
           context.insert(pair<string, reg_t>(vd->name, vd->typ));
        }

        for ( j = 0; j < inner->size(); j++ )
        {
#ifdef TYPECHECKING
	  try {
	  if (typecheck_stmt(&context,  inner->at(j)) < 0) {
	    cout <<"Type error found at:" <<endl;
	  }
	  } catch (TypeError &e) {
	    cout <<"Type Error: " << e.what() <<endl;
	    cout <<"Found at:" <<endl;
	  }
#endif
	  Stmt *s = inner->at(j);
	  cout << "     " << s->tostring();
// 	  if(s->stmt_type == LABEL)
// 	    cout << endl;
// 	  else
// 	    cout << ";" << endl;
	  cout << endl;

        }
        cout << "  }" << endl;
    }
    
}

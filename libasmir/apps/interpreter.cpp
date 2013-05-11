#include <stmt.h>
#include <exp.h>

// a = a + b
// b = a + d
// a = a + b
// b = a


// a = (a + b)
// b = (a + b) + d
// a = (a + (a + b + d) + b


class SymEvaluator : public IRChangeVisitor {  
public:
  virtual void visitTemp(Temp *t)
  {
    if(state.find(t->name) == state.end()){
      ret = t;
      return;
    }
    Exp *o = state[t->name];
    ret = o->clone();
  }
  virtual void visitJmp(Jmp *)
  {
  }

  virtual void visitCJmp(CJmp *)
  {
  }

  virtual void visitLabel(Label *)
  {
  }

  virtual void visitMove(Move *m)
  {
    m->lhs->accept(this);
    m->rhs->accept(this);
    state[m->lhs->tostring()] = m->rhs;
  }

  virtual void visitComment(Comment *)
  {
  }
  virtual void visitExpStmt(ExpStmt *)
  {
  }

  map<string, Exp *> state;
};

Exp *execute(SymEvaluator *evaluator)
{
  Stmt *s = state.q.top();
  input_state.q.pop();

  s->accept(evaluator);

  switch(s->stmt_type){
  case JMP:
    break;
  case CJMP:
    break;
  case SPECIAL:
    break;
  case MOVE:  
    eval_move((Move *) s, input_state);
    break;
  case COMMENT:  
    break;
  case LABEL:
    break;
  case EXPSTMT:
    break;
  }
}

void evaluate(const vector<Stmt *> &stmts, state_t *input_state)
{
  input_state->sstack.push_back(stmts.riterator.begin(),
				stmts.riterator.end());
}

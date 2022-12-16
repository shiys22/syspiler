/*****************************************************
 *  Implementation of the third translation pass.
 *
 *  In the third pass, we will:
 *	translate all the statements and expressions
 *
 *  Keltin Leung 
 */

#include "translation.hpp"
#include "asm/offset_counter.hpp"
#include "ast/ast.hpp"
#include "compiler.hpp"
#include "config.hpp"
#include "scope/scope.hpp"
#include "symb/symbol.hpp"
#include "tac/tac.hpp"
#include "tac/trans_helper.hpp"
#include "type/type.hpp"

using namespace mind;
using namespace mind::symb;
using namespace mind::tac;
using namespace mind::type;
using namespace mind::assembly;

/* Constructor.
 *
 * PARAMETERS:
 *   helper - the translation helper
 */
Translation::Translation(tac::TransHelper *helper) {
	mind_assert(NULL != helper);

	tr = helper;
}
	
/* Translating an ast::Program node.
 */
void Translation::visit(ast::Program *p) {
	for (auto it = p->func_and_globals->begin();
		 it != p->func_and_globals->end(); ++it)
		(*it)->accept(this);
}

// three sugars for parameter offset management
#define RESET_OFFSET() tr->getOffsetCounter()->reset(OffsetCounter::PARAMETER)
#define NEXT_OFFSET(x) tr->getOffsetCounter()->next(OffsetCounter::PARAMETER, x)

/* Translating an ast::FuncDefn node.
 *
 * NOTE:
 *   call tr->startFunc() before translating the statements and
 *   call tr->endFunc() after all the statements have been translated
 */
void Translation::visit(ast::FuncDefn *f) {
	Function *fun = f->ATTR(sym);

	// attaching function entry label
	fun->attachEntryLabel(tr->getNewEntryLabel(fun));
	if (f -> forward_decl) return ;

	// arguments
	int order = 0;
	for (auto it = f->formals->begin(); it != f->formals->end(); ++it) {
		auto v = (*it)->ATTR(sym);
		v->setOrder(order++);
		v->attachTemp(tr->getNewTempI4());
	}

	fun->offset = fun->getOrder() * POINTER_SIZE;

	RESET_OFFSET();

	tr->startFunc(fun);

	// You may process params here, i.e use reg or stack to pass parameters
	for (auto it = f->formals->begin(); it != f->formals->end(); ++it) {
		auto v = (*it)->ATTR(sym);
		tr -> genGetParam(v -> getTemp(), v -> getOrder());
	}


	// translates statement by statement
	for (auto it = f->stmts->begin(); it != f->stmts->end(); ++it)
		(*it)->accept(this);

	tr->genReturn(tr->genLoadImm4(0)); // Return 0 by default

	tr->endFunc();
}

void Translation::visit(ast::Call *fcal) {
	int count = -1, tmp_count = 0;
	for (auto it = fcal -> exprs -> begin(); it != fcal -> exprs -> end(); ++it)
		(*it) -> accept(this), count++;
	for (auto it = fcal -> exprs -> begin(); it != fcal -> exprs -> end() && tmp_count < 8; ++it, ++tmp_count)
		tr -> genParam((*it) -> ATTR(val), tmp_count);
	for (auto it = fcal -> exprs -> rbegin(); it != fcal -> exprs -> rend() && count >= 8; ++it, --count)
		tr -> genParam((*it) -> ATTR(val), count);
	fcal -> ATTR(val) = tr -> genCall(fcal -> ATTR(sym) -> getEntryLabel());
}
/* Translating an ast::VarDecl node.
 */
void Translation::visit(ast::VarDecl *decl) {
	Variable *v = decl -> ATTR(sym);
	v->attachTemp(tr->getNewTempI4(decl -> is_global, decl -> name));
	if (decl -> init != NULL)
	{
		decl -> init -> accept(this);
		tr->genAssign(v->getTemp(), decl -> init -> ATTR(val));
	}
}

/* Translating an ast::AssignStmt node.
 *
 * NOTE:
 *   different kinds of Lvalue require different translation
 */
void Translation::visit(ast::AssignExpr *e) {
	e->left->accept(this);
	e->e->accept(this);
	tr->genAssign(dynamic_cast<ast::VarRef*>(e->left)->ATTR(sym)->getTemp(), e->e->ATTR(val));
	e -> ATTR(val) = e -> e -> ATTR(val);
}
/* Translating an ast::ExprStmt node.
 */
void Translation::visit(ast::ExprStmt *s) { s->e->accept(this); }

/* Translating an ast::IfStmt node.
 *
 * NOTE:decl -> initrch is empty
 */
void Translation::visit(ast::IfStmt *s) {
	Label L1 = tr->getNewLabel(); // entry of the false branch
	Label L2 = tr->getNewLabel(); // exit
	s->condition->accept(this);
	tr->genJumpOnZero(L1, s->condition->ATTR(val));

	s->true_brch->accept(this);
	tr->genJump(L2); // done

	tr->genMarkLabel(L1);
	s->false_brch->accept(this);

	tr->genMarkLabel(L2);
}
/* Translating an ast::WhileStmt node.
 */
void Translation::visit(ast::WhileStmt *s) {
	Label L1 = tr->getNewLabel();
	Label L2 = tr->getNewLabel();

	Label old_break = current_break_label;
	Label old_cont = current_continue_label;
	current_break_label = L2;
	current_continue_label = L1;

	tr->genMarkLabel(L1);
	s->condition->accept(this);
	tr->genJumpOnZero(L2, s->condition->ATTR(val));

	s->loop_body->accept(this);
	tr->genJump(L1);

	tr->genMarkLabel(L2);

	current_break_label = old_break;
	current_continue_label = old_cont;
}
void Translation::visit(ast::DoWhileStmt *s) {
	Label L1 = tr->getNewLabel();
	Label L2 = tr->getNewLabel();
	Label L3 = tr->getNewLabel();

	Label old_break = current_break_label;
	Label old_cont = current_continue_label;
	current_break_label = L2;
	current_continue_label = L3;

	tr->genMarkLabel(L1);
	s->loop_body->accept(this);
	tr->genMarkLabel(L3);
	s->condition->accept(this);
	tr->genJumpOnZero(L2, s->condition->ATTR(val));
	tr->genJump(L1);

	tr->genMarkLabel(L2);

	current_break_label = old_break;
	current_continue_label = old_cont;
}
void Translation::visit(ast::ForExprStmt *s) {
	Label L1 = tr->getNewLabel();
	Label L2 = tr->getNewLabel();
	Label L3 = tr->getNewLabel();

	Label old_break = current_break_label;
	Label old_cont = current_continue_label;
	current_break_label = L2;
	current_continue_label = L3;

	s->start_expr->accept(this);
	tr->genMarkLabel(L1);
	s->condition->accept(this);
	tr->genJumpOnZero(L2, s->condition->ATTR(val));

	s->loop_body->accept(this);
	tr->genMarkLabel(L3);
	s->repeat->accept(this);
	tr->genJump(L1);

	tr->genMarkLabel(L2);

	current_break_label = old_break;
	current_continue_label = old_cont;
}
void Translation::visit(ast::ForDeclStmt *s) {
	Label L1 = tr->getNewLabel();
	Label L2 = tr->getNewLabel();
	Label L3 = tr->getNewLabel();

	Label old_break = current_break_label;
	Label old_cont = current_continue_label;
	current_break_label = L2;
	current_continue_label = L3;

	s->start_decl->accept(this);
	tr->genMarkLabel(L1);
	s->condition->accept(this);
	tr->genJumpOnZero(L2, s->condition->ATTR(val));

	s->loop_body->accept(this);
	tr->genMarkLabel(L3);
	s->repeat->accept(this);
	tr->genJump(L1);

	tr->genMarkLabel(L2);

	current_break_label = old_break;
	current_continue_label = old_cont;
}

/* Translating an ast::BreakStmt node.
 */
void Translation::visit(ast::BreakStmt *s) { tr->genJump(current_break_label); }
void Translation::visit(ast::ContStmt *s) { tr->genJump(current_continue_label); }

/* Translating an ast::CompStmt node.
 */
void Translation::visit(ast::CompStmt *c) {
	// translates statement by statement
	for (auto it = c->stmts->begin(); it != c->stmts->end(); ++it)
		(*it)->accept(this);
}
/* Translating an ast::ReturnStmt node.
 */
void Translation::visit(ast::ReturnStmt *s) {
	s->e->accept(this);
	tr->genReturn(s->e->ATTR(val));
}

void Translation::visit(ast::IfExpr *s) {
	Temp tmp = tr->getNewTempI4();
	s -> ATTR(val) = tmp;
	Label L1 = tr->getNewLabel(); // entry of the false branch
	Label L2 = tr->getNewLabel(); // exit
	s->condition->accept(this);
	tr->genJumpOnZero(L1, s->condition->ATTR(val));

	s->true_brch->accept(this);
	tr -> genAssign(tmp, s->true_brch->ATTR(val));
	tr->genJump(L2); // done

	tr->genMarkLabel(L1);
	s->false_brch->accept(this);
	tr -> genAssign(tmp, s->false_brch->ATTR(val));

	tr->genMarkLabel(L2);
}
/* Translating an ast::AddExpr node.
 */
void Translation::visit(ast::AddExpr *e) {
	e->e1->accept(this);
	e->e2->accept(this);

	e->ATTR(val) = tr->genAdd(e->e1->ATTR(val), e->e2->ATTR(val));
}
void Translation::visit(ast::SubExpr *e) {
	e->e1->accept(this);
	e->e2->accept(this);

	e->ATTR(val) = tr->genSub(e->e1->ATTR(val), e->e2->ATTR(val));
}void Translation::visit(ast::MulExpr *e) {
	e->e1->accept(this);
	e->e2->accept(this);

	e->ATTR(val) = tr->genMul(e->e1->ATTR(val), e->e2->ATTR(val));
}void Translation::visit(ast::DivExpr *e) {
	e->e1->accept(this);
	e->e2->accept(this);

	e->ATTR(val) = tr->genDiv(e->e1->ATTR(val), e->e2->ATTR(val));
}void Translation::visit(ast::ModExpr *e) {
	e->e1->accept(this);
	e->e2->accept(this);

	e->ATTR(val) = tr->genMod(e->e1->ATTR(val), e->e2->ATTR(val));
}

void Translation::visit(ast::LeqExpr *e) {
	e->e1->accept(this);
	e->e2->accept(this);

	e->ATTR(val) = tr->genLeq(e->e1->ATTR(val), e->e2->ATTR(val));
}void Translation::visit(ast::GeqExpr *e) {
	e->e1->accept(this);
	e->e2->accept(this);

	e->ATTR(val) = tr->genGeq(e->e1->ATTR(val), e->e2->ATTR(val));
}void Translation::visit(ast::LesExpr *e) {
	e->e1->accept(this);
	e->e2->accept(this);

	e->ATTR(val) = tr->genLes(e->e1->ATTR(val), e->e2->ATTR(val));
}void Translation::visit(ast::GrtExpr *e) {
	e->e1->accept(this);
	e->e2->accept(this);

	e->ATTR(val) = tr->genGtr(e->e1->ATTR(val), e->e2->ATTR(val));
}void Translation::visit(ast::EquExpr *e) {
	e->e1->accept(this);
	e->e2->accept(this);

	e->ATTR(val) = tr->genEqu(e->e1->ATTR(val), e->e2->ATTR(val));
}void Translation::visit(ast::NeqExpr *e) {
	e->e1->accept(this);
	e->e2->accept(this);

	e->ATTR(val) = tr->genNeq(e->e1->ATTR(val), e->e2->ATTR(val));
}void Translation::visit(ast::OrExpr *e) {
	e->e1->accept(this);
	e->e2->accept(this);

	e->ATTR(val) = tr->genLOr(e->e1->ATTR(val), e->e2->ATTR(val));
}void Translation::visit(ast::AndExpr *e) {
	e->e1->accept(this);
	e->e2->accept(this);

	e->ATTR(val) = tr->genLAnd(e->e1->ATTR(val), e->e2->ATTR(val));
}

/* Translating an ast::IntConst node.
 */
void Translation::visit(ast::IntConst *e) {
	e->ATTR(val) = tr->genLoadImm4(e->value);
}

/* Translating an ast::NegExpr node.
 */
void Translation::visit(ast::NegExpr *e) {
	e->e->accept(this);

	e->ATTR(val) = tr->genNeg(e->e->ATTR(val));
}
void Translation::visit(ast::NotExpr *e) {
	e->e->accept(this);

	e->ATTR(val) = tr->genLNot(e->e->ATTR(val));
}
void Translation::visit(ast::BitNotExpr *e) {
	e->e->accept(this);

	e->ATTR(val) = tr->genBNot(e->e->ATTR(val));
}


/* Translating an ast::LvalueExpr node.
 *
 * NOTE:
 *   different Lvalue kinds need different translation
 */
void Translation::visit(ast::LvalueExpr *e) { // we need to regard lvalue as an address
	ast::VarRef *lv = dynamic_cast<ast::VarRef*>(e -> lvalue);
	lv->ATTR(sym)->attachTemp(tr->getNewTempI4(lv -> is_global, lv -> var));
	e -> ATTR(val) = lv->ATTR(sym) -> getTemp();
}

/* Translating an ast::VarRef node.
 *
 * NOTE:
 *   there are two kinds of variable reference: member variables or simple
 * variables
 */
void Translation::visit(ast::VarRef *ref) {
	switch (ref->ATTR(lv_kind)) {
	case ast::Lvalue::SIMPLE_VAR:
		// nothing to do
		break;

	default:
		mind_assert(false); // impossible
	}
	// actually it is so simple :-)
}


/* Translates an entire AST into a Piece list.
 *
 * PARAMETERS:
 *   tree  - the AST
 * RETURNS:
 *   the result Piece list (represented by the first node)
 */
Piece *MindCompiler::translate(ast::Program *tree) {
	TransHelper *helper = new TransHelper(md);

	tree->accept(new Translation(helper));

	return helper->getPiece();
}

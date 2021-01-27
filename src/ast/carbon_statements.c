#include "ast/carbon_statements.h"
#include "ast/carbon_expressions.h"
#include "carbon_object.h"
#include "carbon_token.h"
#include "carbon_value.h"
#include "utils/carbon_memory.h"

#define allocateNode(type, stmtType) allocate(sizeof(type), stmtType)

static CarbonStmt *allocate(uint32_t size, CarbonStmtType type) {
	CarbonStmt *stmt = (CarbonStmt *) carbon_reallocate(0, size, NULL);
	stmt->type = type;
	return stmt;
}

CarbonStmtFunc *carbon_newFuncStmt(CarbonTypename returnType,
								   CarbonToken identifier) {
	CarbonStmtFunc *func =
		(CarbonStmtFunc *) allocateNode(CarbonStmtFunc, StmtFunc);
	func->returnType = returnType;
	func->identifier = identifier;
	func->arguments = NULL;
	func->arity = 0;
	func->argumentCapacity = 0;
	carbon_stmtList_init(&func->statements);
	return func;
}

CarbonStmtPrint *carbon_newPrintStmt(CarbonExpr *expr, CarbonToken token) {
	CarbonStmtPrint *print =
		(CarbonStmtPrint *) allocateNode(CarbonStmtPrint, StmtPrint);
	print->expression = expr;
	print->token = token;
	return print;
}
CarbonStmtExpr *carbon_newExprStmt(CarbonExpr *expr, CarbonToken last) {
	CarbonStmtExpr *stmt =
		(CarbonStmtExpr *) allocateNode(CarbonStmtExpr, StmtExpr);
	stmt->expression = expr;
	stmt->last = last;
	return stmt;
}

CarbonStmtBlock *carbon_newBlockStmt() {
	CarbonStmtBlock *block =
		(CarbonStmtBlock *) allocateNode(CarbonStmtBlock, StmtBlock);
	carbon_stmtList_init(&block->statements);
	block->hasBreak = false;
	block->locals = 0;
	return block;
}

CarbonStmtIf *carbon_newIfStmt(CarbonExpr *expr, CarbonToken token) {
	CarbonStmtIf *stmt = (CarbonStmtIf *) allocateNode(CarbonStmtIf, StmtIf);
	stmt->condition = expr;
	stmt->then = NULL;
	stmt->notThen = NULL;
	stmt->token = token;
	return stmt;
}

CarbonStmtWhile *carbon_newWhileStmt(CarbonExpr *expr, CarbonToken token) {
	CarbonStmtWhile *stmt =
		(CarbonStmtWhile *) allocateNode(CarbonStmtWhile, StmtWhile);
	stmt->condition = expr;
	stmt->body = NULL;
	stmt->token = token;
	return stmt;
}
CarbonStmtBreak *carbon_newBreakStmt(CarbonToken token) {
	CarbonStmtBreak *brk =
		(CarbonStmtBreak *) allocateNode(CarbonStmtBreak, StmtBreak);
	brk->token = token;
	return brk;
}

CarbonStmtVarDec *carbon_newVarDecStmt(CarbonToken identifier,
									   CarbonTypename type,
									   CarbonExpr *initializer) {
	CarbonStmtVarDec *vardec =
		(CarbonStmtVarDec *) allocateNode(CarbonStmtVarDec, StmtVarDec);
	vardec->identifier = identifier;
	vardec->type = type;
	vardec->initializer = initializer;
	return vardec;
}

CarbonStmtReturn *carbon_newReturnStmt(CarbonToken token) {
	CarbonStmtReturn *ret =
		(CarbonStmtReturn *) allocateNode(CarbonStmtReturn, StmtReturn);
	ret->token = token;
	return ret;
}


CarbonStmtFor *carbon_newForStmt(CarbonToken var, CarbonExpr *arr,
		CarbonStmtBlock *body, CarbonToken token){
	CarbonStmtFor* fr = (CarbonStmtFor*) allocateNode(CarbonStmtFor, StmtFor);
	fr->var = var;
	fr->arr = arr;
	fr->body = body;
	fr->token = token;
	return fr;
}

void carbon_freeStmt(CarbonStmt *stmt) {

#define castNode(type, name) type *name = (type *) stmt;

	switch (stmt->type) {
		case StmtPrint: {
			castNode(CarbonStmtPrint, print);
			carbon_freeExpr(print->expression);
			carbon_reallocate(sizeof(CarbonStmtPrint), 0, stmt);
			break;
		}
		case StmtExpr: {
			castNode(CarbonStmtExpr, expr);
			carbon_freeExpr(expr->expression);
			carbon_reallocate(sizeof(CarbonStmtExpr), 0, stmt);
			break;
		}
		case StmtVarDec: {
			castNode(CarbonStmtVarDec, vardec);
			carbon_freeExpr(vardec->initializer);
			carbon_freeTypename(vardec->type);
			carbon_reallocate(sizeof(CarbonStmtVarDec), 0, stmt);
			break;
		}
		case StmtFunc: {
			castNode(CarbonStmtFunc, func);
			carbon_freeTypename(func->returnType);
			for (uint8_t i = 0; i < func->arity; i++) {
				carbon_freeTypename(func->arguments[i].type);
			}

			carbon_stmtList_free(&func->statements);
			carbon_reallocate(sizeof(struct carbon_arg) *
								  func->argumentCapacity,
							  0, func->arguments);
			carbon_reallocate(sizeof(CarbonStmtFunc), 0, stmt);
			break;
		}
		case StmtReturn: {
			castNode(CarbonStmtReturn, ret);
			carbon_freeExpr(ret->expression);
			carbon_reallocate(sizeof(CarbonStmtReturn), 0, stmt);
			break;
		}
		case StmtBlock: {
			castNode(CarbonStmtBlock, block);
			carbon_stmtList_free(&block->statements);
			carbon_reallocate(sizeof(CarbonStmtBlock), 0, stmt);
			break;
		}
		case StmtIf: {
			castNode(CarbonStmtIf, sif);
			carbon_freeStmt((CarbonStmt *) sif->then);
			if (sif->notThen != NULL)
				carbon_freeStmt((CarbonStmt *) sif->notThen);
			carbon_freeExpr(sif->condition);
			carbon_reallocate(sizeof(CarbonStmtIf), 0, stmt);
			break;
		}
		case StmtWhile: {
			castNode(CarbonStmtWhile, swhl);
			carbon_freeExpr(swhl->condition);
			carbon_freeStmt((CarbonStmt *) swhl->body);
			carbon_reallocate(sizeof(CarbonStmtWhile), 0, stmt);
			break;
		}
		case StmtBreak: {
			carbon_reallocate(sizeof(CarbonStmtBreak), 0, stmt);
			break;
		}
		case StmtFor: {
			castNode(CarbonStmtFor, fr);
			carbon_freeExpr(fr->arr);
			carbon_freeStmt((CarbonStmt *) fr->body);
			carbon_reallocate(sizeof(CarbonStmtFor), 0, stmt);
			break;
		}
	}
#undef castNode
}

void carbon_stmtList_init(CarbonStmtList *sl) {
	sl->arr = NULL;
	sl->count = 0;
	sl->capacity = 0;
}
void carbon_stmtList_add(CarbonStmtList *sl, CarbonStmt *stmt) {
	if (sl->count == sl->capacity) {
		uint32_t oldCapacity = sl->capacity * sizeof(CarbonStmt *);
		if (sl->capacity == 0)
			sl->capacity = 8;
		else
			sl->capacity *= 2;
		uint32_t newCapacity = sl->capacity * sizeof(CarbonStmt *);

		sl->arr = carbon_reallocate(oldCapacity, newCapacity, sl->arr);
	}
	sl->arr[sl->count] = stmt;
	sl->count++;
}

void carbon_stmtList_free(CarbonStmtList *sl) {
	for (uint32_t i = 0; i < sl->count; i++) {
		CarbonStmt *stmt = sl->arr[i];
		carbon_freeStmt(stmt);
	}
	carbon_reallocate(sl->capacity * sizeof(CarbonStmt *), 0, sl->arr);
	carbon_stmtList_init(sl);
}

#undef allocateNode

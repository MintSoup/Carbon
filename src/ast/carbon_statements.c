#include "ast/carbon_statements.h"
#include "ast/carbon_expressions.h"
#include "carbon_token.h"
#include "utils/carbon_memory.h"

#define allocateNode(type, stmtType) allocate(sizeof(type), stmtType)

static CarbonStmt *allocate(size_t size, CarbonStmtType type) {
	CarbonStmt *stmt = (CarbonStmt *) carbon_reallocate(0, size, NULL);
	stmt->type = type;
	return stmt;
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

CarbonStmtVarDec *carbon_newVarDecStmt(CarbonToken identifier, CarbonToken type,
									   CarbonExpr *initializer) {
	CarbonStmtVarDec *vardec =
		(CarbonStmtVarDec *) allocateNode(CarbonStmtVarDec, StmtVarDec);
	vardec->identifier = identifier;
	vardec->type = type;
	vardec->initializer = initializer;
	return vardec;
}

void carbon_freeStmt(CarbonStmt *stmt) {
	switch (stmt->type) {
		case StmtPrint: {
			CarbonStmtPrint *print = (CarbonStmtPrint *) stmt;
			carbon_freeExpr(print->expression);
			carbon_reallocate(sizeof(CarbonStmtPrint), 0, stmt);
			break;
		}
		case StmtExpr: {
			CarbonStmtExpr *expr = (CarbonStmtExpr *) stmt;
			carbon_freeExpr(expr->expression);
			carbon_reallocate(sizeof(CarbonStmtExpr), 0, stmt);
			break;
		}
		case StmtVarDec: {
			CarbonStmtVarDec *vardec = (CarbonStmtVarDec *) stmt;
			carbon_freeExpr(vardec->initializer);
			carbon_reallocate(sizeof(CarbonStmtVarDec), 0, stmt);
			break;
		}
	}
}

void carbon_stmtList_init(StatementList *sl) {
	sl->arr = NULL;
	sl->count = 0;
	sl->capacity = 0;
}
void carbon_stmtList_add(StatementList *sl, CarbonStmt* stmt) {
	if (sl->count == sl->capacity) {
		uint32_t oldCapacity = sl->capacity * sizeof(CarbonStmt *);
		if (sl->capacity == 0)
			sl->capacity = 8;
		else
			sl->capacity *= 2;
		uint32_t newCapacity = sl->capacity * sizeof(CarbonStmt *);

		sl->arr =
			carbon_reallocate(oldCapacity, newCapacity, sl->arr);
	}
	sl->arr[sl->count] = stmt;
	sl->count++;
}
 
void carbon_stmtList_free(StatementList *sl) {
	for (uint32_t i = 0; i < sl->count; i++) {
		CarbonStmt *stmt = sl->arr[i];
		carbon_freeStmt(stmt);
	}
	carbon_reallocate(sl->capacity * sizeof(CarbonStmt *), 0,
					  sl->arr);
	carbon_stmtList_init(sl);
}

#undef allocateNode

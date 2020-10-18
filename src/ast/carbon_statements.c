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
	}
}

#undef allocateNode

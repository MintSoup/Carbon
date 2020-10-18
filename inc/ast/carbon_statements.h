#pragma once

#include "ast/carbon_expressions.h"
#include "carbon_token.h"

typedef enum { StmtPrint, StmtExpr } CarbonStmtType;

typedef struct {
	CarbonStmtType type;
} CarbonStmt;

typedef struct {
	CarbonStmt stmt;
	CarbonExpr *expression;
	CarbonToken token;
} CarbonStmtPrint;

typedef struct {
	CarbonStmt stmt;
	CarbonExpr *expression;
	CarbonToken last;
} CarbonStmtExpr;

CarbonStmtPrint *carbon_newPrintStmt(CarbonExpr *expr, CarbonToken token);
CarbonStmtExpr *carbon_newExprStmt(CarbonExpr *expr, CarbonToken last);
void carbon_freeStmt(CarbonStmt *stmt);

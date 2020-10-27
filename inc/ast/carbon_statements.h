#pragma once

#include "ast/carbon_expressions.h"
#include "carbon_token.h"

typedef enum { StmtPrint, StmtExpr, StmtVarDec } CarbonStmtType;



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

typedef struct {
	CarbonStmt stmt;
	CarbonToken identifier;
	CarbonToken type;
	CarbonExpr *initializer; // If this is NULL, does that mean the initializer
							 // couldn't be parsed, or was there none?
} CarbonStmtVarDec;




typedef struct {
	CarbonStmt **arr;
	uint32_t count;
	uint32_t capacity;
} StatementList;





void carbon_stmtList_init(StatementList* sl);
void carbon_stmtList_add(StatementList* sl, CarbonStmt* stmt);
void carbon_stmtList_free(StatementList* sl);


CarbonStmtPrint *carbon_newPrintStmt(CarbonExpr *expr, CarbonToken token);
CarbonStmtExpr *carbon_newExprStmt(CarbonExpr *expr, CarbonToken last);
CarbonStmtVarDec *carbon_newVarDecStmt(CarbonToken identifier, CarbonToken type,
									   CarbonExpr *initializer);
void carbon_freeStmt(CarbonStmt *stmt);

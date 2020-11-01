#pragma once

#include "ast/carbon_expressions.h"
#include "carbon_token.h"

typedef enum {
	StmtPrint,
	StmtExpr,
	StmtVarDec,
	StmtFunc,
	StmtReturn
} CarbonStmtType;

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
	CarbonExpr *initializer;
} CarbonStmtVarDec;

typedef struct {
	CarbonStmt **arr;
	uint32_t count;
	uint32_t capacity;
} CarbonStmtList;

typedef struct {
	CarbonStmt stmt;
	CarbonToken returnType;
	CarbonToken identifier;
	struct carbon_arg {
		CarbonToken type;
		CarbonToken name;
	} * arguments;
	CarbonStmtList statements;
	uint8_t arity;
	uint16_t argumentCapacity;
	uint32_t end;
} CarbonStmtFunc;

typedef struct {
	CarbonStmt stmt;
	CarbonToken token;
	CarbonExpr *expression;
} CarbonStmtReturn;

void carbon_stmtList_init(CarbonStmtList *sl);
void carbon_stmtList_add(CarbonStmtList *sl, CarbonStmt *stmt);
void carbon_stmtList_free(CarbonStmtList *sl);

CarbonStmtFunc *carbon_newFuncStmt(CarbonToken returnType,
								   CarbonToken identifier);
CarbonStmtPrint *carbon_newPrintStmt(CarbonExpr *expr, CarbonToken token);
CarbonStmtExpr *carbon_newExprStmt(CarbonExpr *expr, CarbonToken last);
CarbonStmtVarDec *carbon_newVarDecStmt(CarbonToken identifier, CarbonToken type,
									   CarbonExpr *initializer);
CarbonStmtReturn *carbon_newReturnStmt(CarbonToken token);
void carbon_freeStmt(CarbonStmt *stmt);

#pragma once

#include "ast/carbon_expressions.h"
#include "carbon_token.h"

typedef enum {
	StmtPrint,
	StmtExpr,
	StmtVarDec,
	StmtFunc,
	StmtReturn,
	StmtBlock,
	StmtIf,
	StmtWhile,
	StmtBreak,
	StmtFor,
	StmtClass,
	StmtImport,
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
	CarbonTypename type;
	CarbonExpr *initializer;
} CarbonStmtVarDec;

typedef struct {
	CarbonStmt **arr;
	uint32_t count;
	uint32_t capacity;
} CarbonStmtList;

typedef struct {
	CarbonStmt stmt;
	CarbonTypename returnType;
	CarbonToken identifier;
	struct carbon_arg {
		CarbonTypename type;
		CarbonToken name;
	} * arguments;
	CarbonStmtList statements;
	uint32_t end;
	uint16_t argumentCapacity;
	uint8_t arity;
} CarbonStmtFunc;

typedef struct {
	CarbonStmt stmt;
	CarbonToken token;
	CarbonExpr *expression;
} CarbonStmtReturn;

typedef struct {
	CarbonStmt stmt;
	CarbonStmtList statements;
	uint8_t locals;
	bool hasBreak;
} CarbonStmtBlock;

typedef struct {
	CarbonStmt stmt;
	CarbonExpr *condition;
	CarbonStmt *then;
	CarbonStmt *notThen; // else
	CarbonToken token;
	CarbonToken elseToken;
} CarbonStmtIf;

typedef struct {
	CarbonStmt stmt;
	CarbonExpr *condition;
	CarbonStmtBlock *body;
	CarbonToken token;
} CarbonStmtWhile;

typedef struct {
	CarbonStmt stmt;
	CarbonToken var;
	CarbonExpr *arr;
	CarbonStmtBlock *body;
	CarbonToken token;
} CarbonStmtFor;

typedef struct {
	CarbonStmt *stmt;
	CarbonToken token;
} CarbonStmtBreak;

typedef struct {
	CarbonStmt stmt;
	CarbonToken name;
	CarbonToken superclass;
	CarbonStmtList statements;
	bool hasSuperclass;
} CarbonStmtClass;


typedef struct {
	CarbonStmt stmt;
	CarbonToken token;
} CarbonStmtImport;

void carbon_stmtList_init(CarbonStmtList *sl);
void carbon_stmtList_add(CarbonStmtList *sl, CarbonStmt *stmt);
void carbon_stmtList_free(CarbonStmtList *sl);

CarbonStmtFunc *carbon_newFuncStmt(CarbonTypename returnType,
								   CarbonToken identifier);
CarbonStmtPrint *carbon_newPrintStmt(CarbonExpr *expr, CarbonToken token);
CarbonStmtExpr *carbon_newExprStmt(CarbonExpr *expr, CarbonToken last);
CarbonStmtVarDec *carbon_newVarDecStmt(CarbonToken identifier,
									   CarbonTypename type,
									   CarbonExpr *initializer);
CarbonStmtReturn *carbon_newReturnStmt(CarbonToken token);
CarbonStmtImport *carbon_newImportStmt(CarbonToken what);
CarbonStmtIf *carbon_newIfStmt(CarbonExpr *expr, CarbonToken token);
CarbonStmtWhile *carbon_newWhileStmt(CarbonExpr *expr, CarbonToken tok);
CarbonStmtBreak *carbon_newBreakStmt(CarbonToken token);
CarbonStmtFor *carbon_newForStmt(CarbonToken var, CarbonExpr *arr,
								 CarbonStmtBlock *body, CarbonToken token);
CarbonStmtClass *carbon_newClassStmt(CarbonToken name);

CarbonStmtBlock *carbon_newBlockStmt();

void carbon_freeStmt(CarbonStmt *stmt);
void carbon_freeTypename(CarbonTypename t);

#pragma once

#include "carbon_token.h"
#include "carbon_value.h"

typedef struct carbon_stmtTypename {
	CarbonToken base;
	struct carbon_stmtTypename *templates;
	uint8_t templateCount;
} CarbonTypename;

typedef enum {
	ExprBinary,
	ExprUnary,
	ExprGrouping,
	ExprLiteral,
	ExprCast,
	ExprVar,
	ExprAssignment,
	ExprCall,
	ExprArray,
	ExprIndex,
	ExprIndexAssignment,
	ExprDot,
	ExprDotAssignment,
	ExprIs,
	ExprInit
} CarbonExprType;

typedef struct {
	CarbonExprType type;
	CarbonValueType evalsTo;
	CarbonToken first;
} CarbonExpr;

typedef struct {
	CarbonExpr expr;

	CarbonExpr *left;
	CarbonExpr *right;
	CarbonToken op;
} CarbonExprBinary;

typedef struct {
	CarbonExpr expr;

	CarbonExpr *operand;
	CarbonToken op;
} CarbonExprUnary;

typedef struct {
	CarbonExpr expr;

	CarbonExpr *expression;
} CarbonExprGrouping;

typedef struct {
	CarbonExpr expr;
	CarbonToken token;
} CarbonExprLiteral;

typedef struct {
	CarbonExpr expr;
	CarbonTypename to;
	CarbonExpr *expression;
} CarbonExprCast;

typedef struct {
	CarbonExpr expr;
	CarbonToken token;
} CarbonExprVar;

typedef struct {
	CarbonExpr expr;
	CarbonToken left;
	CarbonExpr *right;
	CarbonToken equals;
} CarbonExprAssignment;

typedef struct {
	CarbonExpr expr;
	CarbonExpr *callee;
	CarbonExpr **arguments;
	uint32_t line;
	uint16_t argumentCapacity;
	uint8_t arity;
} CarbonExprCall;

typedef struct {
	CarbonExpr expr;
	uint32_t count;
	uint32_t capacity;
	CarbonExpr **members;
	CarbonToken bracket;
	enum ArrayInitMethod {
		ImethodStandard,
		ImethodGenerator,
		ImethodContracted
	} imethod;
	CarbonTypename type;
} CarbonExprArray;

typedef struct {
	CarbonExpr expr;
	CarbonExpr *object;
	CarbonExpr *index;
	CarbonToken bracket;
} CarbonExprIndex;

typedef struct {
	CarbonExpr expr;
	CarbonExprIndex *left;
	CarbonExpr *right;
	CarbonToken equals;
} CarbonExprIndexAssignment;

typedef struct {
	CarbonExpr expr;
	CarbonExpr *left;
	CarbonToken right;
	CarbonToken dot;
} CarbonExprDot;

typedef struct {
	CarbonExpr expr;
	CarbonExpr *left;
	CarbonTypename right;
	CarbonToken tok;
} CarbonExprIs;

typedef struct {
	CarbonExpr expr;
	CarbonExprDot* left;
	CarbonExpr* right;
	CarbonToken equals;
} CarbonExprDotAssignment;

CarbonExprBinary *carbon_newBinaryExpr(CarbonExpr *left, CarbonExpr *right,
									   CarbonToken op);

CarbonExprUnary *carbon_newUnaryExpr(CarbonExpr *operand, CarbonToken op);

CarbonExprLiteral *carbon_newLiteralExpr(CarbonToken token);

CarbonExprGrouping *carbon_newGroupingExpr(CarbonExpr *expr);
CarbonExprIndexAssignment *carbon_newIndexAssignmentExpr(CarbonExprIndex *left,
														 CarbonExpr *right,
														 CarbonToken equals);

CarbonExprCast *carbon_newCastExpr(CarbonTypename to, CarbonExpr *expr);
CarbonExprVar *carbon_newVarExpr(CarbonToken token);
CarbonExprCall *carbon_newCallExpr(CarbonExpr *callee, uint32_t line);
CarbonExprAssignment *carbon_newAssignmentExpr(CarbonToken left,
											   CarbonExpr *right,
											   CarbonToken equals);
CarbonExprArray *carbon_newArrayExpr(CarbonToken bracket);
CarbonExprIndex *carbon_newIndexExpr(CarbonExpr *object, CarbonExpr *index,
									 CarbonToken bracket);

CarbonExprDot *carbon_newDotExpr(CarbonExpr *left, CarbonToken right,
								 CarbonToken dot);
CarbonExprDotAssignment *carbon_newDotAssignmentExpr(CarbonExprDot *left, CarbonExpr* right,
								 CarbonToken equals);
CarbonExprIs *carbon_newIsExpr(CarbonExpr *left, CarbonTypename type,
							   CarbonToken is);

void carbon_freeSignature(CarbonFunctionSignature sig);
void carbon_freeExpr(CarbonExpr *expr);
void carbon_freeTypename(CarbonTypename t);
void carbon_freeType(CarbonValueType t);
CarbonValueType carbon_cloneType(CarbonValueType type);

#pragma once

#include "ast/carbon_expressions.h"
#include "carbon_parser.h"
#include "carbon_value.h"
#include "vm/carbon_vm.h"

typedef struct {
	uint8_t depth;
	CarbonString *name;
	CarbonValueType type;
} CarbonLocal;

typedef struct {
	bool parserHadError;
	bool hadError;
	CarbonTable globals;
	CarbonFunction *compilingTo;
	CarbonLocal locals[256];
	uint8_t localCount;
	uint8_t depth;

	struct {
		uint32_t position;
		uint8_t depth;
		CarbonToken token;
		bool isBreak;
	} breaks[256];
	uint8_t breaksCount;
	uint8_t loopDepth;

} CarbonCompiler;

void carbon_compileExpression(CarbonExpr *expr, CarbonChunk *chunk,
							  CarbonCompiler *c, CarbonVM *vm);
void carbon_compileStatement(CarbonStmt *stmt, CarbonChunk *chunk,
							 CarbonCompiler *c, CarbonVM *vm);
void carbon_initCompiler(CarbonCompiler *compiler, CarbonParser *parser);
void carbon_freeCompiler(CarbonCompiler *compiler);
void carbon_markGlobal(CarbonStmt *stmt, CarbonCompiler *c, CarbonVM *vm);

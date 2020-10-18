#pragma once

#include "ast/carbon_expressions.h"
#include "carbon_parser.h"
#include "vm/carbon_vm.h"

typedef struct{
	bool parserHadError;
	bool hadError;
} CarbonCompiler;


void carbon_compileExpression(CarbonExpr *expr, CarbonChunk *chunk, CarbonCompiler* c, CarbonVM* vm);
void carbon_compileStatement(CarbonStmt *stmt, CarbonChunk *chunk, CarbonCompiler* c, CarbonVM* vm);
void carbon_initCompiler(CarbonCompiler* compiler, CarbonParser* parser);
void carbon_freeCompiler(CarbonCompiler* compiler);

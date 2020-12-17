#pragma once

#include "ast/carbon_statements.h"
#include "carbon_compiler.h"
#include "carbon_lexer.h"
#include "carbon_parser.h"
#include "utils/carbon_commons.h"

typedef struct {
	CarbonLexer lexer;
	CarbonParser parser;
	CarbonCompiler compiler;
	CarbonVM vm;
	CarbonStmtList statements;
} CarbonInstance;

typedef enum carbon_runresult {
	Carbon_OK,
	Carbon_Parser_Error,
	Carbon_Compiler_Error,
	Carbon_Runtime_Error
} CarbonRunResult;

struct CarbonFlags {
	bool disassemble;
	bool norun;
};

CarbonRunResult carbon_execute(CarbonInstance *instance, char *source,
							   uint32_t length, struct CarbonFlags flags);
void carbon_init(CarbonInstance *instance);
void carbon_free(CarbonInstance *instance);
bool carbon_isPrimitive(CarbonInstance *instance, char *name) ;
bool carbon_getValue(CarbonInstance *instance, char *name, CarbonValue *out) ;

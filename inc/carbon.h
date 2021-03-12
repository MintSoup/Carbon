#pragma once

#include "ast/carbon_statements.h"
#include "carbon_compiler.h"
#include "carbon_lexer.h"
#include "carbon_parser.h"
#include "utils/carbon_commons.h"

typedef struct {
	CarbonCompiler compiler;
	CarbonVM vm;
	CarbonStmtList statements;
	char *(*readFile)(char *, char *, uint32_t *);
} CarbonState;

enum carbon_runresult {
	Carbon_OK,
	Carbon_Parser_Error,
	Carbon_Compiler_Error,
	Carbon_Runtime_Error
};

struct CarbonFlags {
	bool disassemble;
	bool norun;
};

CarbonRunResult carbon_execute(CarbonState *instance, char *source,
							   uint32_t length, char *name,
							   struct CarbonFlags flags);
void carbon_init(CarbonState *instance,
				 char *(*readFile)(char *, char *, uint32_t *) );
void carbon_free(CarbonState *instance);
bool carbon_isPrimitive(CarbonState *instance, char *name);
bool carbon_getValue(CarbonState *instance, char *name, CarbonValue *out);

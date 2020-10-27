#include "carbon.h"
#include "ast/carbon_statements.h"
#include "carbon_compiler.h"
#include "carbon_parser.h"
#include "carbon_token.h"
#include "utils/carbon_commons.h"
#include "utils/carbon_disassembler.h"
#include "utils/carbon_memory.h"
#include "vm/carbon_chunk.h"
#include "vm/carbon_vm.h"

void carbon_init(CarbonInstance *instance) {
	carbon_initVM(&instance->vm);
	carbon_stmtList_init(&instance->statements);
}


CarbonRunResult carbon_execute(CarbonInstance *instance, char *source,
							   uint32_t length) {
	instance->lexer = carbon_initLexer(source, length);
	carbon_initParser(&instance->parser, &instance->lexer);
	carbon_initCompiler(&instance->compiler, &instance->parser);
	while (instance->parser.tokens[instance->parser.currentToken].type !=
		   TokenEOF) {
		CarbonStmt *stmt = carbon_parseStatement(&instance->parser);
		if (stmt != NULL)
			carbon_stmtList_add(&instance->statements, stmt);
	}
	for (uint32_t i = 0; i < instance->statements.count; i++) {
		CarbonStmt *stmt = instance->statements.arr[i];
		carbon_compileStatement(stmt, &instance->vm.chunk, &instance->compiler,
								&instance->vm);
	}
	carbon_writeToChunk(&instance->vm.chunk, OpReturn, -1);

	bool compilerHadError = instance->compiler.hadError;
	bool parserHadError = instance->parser.hadError;

	carbon_freeParser(&instance->parser);
	carbon_freeCompiler(&instance->compiler);
	carbon_stmtList_free(&instance->statements);

	if (parserHadError)
		return Carbon_Parser_Error;
	if (compilerHadError)
		return Carbon_Compiler_Error;
	carbon_disassemble(&instance->vm.chunk);
	return carbon_run(&instance->vm);
}
void carbon_free(CarbonInstance *instance) {
	carbon_freeVM(&instance->vm);
}

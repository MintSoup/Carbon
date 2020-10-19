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
	instance->statements.arr = NULL;
	instance->statements.count = 0;
	instance->statements.capacity = 0;
}

static void addStatement(CarbonInstance *i, CarbonStmt *stmt) {
#define statements i->statements
	if (statements.count == statements.capacity) {
		uint32_t oldCapacity = statements.capacity * sizeof(CarbonStmt *);
		if (statements.capacity == 0)
			statements.capacity = 8;
		else
			statements.capacity *= 2;
		uint32_t newCapacity = statements.capacity * sizeof(CarbonStmt *);

		statements.arr =
			carbon_reallocate(oldCapacity, newCapacity, statements.arr);
	}
	statements.arr[statements.count] = stmt;
	statements.count++;

#undef statements
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
			addStatement(instance, stmt);
	}
	for (uint32_t i = 0; i < instance->statements.count; i++) {
		CarbonStmt *stmt = instance->statements.arr[i];
		carbon_compileStatement(stmt, &instance->vm.chunk, &instance->compiler,
								&instance->vm);
	}
	carbon_writeToChunk(&instance->vm.chunk, OpReturn, -1);
	if (instance->parser.hadError)
		return Carbon_Parser_Error;
	if (instance->compiler.hadError)
		return Carbon_Compiler_Error;
  	//carbon_disassemble(&instance->vm.chunk);
	return carbon_run(&instance->vm);
}
void carbon_free(CarbonInstance *instance) {
	carbon_freeVM(&instance->vm);
	carbon_freeParser(&instance->parser);
	carbon_freeCompiler(&instance->compiler);

	
	for (uint32_t i = 0; i < instance->statements.count; i++) {
		CarbonStmt *stmt = instance->statements.arr[i];
		carbon_freeStmt(stmt);
	}

	carbon_reallocate(instance->statements.capacity * sizeof(CarbonStmt *), 0,
					  instance->statements.arr);
	instance->statements.capacity = 0;
	instance->statements.capacity = 0;
}

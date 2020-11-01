#include "carbon.h"
#include "ast/carbon_statements.h"
#include "carbon_compiler.h"
#include "carbon_object.h"
#include "carbon_parser.h"
#include "carbon_token.h"
#include "carbon_value.h"
#include "utils/carbon_commons.h"
#include "utils/carbon_disassembler.h"
#include "utils/carbon_memory.h"
#include "utils/carbon_table.h"
#include "vm/carbon_chunk.h"
#include "vm/carbon_vm.h"
#include <stdio.h>
#include <string.h>

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

	CarbonFunction *topLevel =
		carbon_newFunction(NULL, 0, ValueVoid, &instance->vm);

	for (uint32_t i = 0; i < instance->statements.count; i++) {
		CarbonStmt *stmt = instance->statements.arr[i];
		carbon_markGlobal(stmt, &instance->compiler, &instance->vm);
	}

	for (uint32_t i = 0; i < instance->statements.count; i++) {
		CarbonStmt *stmt = instance->statements.arr[i];
		carbon_compileStatement(stmt, &topLevel->chunk, &instance->compiler,
								&instance->vm);
	}

	carbon_writeToChunk(&topLevel->chunk, OpReturnVoid, -1);

	bool compilerHadError = instance->compiler.hadError;
	bool parserHadError = instance->parser.hadError;

	carbon_freeParser(&instance->parser);
	carbon_freeCompiler(&instance->compiler);
	carbon_stmtList_free(&instance->statements);

	if (parserHadError)
		return Carbon_Parser_Error;
	if (compilerHadError)
		return Carbon_Compiler_Error;

	if (1) { // disassembly
		printf("TOP-LEVEL CODE\n");
		carbon_disassemble(&topLevel->chunk);

		for (uint32_t i = 0; i < instance->vm.globals.capacity; i++) {
			CarbonEntry *entry = &instance->vm.globals.entries[i];
			if (entry->key != NULL) {
				CarbonValue function;
				if (!carbon_tableGet(&instance->vm.primitives, entry->key,
									 &function)) {
					carbon_tableGet(&instance->vm.globals, entry->key,
									&function);
					if (function.obj->type == CrbnObjFunc) {
						CarbonFunction *objf = (CarbonFunction *) function.obj;
						printf("--function %s--\n", objf->name->chars);
						carbon_disassemble(&objf->chunk);
					}
				}
			}
		}
	}

	return carbon_run(&instance->vm, topLevel);
}


bool carbon_isPrimitive(CarbonInstance *instance, char *name) {
	CarbonString *str = carbon_copyString(name, strlen(name), &instance->vm);
	CarbonValue dummy;
	return carbon_tableGet(&instance->vm.primitives, (CarbonObj *) str, &dummy);
}

bool carbon_getValue(CarbonInstance *instance, char *name, CarbonValue *out) {
	CarbonString *str = carbon_copyString(name, strlen(name), &instance->vm);
	return carbon_tableGet(&instance->vm.globals, (CarbonObj *) str, out);
}

void carbon_free(CarbonInstance *instance) {
	carbon_freeVM(&instance->vm);
}

#include "carbon.h"
#include "ast/carbon_statements.h"
#include "carbon_compiler.h"
#include "carbon_lexer.h"
#include "carbon_modules.h"
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
#include <stdlib.h>

extern uint32_t heapSize;

void carbon_init(CarbonState *instance,
				 char *(*readFile)(char *, char *, uint32_t *) ) {
	carbon_initVM(&instance->vm);
	carbon_stmtList_init(&instance->statements);
	instance->readFile = readFile;
	carbon_tableInit(&instance->modules);
}

static bool add(CarbonState *state, char *source, uint32_t length, char *from);

static bool loadModule(CarbonToken name, CarbonState *state, char *from) {
	CarbonString *str = carbon_strFromToken(name, &state->vm);
	CarbonValue dummy;
	if (carbon_tableGet(&state->modules, (CarbonObj *) str, &dummy))
		return false;
	for (uint32_t i = 0; i < carbon_modules_count; i++) {
		CarbonModuleHandle const *modh = &carbon_modules[i];
		if (!strcmp(modh->name, str->chars)) {
			carbon_tableSet(&state->modules, (CarbonObj *) str,
							CarbonBool(true));
			CarbonModule *mod = modh->generator(&state->vm);
			for (uint32_t j = 0; j < mod->count; j++) {
				carbon_addBuiltin(&mod->funcs[j], &state->compiler, &state->vm);
			}
			if (mod->src != NULL)
				add(state, mod->src, mod->srclen, modh->name);
			// free the container
			carbon_reallocate(mod->count * sizeof(CarbonModuleElement), 0,
							  mod->funcs);
			carbon_reallocate(sizeof(CarbonModule), 0, mod);
			return false;
		}
	}

	fprintf(stderr, "%s: Cannot load module '%s'\n", from, str->chars);
	return true;
}

static bool add(CarbonState *state, char *source, uint32_t length, char *from) {
	CarbonLexer lexer = carbon_initLexer(source, length, from);
	CarbonParser parser;
	carbon_initParser(&parser, &lexer);

	bool hadError = false;

	while (parser.tokens[parser.currentToken].type != TokenEOF) {
		CarbonStmt *stmt = carbon_parseStatement(&parser);
		if (stmt == NULL)
			continue;

		if (stmt->type == StmtImport) {
			CarbonStmtImport *imp = (CarbonStmtImport *) stmt;
			if (imp->token.type == TokenStringLiteral) {
				char *name = malloc(imp->token.length - 1);
				memcpy(name, imp->token.lexeme + 1, imp->token.length - 2);
				name[imp->token.length - 2] = 0;
				uint32_t length;
				char *file = state->readFile(name, from, &length);
				if (file != NULL)
					hadError = hadError || add(state, file, length, name);
			} else if (imp->token.type == TokenIdentifier) {
				hadError = hadError || loadModule(imp->token, state, from);
			}
			carbon_freeStmt(stmt);
		} else
			carbon_stmtList_add(&state->statements, stmt);
	}
	hadError = hadError || parser.hadError;
	carbon_freeParser(&parser);
	return hadError;
}

CarbonRunResult carbon_execute(CarbonState *state, char *source,
							   uint32_t length, char *name,
							   struct CarbonFlags flags) {

	carbon_initCompiler(&state->compiler);
	bool parserHadError = add(state, source, length, name);
	state->compiler.parserHadError = parserHadError;

	CarbonFunction *topLevel = carbon_newFunction(NULL, 0, NULL, &state->vm);

	for (uint32_t i = 0; i < state->statements.count; i++) {
		CarbonStmt *stmt = state->statements.arr[i];
		carbon_scoutClass(stmt, &state->compiler, &state->vm);
	}

	for (uint32_t i = 0; i < state->statements.count; i++) {
		CarbonStmt *stmt = state->statements.arr[i];
		carbon_markGlobal(stmt, &state->compiler, &state->vm);
	}
	state->vm.classCount = state->compiler.classCount;
	state->vm.classes = carbon_reallocate(
		0, state->vm.classCount * sizeof(struct carbon_class), NULL);

	for (uint32_t i = 0; i < state->statements.count; i++) {
		CarbonStmt *stmt = state->statements.arr[i];
		carbon_compileStatement(stmt, &topLevel->chunk, &state->compiler,
								&state->vm);
	}

	carbon_writeToChunk(&topLevel->chunk, OpReturnVoid, -1);

	bool compilerHadError = state->compiler.hadError;

	carbon_freeCompiler(&state->compiler);
	carbon_stmtList_free(&state->statements);

	if (parserHadError)
		return Carbon_Parser_Error;
	if (compilerHadError)
		return Carbon_Compiler_Error;

	if (flags.disassemble) { // disassembly
		CarbonObj *o = state->vm.objects;
		while (o != NULL) {
			if (o->type == CrbnObjFunc) {
				CarbonFunction *f = (CarbonFunction *) o;
				if (f->name == NULL)
					printf("-<global>-\n");
				else
					printf("-<function %s>-\n", f->name->chars);
				carbon_disassemble(&f->chunk, &state->vm);
				puts("");
			}
			o = o->next;
		}
	}

	if (!flags.norun) {
		CarbonRunResult r = carbon_run(&state->vm, topLevel);
		return r;
	} else
		return Carbon_OK;
}

bool carbon_isPrimitive(CarbonState *instance, char *name) {
	CarbonString *str = carbon_copyString(name, strlen(name), &instance->vm);
	CarbonValue dummy;
	return carbon_tableGet(&instance->vm.primitives, (CarbonObj *) str, &dummy);
}

bool carbon_getValue(CarbonState *instance, char *name, CarbonValue *out) {
	CarbonString *str = carbon_copyString(name, strlen(name), &instance->vm);
	return carbon_tableGet(&instance->vm.globals, (CarbonObj *) str, out);
}

void carbon_free(CarbonState *instance) {
	carbon_freeVM(&instance->vm);
	carbon_tableFree(&instance->modules);
}

#pragma once

#include "ast/carbon_expressions.h"
#include "carbon_modules.h"
#include "carbon_parser.h"
#include "carbon_value.h"
#include "vm/carbon_vm.h"

typedef struct {
	CarbonValueType type;
	CarbonString *name;
	uint8_t depth;
} CarbonLocal;

struct carbon_compiler {
	struct {
		CarbonToken token;
		uint32_t position;
		uint8_t depth;
		bool isBreak;
	} breaks[256];

	struct carbon_classInfo {
		struct carbon_classInfo *parent;
		struct field {
			CarbonValueType type;
			CarbonString *name;
		} * fields;
		struct method {
			CarbonFunctionSignature sig;
			CarbonString *name;
		} * methods;
		bool hasInit;
		bool copy;
		uint8_t init;
		uint8_t id;
		uint8_t fieldCount;
		uint8_t fieldCap; // Capacity
		uint8_t methodCount;
		uint8_t methodCap; // Capacity
	} * class;
	CarbonTable globals;
	CarbonFunction *compilingTo;
	CarbonLocal locals[256];
	CarbonTable classes;
	int16_t selfSlot;
	uint8_t localCount;
	uint8_t depth;
	uint8_t breaksCount;
	uint8_t loopDepth;
	uint8_t classCount;
	bool parserHadError;
	bool hadError;
};

void carbon_compileExpression(CarbonExpr *expr, CarbonChunk *chunk,
							  CarbonCompiler *c, CarbonVM *vm);
void carbon_compileStatement(CarbonStmt *stmt, CarbonChunk *chunk,
							 CarbonCompiler *c, CarbonVM *vm);
void carbon_initCompiler(CarbonCompiler *compiler);
void carbon_freeCompiler(CarbonCompiler *compiler);
void carbon_markGlobal(CarbonStmt *stmt, CarbonCompiler *c, CarbonVM *vm);
void carbon_scoutClass(CarbonStmt *stmt, CarbonCompiler *c, CarbonVM *vm);
bool carbon_isSuperclass(CarbonValueType from, CarbonValueType to,
						 CarbonCompiler *c);
void carbon_addBuiltin(CarbonModuleElement *modelm, CarbonCompiler *c,
					   CarbonVM *vm);

#pragma once

#include "carbon_value.h"
#include "vm/carbon_chunk.h"
#include "utils/carbon_commons.h"
#include "utils/carbon_table.h"

typedef struct carbon_object CarbonObj;
typedef struct carbon_function CarbonFunction;
typedef enum carbon_runresult CarbonRunResult;

typedef struct {
	CarbonFunction *func;
	uint8_t *ip;
	CarbonValue *slots;
} CarbonCallframe;

typedef struct {
	CarbonValue stack[CARBON_StackSize];
	uint16_t stackTop;
	CarbonObj *objects;
	uint32_t objectHeapSize;

	CarbonTable strings;
	CarbonTable globals;
	CarbonTable primitives;

	CarbonCallframe callStack[256];
	uint8_t callDepth;

} CarbonVM;

void carbon_initVM(CarbonVM *vm);
void carbon_freeVM(CarbonVM *vm);
CarbonRunResult carbon_run(CarbonVM *vm, CarbonFunction* func);

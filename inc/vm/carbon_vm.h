#pragma once

#include "carbon_value.h"
#include "vm/carbon_chunk.h"
#include "utils/carbon_commons.h"
#include "utils/carbon_table.h"

typedef struct carbon_function CarbonFunction;
typedef enum carbon_runresult CarbonRunResult;

typedef struct {
	CarbonFunction *func;
	uint8_t *ip;
	CarbonValue *slots;
} CarbonCallframe;

typedef struct carbon_vm {
	CarbonValue stack[CARBON_StackSize];

	CarbonObj *objects;

	CarbonObj **greyStack;
	CarbonObj **gcarr;

	CarbonTable strings;
	CarbonTable globals;
	CarbonTable primitives;

	CarbonCallframe callStack[256];


	CarbonValueType *typeData;

	struct carbon_class {
		CarbonFunction **methods;
		int16_t superclass;
		uint8_t init;
		uint8_t methodCount;
		uint8_t fieldCount;
	} * classes;

	uint32_t objectHeapSize;
	uint32_t objectCount;

	uint32_t gcarrSize;
	uint32_t greySize;
	uint32_t greyTop;

	uint16_t stackTop;
	uint16_t typeCount;
	uint16_t typeCapacity;

	uint8_t classCount;
	uint8_t callDepth;

	bool gc;
} CarbonVM;

void carbon_initVM(CarbonVM *vm);
void carbon_freeVM(CarbonVM *vm);
uint16_t carbon_pushType(CarbonVM* vm, CarbonValueType type);
CarbonRunResult carbon_run(CarbonVM *vm, CarbonFunction *func);

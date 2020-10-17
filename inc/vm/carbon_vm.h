#pragma once


#include "carbon_value.h"
#include "vm/carbon_chunk.h"
#include "utils/carbon_commons.h"
#include "utils/carbon_table.h"


typedef struct carbon_object CarbonObj;

typedef struct {
	CarbonChunk chunk;
	CarbonValue stack[CARBON_StackSize];
	uint16_t stackTop;
	CarbonObj* objects;	
	size_t objectHeapSize;
	CarbonTable strings;
} CarbonVM;

void carbon_initVM(CarbonVM *vm);
void carbon_freeVM(CarbonVM *vm);
void carbon_run(CarbonVM *vm);


#pragma once

#include "carbon_value.h"
#include "vm/carbon_chunk.h"

#define StackSize 256

typedef struct carbon_object CarbonObj;

typedef struct {
	CarbonChunk chunk;
	CarbonValue stack[StackSize];
	uint16_t stackTop;
	CarbonObj* objects;	
	size_t objectHeapSize;
} CarbonVM;

void carbon_initVM(CarbonVM *vm);
void carbon_freeVM(CarbonVM *vm);
void carbon_run(CarbonVM *vm);

#undef StackSize

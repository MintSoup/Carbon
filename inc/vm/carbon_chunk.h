#pragma once

#include "carbon_value.h"
#include "utils/carbon_commons.h"

typedef enum {
	// Misc
	OpLoadConstant,
	OpReturn,

	// Arithmetic
	OpAddInt,
	OpAddUInt,
	OpAddDouble,
	OpSubInt,
	OpSubUInt,
	OpSubDouble,
	OpMulInt,
	OpMulUInt,
	OpMulDouble,
	OpDivInt,
	OpDivUInt,
	OpDivDouble,

	// Castings
	OpIntToDouble,
	OpDoubleToInt,
	OpUIntToDouble,
	OpDoubleToUInt

} CarbonOpCode;

typedef struct {
	uint8_t *code;
	uint32_t *lines;
	uint32_t capacity;
	uint32_t count;
	CarbonValueArray constants;
} CarbonChunk;

void carbon_initChunk(CarbonChunk *chunk);
void carbon_writeToChunk(CarbonChunk *chunk, uint8_t data, uint32_t line);
void carbon_freeChunk(CarbonChunk *chunk);
void carbon_addConstant(CarbonChunk *chunk, CarbonValue constant);

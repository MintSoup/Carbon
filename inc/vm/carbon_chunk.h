#pragma once

#include "carbon_value.h"
#include "utils/carbon_commons.h"

typedef enum {
	// Misc
	OpLoadConstant,
	OpLoadConstant16,
	OpReturn,
	OpReturnVoid,
	OpConcat,
	OpPop,
	OpPopn,
	OpPush0,
	OpPush1,
	OpCall,
	OpLen,
	OpAppend,
	OpGetIndex,
	OpSetIndex,
	OpMakeArray,
	OpMakeArray64,
	OpInitArray,
	OpMakeGenerator,

	// Binary Operations
	OpAddInt,
	OpAddDouble,
	OpSubInt,
	OpSubDouble,
	OpMulInt,
	OpMulUInt,
	OpMulDouble,
	OpDivInt,
	OpDivUInt,
	OpDivDouble,
	OpMod,

	// Unary operations
	OpNegateDouble,
	OpNegateInt,
	OpNegateUInt,
	OpNegateBool,

	// Castings
	OpIntToDouble,
	OpDoubleToInt,
	OpUIntToDouble,
	OpDoubleToUInt,

	// Comparison and equality
	OpCompareInt,
	OpCompareUInt,
	OpCompareDouble,
	OpEquals,
	OpNotEquals,
	OpGreater,
	OpLess,
	OpGEQ,
	OpLEQ,

	// Prints
	OpPrintInt,
	OpPrintUInt,
	OpPrintDouble,
	OpPrintBool,
	OpPrintObj,

	// Variables
	OpSetGlobal,
	OpGetGlobal,
	OpSetGlobalInline,
	OpGetGlobalInline,
	OpSetLocal,
	OpGetLocal,

	// Control flow
	OpJumpOnFalse,
	OpJumpOnTrue,
	OpJump,
	OpIf,
	OpLoop

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
uint16_t carbon_addConstant(CarbonChunk *chunk, CarbonValue constant);

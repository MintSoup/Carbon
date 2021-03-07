#pragma once

#include "carbon_value.h"
#include "utils/carbon_commons.h"

typedef enum {
	// Misc
	OpConcat,
	OpCall,
	OpLen,
	OpBuiltin,
	OpIs,
	OpCastcheck,
	OpIsInstance,
	OpInstanceCastcheck,
	OpSuper,

	// Constants
	OpLoadConstant,
	OpLoadConstant16,

	// Returns
	OpReturn,
	OpReturnVoid,

	// Stack manipulation
	OpPop,
	OpPopn,
	OpPush0,
	OpPush1,

	// Array operations
	OpGetIndex,
	OpSetIndex,
	OpMakeArray,
	OpMakeArray64,
	OpInitArray,
	OpMakeGenerator,
	OpAppend,

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

	// Casts
	OpIntToDouble,
	OpDoubleToInt,
	OpUIntToDouble,
	OpDoubleToUInt,
	OpToBool,

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
	OpLoop,
	OpFor,

	// Class
	OpDot,
	OpDotSet,
	OpMethod,
	OpMakeInstance,
	OpInitInstance,

} CarbonOpCode;

typedef struct {
	uint8_t *code;
	struct carbon_lineInfo{
		uint32_t count;
		uint32_t line;
	} *lines;
	CarbonValueArray constants;
	CarbonValueType *typeData;
	uint32_t lineCount;
	uint32_t lineCapacity;
	uint32_t capacity;
	uint32_t count;
	uint16_t typeCount;
	uint16_t typeCapacity;
} CarbonChunk;

void carbon_initChunk(CarbonChunk *chunk);
void carbon_writeToChunk(CarbonChunk *chunk, uint8_t data, uint32_t line);
void carbon_freeChunk(CarbonChunk *chunk);
uint16_t carbon_pushType(CarbonChunk *chunk, CarbonValueType type);
uint16_t carbon_addConstant(CarbonChunk *chunk, CarbonValue constant, bool primitive);
uint32_t carbon_getLine(CarbonChunk *c, uint32_t n);

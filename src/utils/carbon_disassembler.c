#include "utils/carbon_commons.h"
#include "utils/carbon_disassembler.h"
#include "vm/carbon_chunk.h"
#include <stdio.h>

static char *names[] = {

	// Misc
	[OpLoadConstant] = "load",
	[OpLoadConstant16] = "load16",
	[OpReturn] = "return",

	// Binary Operations
	[OpAddInt] = "iadd",
	[OpAddDouble] = "dadd",
	[OpSubInt] = "isub",
	[OpSubDouble] = "dsub",
	[OpMulInt] = "imul",
	[OpMulUInt] = "umul",
	[OpMulDouble] = "dmul",
	[OpDivInt] = "idiv",
	[OpDivUInt] = "udiv",
	[OpDivDouble] = "ddiv",

	// Unary operations
	[OpNegateDouble] = "dneg",
	[OpNegateInt] = "ineg",
	[OpNegateUInt] = "uneg",
	[OpNegateBool] = "bneg",

	// Castings
	[OpIntToDouble] = "i2d",
	[OpDoubleToInt] = "d2i",
	[OpUIntToDouble] = "u2d",
	[OpDoubleToUInt] = "d2u",

	// Comparison and equality
	[OpCompareInt] = "icmp",
	[OpCompareUInt] = "ucmp",
	[OpCompareDouble] = "dcmp",
	[OpEquals] = "eq",
	[OpNotEquals] = "neq",
	[OpGreater] = "grt",
	[OpLess] = "less",
	[OpGEQ] = "geq",
	[OpLEQ] = "leq"

};

void carbon_disassemble(CarbonChunk *chunk) {
	uint32_t instructionNumber = 0;
	uint8_t *ip = chunk->code;

	while (ip < chunk->code + chunk->count) {
		printf("%04d %04d|0x%04lx  %s",
			   chunk->lines[ip - chunk->code], instructionNumber,
			   ip - chunk->code, names[*ip]);

		switch (*ip) {
			case OpReturn:
			case OpAddInt:
			case OpAddDouble:
			case OpSubInt:
			case OpSubDouble:
			case OpMulInt:
			case OpMulUInt:
			case OpMulDouble:
			case OpDivInt:
			case OpDivUInt:
			case OpDivDouble:
			case OpNegateDouble:
			case OpNegateInt:
			case OpNegateUInt:
			case OpNegateBool:
			case OpIntToDouble:
			case OpDoubleToInt:
			case OpUIntToDouble:
			case OpDoubleToUInt:
			case OpCompareInt:
			case OpCompareUInt:
			case OpCompareDouble:
			case OpEquals:
			case OpNotEquals:
			case OpGreater:
			case OpLess:
			case OpGEQ:
			case OpLEQ:
				ip++;
				break;
			case OpLoadConstant:
				ip++;
				printf("\t%d", *ip);
				ip++;
				break;
			case OpLoadConstant16:
				ip++;
				uint8_t higher = *ip;
				ip++;
				uint8_t lower = *ip;
				printf("\t%d", (higher << 8) | lower);
				ip++;
				break;
		}
		puts("");
		instructionNumber++;
	}
}

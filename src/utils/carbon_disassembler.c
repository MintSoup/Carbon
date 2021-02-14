#include "carbon_object.h"
#include "carbon_value.h"
#include "utils/carbon_commons.h"
#include "utils/carbon_disassembler.h"
#include "vm/carbon_chunk.h"

extern char *CarbonValueTypeLexeme[];

static char *names[] = {

	// Misc
	[OpLoadConstant] = "load",
	[OpLoadConstant16] = "load16",
	[OpReturn] = "return",
	[OpReturnVoid] = "vreturn",
	[OpPop] = "pop",
	[OpPopn] = "popn",
	[OpPush0] = "push0",
	[OpPush1] = "push1",
	[OpCall] = "call",
	[OpLen] = "len",
	[OpAppend] = "appnd",
	[OpGetIndex] = "iget",
	[OpSetIndex] = "iset",
	[OpMakeArray] = "mkarr",
	[OpMakeArray64] = "mkarr64",
	[OpInitArray] = "initarr",
	[OpMakeGenerator] = "mkgen",
	[OpBuiltin] = "builtin",
	[OpIs] = "is",
	[OpCastcheck] = "castchk",

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
	[OpMod] = "mod",

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
	[OpLEQ] = "leq",
	[OpConcat] = "concat",

	[OpPrintInt] = "iprint",
	[OpPrintUInt] = "uprint",
	[OpPrintDouble] = "dprint",
	[OpPrintBool] = "bprint",
	[OpPrintObj] = "oprint",

	[OpSetGlobal] = "globset",
	[OpGetGlobal] = "globget",
	[OpSetGlobalInline] = "nglobset",
	[OpGetGlobalInline] = "nglobget",
	[OpSetLocal] = "locset",
	[OpGetLocal] = "locget",

	[OpJumpOnFalse] = "jz",
	[OpJumpOnTrue] = "jnz",
	[OpJump] = "jmp",
	[OpIf] = "if",
	[OpLoop] = "loop",
	[OpFor] = "for",

};

char *builtinFunctionNames[] = {[BuiltinAppend] = "append"};

void carbon_disassemble(CarbonChunk *chunk) {
	uint32_t instructionNumber = 0;
	uint8_t *ip = chunk->code;

	while (ip < chunk->code + chunk->count) {
		printf("%04d %04d|%04ld  %s", chunk->lines[ip - chunk->code],
			   instructionNumber, ip - chunk->code, names[*ip]);

		switch (*ip) {
			case OpReturn:
			case OpReturnVoid:
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
			case OpMod:
			case OpCompareInt:
			case OpCompareUInt:
			case OpCompareDouble:
			case OpEquals:
			case OpNotEquals:
			case OpGreater:
			case OpLess:
			case OpGEQ:
			case OpLEQ:
			case OpConcat:
			case OpPrintInt:
			case OpPrintUInt:
			case OpPrintDouble:
			case OpPrintObj:
			case OpPrintBool:
			case OpPop:
			case OpPush0:
			case OpPush1:
			case OpGetGlobal:
			case OpSetGlobal:
			case OpLen:
			case OpAppend:
			case OpSetIndex:
			case OpGetIndex:
			case OpInitArray:
				ip++;
				break;
			case OpLoadConstant:
			case OpGetGlobalInline:
			case OpSetGlobalInline:
			case OpSetLocal:
			case OpGetLocal:
			case OpCall:
			case OpPopn:
				ip++;
				printf("\t%u", *ip);
				ip++;
				break;
			case OpLoadConstant16:
			case OpJumpOnFalse:
			case OpJumpOnTrue:
			case OpIf:
			case OpLoop:
			case OpJump:
			case OpFor: {
				ip++;
				uint8_t higher = *ip;
				ip++;
				uint8_t lower = *ip;
				printf("\t%d", (higher << 8) | lower);
				ip++;
				break;
			}
			case OpMakeGenerator:
			case OpMakeArray64: {
				ip++;
				uint8_t higher = *ip;
				ip++;
				uint8_t lower = *ip;
				printf("\t");
				carbon_printType(stdout,
								 chunk->typeData[(higher << 8) | lower]);
				ip++;
				break;
			}
			case OpMakeArray: {
				ip++;
				printf("\t%u\t", *ip);
				ip++;
				uint8_t higher = *ip;
				ip++;
				uint8_t lower = *ip;
				carbon_printType(stdout,
								 chunk->typeData[(higher << 8) | lower]);
				ip++;
				break;
			}
			case OpBuiltin: {
				ip++;
				printf("\t%s\t", builtinFunctionNames[*ip]);
				ip += 3;
				break;
			}
			case OpIs:
			case OpCastcheck: {
				ip++;
				printf("\t");
				uint8_t higher = *ip;
				ip++;
				uint8_t lower = *ip;
				carbon_printType(stdout,
								 chunk->typeData[(higher << 8) | lower]);
				ip++;
				break;
			}
		}
		puts("");
		instructionNumber++;
	}
}

static void printSig(FILE *f, CarbonFunctionSignature sig) {
	fprintf(f, "<");
	carbon_printType(f, *sig.returnType);
	if (sig.arity > 0)
		fprintf(f, ", ");

	for (uint8_t i = 0; i < sig.arity; i++) {
		carbon_printType(f, sig.arguments[i]);
		if (i + 1 < sig.arity)
			fprintf(f, ", ");
	}
	fprintf(f, ">");
}

void carbon_printType(FILE *f, CarbonValueType type) {
	if (type.tag != ValueInstance)
		fprintf(f, "%s", CarbonValueTypeLexeme[type.tag]);
	else
		fprintf(f, "%s", type.compound.instanceName->chars);

	if (type.compound.memberType != NULL)
		switch (type.tag) {
			case ValueGenerator:
			case ValueArray:
				fprintf(f, "<");
				carbon_printType(f, *type.compound.memberType);
				fprintf(f, ">");
				break;
			case ValueFunction:
				printSig(f, *type.compound.signature);
				break;
			default:
				break;
		}
}

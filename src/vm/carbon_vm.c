#include "vm/carbon_vm.h"
#include "carbon_value.h"
#include "vm/carbon_chunk.h"
#include <sys/types.h>

void carbon_initVM(CarbonVM *vm) {
	vm->stackTop = 0;
	carbon_initChunk(&vm->chunk);
}
void carbon_freeVM(CarbonVM *vm) {
	carbon_freeChunk(&vm->chunk);
}

static inline CarbonValue pop(CarbonVM *vm) {
	return vm->stack[--vm->stackTop];
}

static inline void push(CarbonValue v, CarbonVM *vm) {
	vm->stack[vm->stackTop++] = v;
}

static inline CarbonValue c16(CarbonVM *vm, uint8_t *ip) {
	uint8_t higher = *ip;
	ip++;
	uint8_t lower = *ip;
	uint16_t index = (higher << 8) | lower;
	return vm->chunk.constants.arr[index];
}

void carbon_run(CarbonVM *vm) {

#define ReadByte() *(ip++)
#define push(x) push(x, vm)
	register uint8_t *ip = vm->chunk.code;
#define pop() pop(vm)
#define peek() vm->stack[vm->stackTop - 1]

#define binary(type, operator, cast, fieldName)                                \
	do {                                                                       \
		type a = pop().fieldName;                                              \
		type b = pop().fieldName;                                              \
		push(cast(b operator a));                                              \
	} while (false)

#define unary(type, operator, cast, fieldName)                                 \
	do {                                                                       \
		type a = pop().fieldName;                                              \
		push(cast(operator a));                                                \
	} while (false)

#define cast(from, to, nativeType) push(to((nativeType) pop().from))
#define ReadConstant8() vm->chunk.constants.arr[*ip]
#define ReadConstant16() c16(vm, ip)
	while (true) {
		switch (*ip) {
		case OpReturn:
			return;

		// Signed integer binary ops
		case OpAddInt:
			binary(int64_t, +, CarbonInt, sint);
			ip++;
			break;
		case OpSubInt:
			binary(int64_t, -, CarbonInt, sint);
			ip++;
			break;
		case OpDivInt:
			binary(int64_t, /, CarbonInt, sint);
			ip++;
			break;
		case OpMulInt:
			binary(int64_t, *, CarbonInt, sint);
			ip++;
			break;

		// Unsigned integer binary ops
		case OpAddUInt:
			binary(uint64_t, +, CarbonUInt, uint);
			ip++;
			break;
		case OpSubUInt:
			binary(uint64_t, -, CarbonUInt, uint);
			ip++;
			break;
		case OpDivUInt:
			binary(uint64_t, /, CarbonUInt, uint);
			ip++;
			break;
		case OpMulUInt:
			binary(uint64_t, *, CarbonUInt, uint);
			ip++;
			break;

		// Double binary ops
		case OpAddDouble:
			binary(double, +, CarbonDouble, dbl);
			ip++;
			break;
		case OpSubDouble:
			binary(double, -, CarbonDouble, dbl);
			ip++;
			break;
		case OpDivDouble:
			binary(double, /, CarbonDouble, dbl);
			ip++;
			break;
		case OpMulDouble:
			binary(double, *, CarbonDouble, dbl);
			ip++;
			break;

		// Unary ops
		case OpNegateBool:
			unary(bool, !, CarbonBool, boolean);
			ip++;
			break;
		case OpNegateUInt:
			unary(uint64_t, -, CarbonInt, uint);
			ip++;
			break;
		case OpNegateInt:
			unary(int64_t, -, CarbonInt, sint);
			ip++;
			break;
		case OpNegateDouble:
			unary(double, -, CarbonDouble, dbl);
			ip++;
			break;

		// Primitive casts
		case OpDoubleToInt:
			cast(dbl, CarbonInt, int64_t);
			ip++;
			break;
		case OpDoubleToUInt:
			cast(dbl, CarbonUInt, uint64_t);
			ip++;
			break;
		case OpIntToDouble:
			cast(sint, CarbonDouble, double);
			ip++;
			break;
		case OpUIntToDouble:
			cast(uint, CarbonDouble, double);
			ip++;
			break;

		// Constant instructions
		case OpLoadConstant:
			ip++;
			push(ReadConstant8());
			ip++;
			break;
		case OpLoadConstant16:
			ip++;
			push(ReadConstant16());
			ip += 2;
			break;
		}
	}
}

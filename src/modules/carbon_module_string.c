#include "carbon_modules.h"

#ifdef CARBON_MODULE_STRING

#include "carbon_value.h"
#include "utils/carbon_commons.h"
#include "utils/carbon_memory.h"
#include "carbon_object.h"
#include "vm/carbon_vm.h"
#include <string.h>
#include <stdlib.h>
#include <stdio.h>

static const uint32_t members = 6;
static char *src = "import error;";

static inline bool isNumeric(char i) {
	return i >= '0' && i <= '9';
}
static inline bool isHex(char i) {
	return isNumeric(i) || (i >= 'A' && i <= 'F') || (i >= 'a' && i <= 'f');
}
static inline bool isBin(char i) {
	return i == '1' || i == '0';
}
static inline bool isOct(char i) {
	return i >= '0' && i <= '7';
}

CarbonFunctionSignature *sigX2S(CarbonVM *vm, enum CarbonValueTag tag) {
	CarbonFunctionSignature *sig =
		carbon_reallocate(0, sizeof(CarbonFunctionSignature), NULL);
	sig->arity = 1;
	sig->returnType = carbon_reallocate(0, sizeof(CarbonValueType), NULL);
	sig->returnType->tag = ValueString;
	sig->returnType->compound.instanceName = NULL;

	sig->arguments = carbon_reallocate(0, sizeof(CarbonValueType), NULL);

	sig->arguments[0].compound.instanceName = NULL;
	sig->arguments[0].tag = tag;

	CarbonValueType t = {.tag = ValueFunction, .compound = {.signature = sig}};
	carbon_pushType(vm, t);
	return sig;
}

CarbonFunctionSignature *sigS2X(CarbonVM *vm, enum CarbonValueTag tag) {
	CarbonFunctionSignature *sig =
		carbon_reallocate(0, sizeof(CarbonFunctionSignature), NULL);
	sig->arity = 2;
	sig->returnType = carbon_reallocate(0, sizeof(CarbonValueType), NULL);
	sig->returnType->tag = tag;
	sig->returnType->compound.instanceName = NULL;

	sig->arguments = carbon_reallocate(0, sizeof(CarbonValueType) * 2, NULL);

	sig->arguments[0].compound.instanceName = NULL;
	sig->arguments[0].tag = ValueString;

	sig->arguments[1].compound.instanceName =
		carbon_copyString("@Error", 6, vm);
	sig->arguments[1].tag = ValueInstance;

	CarbonValueType t = {.tag = ValueFunction, .compound = {.signature = sig}};
	carbon_pushType(vm, t);
	return sig;
}

char *mD2S(CarbonObj *parent, CarbonValue *args, CarbonVM *vm) {
	double x = args[0].dbl;
	uint32_t length = snprintf(NULL, 0, "%lf", x);
	char *c = carbon_reallocateObj(0, length + 1, NULL, vm);
	snprintf(c, length + 1, "%g", x);
	CarbonString *str = carbon_takeString(c, length, vm);

	vm->stack[vm->stackTop++] = CarbonObject((CarbonObj *) str);

	return NULL;
}
char *mI2S(CarbonObj *parent, CarbonValue *args, CarbonVM *vm) {
	int64_t x = args[0].sint;
	uint32_t length = snprintf(NULL, 0, "%" PRId64, x);
	char *c = carbon_reallocateObj(0, length + 1, NULL, vm);
	snprintf(c, length + 1, "%" PRId64, x);
	CarbonString *str = carbon_takeString(c, length, vm);

	vm->stack[vm->stackTop++] = CarbonObject((CarbonObj *) str);

	return NULL;
}
char *mU2S(CarbonObj *parent, CarbonValue *args, CarbonVM *vm) {
	uint64_t x = args[0].uint;
	uint32_t length = snprintf(NULL, 0, "%" PRIu64, x);
	char *c = carbon_reallocateObj(0, length + 1, NULL, vm);
	snprintf(c, length + 1, "%" PRIu64, x);
	CarbonString *str = carbon_takeString(c, length, vm);

	vm->stack[vm->stackTop++] = CarbonObject((CarbonObj *) str);

	return NULL;
}

bool isValid(CarbonString *x, char **num, uint32_t *base, bool allowNegative,
			 CarbonVM *vm) {
	bool noZero = true;
	char *first = x->chars;
	char *last = x->chars + x->length;

	if (allowNegative) {
		if (*first == '-')
			first++;
	}

	while (first < last && *first == '0') {
		first++;
		noZero = false;
	}
	if (first < last) {
		switch (*first) {
			case 'x':
				if (noZero)
					return false;

				*num = ++first;
				while (isHex(*first))
					first++;
				if (first != last)
					return false;

				*base = 16;
				break;
			case 'b':
				if (noZero)
					return false;

				*num = ++first;
				while (isBin(*first))
					first++;
				if (first != last)
					return false;

				*base = 2;
				break;
			case 'o':
				if (noZero)
					return false;

				*num = ++first;
				while (isOct(*first))
					first++;
				if (first != last)
					return false;

				*base = 2;
			default:
				*num = first;
				while (isNumeric(*first))
					first++;
				if (first != last)
					return false;
				*base = 10;
		}
	} else {
		if (noZero) // empty string
			return false;

		*base = 10; // can be anything really, we only have zeroes
		*num = x->chars;
		return true;
	}
	return true;
}

char *mS2U(CarbonObj *parent, CarbonValue *args, CarbonVM *vm) {
	CarbonString *x = (CarbonString *) args[0].obj;
	CarbonInstance *err = (CarbonInstance *) args[1].obj;

	uint32_t base;
	char *num;
	char *last = x->chars + x->length;

	if (!isValid(x, &num, &base, false, vm)) {
		if (err != NULL)
			err->fields[0].uint = 1;
		vm->stack[vm->stackTop++] = CarbonUInt(0);
		return NULL;
	}

	uint64_t returnValue = strtol(num, &last, base);
	vm->stack[vm->stackTop++] = CarbonUInt(returnValue);

	return NULL;
}

char *mS2I(CarbonObj *parent, CarbonValue *args, CarbonVM *vm) {
	CarbonString *x = (CarbonString *) args[0].obj;
	CarbonInstance *err = (CarbonInstance *) args[1].obj;

	uint32_t base;
	char *num;
	char *last = x->chars + x->length;

	if (!isValid(x, &num, &base, true, vm)) {
		if (err != NULL)
			err->fields[0].uint = 1;
		vm->stack[vm->stackTop++] = CarbonUInt(0);
		return NULL;
	}

	int64_t returnValue = strtol(num, &last, base);
	if (*x->chars == '-')
		returnValue = -returnValue;
	vm->stack[vm->stackTop++] = CarbonInt(returnValue);

	return NULL;
}

char *mS2D(CarbonObj *parent, CarbonValue *args, CarbonVM *vm) {
	CarbonString *x = (CarbonString *) args[0].obj;
	CarbonInstance *err = (CarbonInstance *) args[1].obj;

	char *first = x->chars;
	char *last;
	double v = strtod(first, &last);
	if (last != x->chars + x->length) {
		if (err != NULL)
			err->fields[0].uint = 1;
		vm->stack[vm->stackTop++] = CarbonDouble(0);
		return NULL;
	}
	vm->stack[vm->stackTop++] = CarbonDouble(v);
	return NULL;
}

CarbonModule *carbon_initModuleString(CarbonVM *vm) {
#define str(v) carbon_copyString(v, strlen(v), vm)
#define func(v, s) carbon_newBuiltin(NULL, v, s, vm)

	CarbonModule *mod = carbon_reallocate(0, sizeof(CarbonModule), NULL);
	mod->funcs =
		carbon_reallocate(0, sizeof(CarbonModuleElement) * members, NULL);
	mod->count = members;
	mod->src = src;
	mod->srclen = strlen(src);

	CarbonFunctionSignature *s2u = sigS2X(vm, ValueUInt);
	CarbonFunctionSignature *s2i = sigS2X(vm, ValueInt);
	CarbonFunctionSignature *s2d = sigS2X(vm, ValueDouble);

	CarbonFunctionSignature *d2s = sigX2S(vm, ValueDouble);
	CarbonFunctionSignature *i2s = sigX2S(vm, ValueInt);

	mod->funcs[0] = (CarbonModuleElement){str("d2s"), func(mD2S, d2s)};
	mod->funcs[1] = (CarbonModuleElement){str("i2s"), func(mI2S, i2s)};
	mod->funcs[2] = (CarbonModuleElement){str("u2s"), func(mU2S, i2s)};
	mod->funcs[3] = (CarbonModuleElement){str("s2u"), func(mS2U, s2u)};
	mod->funcs[4] = (CarbonModuleElement){str("s2i"), func(mS2I, s2i)};
	mod->funcs[5] = (CarbonModuleElement){str("s2d"), func(mS2D, s2d)};

	return mod;
}
#endif

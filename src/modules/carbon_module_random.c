#include "carbon_modules.h"

#ifdef CARBON_MODULE_RANDOM

#include "carbon_value.h"
#include "utils/carbon_commons.h"
#include "utils/carbon_memory.h"
#include "carbon_object.h"
#include "vm/carbon_vm.h"
#include <string.h>
#include <stdlib.h>

static const uint32_t members = 3;

CarbonFunctionSignature *makeRandomSig(CarbonVM *vm) {
	CarbonFunctionSignature *sig =
		carbon_reallocate(0, sizeof(CarbonFunctionSignature), NULL);
	sig->arity = 0;
	sig->returnType = carbon_reallocate(0, sizeof(CarbonValueType), NULL);
	sig->returnType->tag = ValueDouble;
	sig->returnType->compound.instanceName = NULL;
	sig->arguments = NULL;

	CarbonValueType t = {.tag = ValueFunction, .compound = {.signature = sig}};
	carbon_pushType(vm, t);
	return sig;
}
CarbonFunctionSignature *makeRandintSig(CarbonVM *vm) {
	CarbonFunctionSignature *sig =
		carbon_reallocate(0, sizeof(CarbonFunctionSignature), NULL);
	sig->arity = 2;
	sig->returnType = carbon_reallocate(0, sizeof(CarbonValueType), NULL);
	sig->returnType->tag = ValueInt;
	sig->returnType->compound.instanceName = NULL;

	sig->arguments = carbon_reallocate(0, sizeof(CarbonValueType) * 2, NULL);

	sig->arguments[0].compound.instanceName = NULL;
	sig->arguments[0].tag = ValueInt;

	sig->arguments[1].compound.instanceName = NULL;
	sig->arguments[1].tag = ValueInt;
	CarbonValueType t = {.tag = ValueFunction, .compound = {.signature = sig}};
	carbon_pushType(vm, t);
	return sig;
}

CarbonFunctionSignature *makeRandSeed(CarbonVM *vm) {
	CarbonFunctionSignature *sig =
		carbon_reallocate(0, sizeof(CarbonFunctionSignature), NULL);
	sig->arity = 1;
	sig->returnType = carbon_reallocate(0, sizeof(CarbonValueType), NULL);
	sig->returnType->tag = ValueVoid;
	sig->returnType->compound.instanceName = NULL;

	sig->arguments = carbon_reallocate(0, sizeof(CarbonValueType), NULL);

	sig->arguments[0].compound.instanceName = NULL;
	sig->arguments[0].tag = ValueUInt;

	CarbonValueType t = {.tag = ValueFunction, .compound = {.signature = sig}};
	carbon_pushType(vm, t);
	return sig;
}

char *mRandSeed(CarbonObj *parent, CarbonValue *args, CarbonVM *vm) {
	srand(args[0].uint);
	return NULL;
}
char *mRandom(CarbonObj *parent, CarbonValue *args, CarbonVM *vm) {
	vm->stack[vm->stackTop++] = CarbonDouble((double) rand() / RAND_MAX);
	return NULL;
}
char *mRandint(CarbonObj *parent, CarbonValue *args, CarbonVM *vm) {
	int64_t a = args[0].sint;
	int64_t b = args[1].sint;
	int num = (rand() % (a - b + 1)) + a;
	vm->stack[vm->stackTop++] = CarbonInt(num);
	return NULL;
}

CarbonModule *carbon_initModuleRandom(CarbonVM *vm) {
#define str(v) carbon_copyString(v, strlen(v), vm)
#define func(v, s) carbon_newBuiltin(NULL, v, s, vm)

	CarbonModule *mod = carbon_reallocate(0, sizeof(CarbonModule), NULL);
	mod->funcs =
		carbon_reallocate(0, sizeof(CarbonModuleElement) * members, NULL);
	mod->count = members;
	mod->src = NULL;
	mod->srclen = 0;

	CarbonFunctionSignature *random = makeRandomSig(vm);
	CarbonFunctionSignature *randint = makeRandintSig(vm);
	CarbonFunctionSignature *seed = makeRandSeed(vm);

	mod->funcs[0] = (CarbonModuleElement){str("random"), func(mRandom, random)};
	mod->funcs[1] =
		(CarbonModuleElement){str("randint"), func(mRandint, randint)};
	mod->funcs[2] =
		(CarbonModuleElement){str("randseed"), func(mRandSeed, seed)};

	return mod;
}
#endif

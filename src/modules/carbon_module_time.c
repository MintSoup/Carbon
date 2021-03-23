#include "carbon_modules.h"

#ifdef CARBON_MODULE_TIME

#include "carbon_value.h"
#include "utils/carbon_commons.h"
#include "utils/carbon_memory.h"
#include "carbon_object.h"
#include "vm/carbon_vm.h"
#include <string.h>
#include <time.h>

static const uint32_t members = 3;

CarbonFunctionSignature *makeSig(CarbonVM *vm) {
	CarbonFunctionSignature *sig =
		carbon_reallocate(0, sizeof(CarbonFunctionSignature), NULL);
	sig->arity = 0;
	sig->returnType = carbon_reallocate(0, sizeof(CarbonValueType), NULL);
	sig->returnType->tag = ValueUInt;
	sig->returnType->compound.instanceName = NULL;
	sig->arguments = NULL;
	CarbonValueType t = {.tag = ValueFunction, .compound = {.signature = sig}};
	carbon_pushType(vm, t);
	return sig;
}

char *mRawClock(CarbonObj *parent, CarbonValue *args, CarbonVM *vm) {
	vm->stack[vm->stackTop++] = CarbonUInt(clock());
	return NULL;
}
char *mClock(CarbonObj *parent, CarbonValue *args, CarbonVM *vm) {
	vm->stack[vm->stackTop++] =
		CarbonUInt(clock() * 1000000 / CLOCKS_PER_SEC);
	return NULL;
}
char *mTime(CarbonObj *parent, CarbonValue *args, CarbonVM *vm) {
	vm->stack[vm->stackTop++] = CarbonUInt(time(NULL));
	return NULL;
}

CarbonModule *carbon_initModuleTime(CarbonVM *vm) {
#define str(v) carbon_copyString(v, strlen(v), vm)
#define func(v, s) carbon_newBuiltin(NULL, v, s, vm)

	CarbonModule *mod = carbon_reallocate(0, sizeof(CarbonModule), NULL);
	mod->funcs =
		carbon_reallocate(0, sizeof(CarbonModuleElement) * members, NULL);
	mod->count = members;
	mod->src = NULL;
	mod->srclen = 0;

	CarbonFunctionSignature *sig = makeSig(vm);

	mod->funcs[0] = (CarbonModuleElement){str("time"), func(mTime, sig)};
	mod->funcs[1] =
		(CarbonModuleElement){str("rawclock"), func(mRawClock, sig)};
	mod->funcs[2] = (CarbonModuleElement){str("clock"), func(mClock, sig)};

	return mod;
}
#endif

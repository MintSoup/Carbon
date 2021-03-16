#include "carbon_modules.h"

#ifdef CARBON_MODULE_MATH

#include "carbon_value.h"
#include "utils/carbon_commons.h"
#include "utils/carbon_memory.h"
#include "carbon_object.h"
#include "vm/carbon_vm.h"
#include <math.h>
#include <string.h>

static const uint32_t members = 15;
static const double pi = 3.14159265358979323846;
static char *src = "\
double M_PI = 3.14159265358979323846;\
double M_E = 2.71828182845904523536;\
";

CarbonFunctionSignature *makeD2D(CarbonVM *vm) {
	CarbonFunctionSignature *sig =
		carbon_reallocate(0, sizeof(CarbonFunctionSignature), NULL);
	sig->arity = 1;
	sig->returnType = carbon_reallocate(0, sizeof(CarbonValueType), NULL);
	sig->returnType->tag = ValueDouble;
	sig->returnType->compound.instanceName = NULL;
	sig->arguments = carbon_reallocate(0, sizeof(CarbonValueType), NULL);
	sig->arguments[0].compound.instanceName = NULL;
	sig->arguments[0].tag = ValueDouble;

	CarbonValueType t = {.tag = ValueFunction, .compound = {.signature = sig}};
	carbon_pushType(vm, t);
	return sig;
}
CarbonFunctionSignature *make2D2D(CarbonVM *vm) {
	CarbonFunctionSignature *sig =
		carbon_reallocate(0, sizeof(CarbonFunctionSignature), NULL);
	sig->arity = 2;
	sig->returnType = carbon_reallocate(0, sizeof(CarbonValueType), NULL);
	sig->returnType->tag = ValueDouble;
	sig->returnType->compound.instanceName = NULL;

	sig->arguments = carbon_reallocate(0, sizeof(CarbonValueType) * 2, NULL);

	sig->arguments[0].compound.instanceName = NULL;
	sig->arguments[0].tag = ValueDouble;

	sig->arguments[1].compound.instanceName = NULL;
	sig->arguments[1].tag = ValueDouble;

	CarbonValueType t = {.tag = ValueFunction, .compound = {.signature = sig}};
	carbon_pushType(vm, t);
	return sig;
}

char *mSin(CarbonObj *parent, CarbonValue *args, CarbonVM *vm) {
	double val = args[0].dbl;
	vm->stack[vm->stackTop++] = CarbonDouble(sin(val));
	return NULL;
}
char *mCos(CarbonObj *parent, CarbonValue *args, CarbonVM *vm) {
	double val = args[0].dbl;
	vm->stack[vm->stackTop++] = CarbonDouble(cos(val));
	return NULL;
}
char *mTan(CarbonObj *parent, CarbonValue *args, CarbonVM *vm) {
	double val = args[0].dbl;
	vm->stack[vm->stackTop++] = CarbonDouble(tan(val));
	return NULL;
}
char *mAsin(CarbonObj *parent, CarbonValue *args, CarbonVM *vm) {
	double val = args[0].dbl;
	vm->stack[vm->stackTop++] = CarbonDouble(asin(val));
	return NULL;
}
char *mAcos(CarbonObj *parent, CarbonValue *args, CarbonVM *vm) {
	double val = args[0].dbl;
	vm->stack[vm->stackTop++] = CarbonDouble(acos(val));
	return NULL;
}
char *mAtan(CarbonObj *parent, CarbonValue *args, CarbonVM *vm) {
	double val = args[0].dbl;
	vm->stack[vm->stackTop++] = CarbonDouble(atan(val));
	return NULL;
}
char *mDeg(CarbonObj *parent, CarbonValue *args, CarbonVM *vm) {
	double val = args[0].dbl;
	vm->stack[vm->stackTop++] = CarbonDouble(val * 180 / pi);
	return NULL;
}
char *mRad(CarbonObj *parent, CarbonValue *args, CarbonVM *vm) {
	double val = args[0].dbl;
	vm->stack[vm->stackTop++] = CarbonDouble(val * pi / 180);
	return NULL;
}
char *mLn(CarbonObj *parent, CarbonValue *args, CarbonVM *vm) {
	double val = args[0].dbl;
	vm->stack[vm->stackTop++] = CarbonDouble(log(val));
	return NULL;
}
char *mLog2(CarbonObj *parent, CarbonValue *args, CarbonVM *vm) {
	double val = args[0].dbl;
	vm->stack[vm->stackTop++] = CarbonDouble(log2(val));
	return NULL;
}
char *mLog10(CarbonObj *parent, CarbonValue *args, CarbonVM *vm) {
	double val = args[0].dbl;
	vm->stack[vm->stackTop++] = CarbonDouble(log10(val));
	return NULL;
}
char *mSqrt(CarbonObj *parent, CarbonValue *args, CarbonVM *vm) {
	double val = args[0].dbl;
	vm->stack[vm->stackTop++] = CarbonDouble(sqrt(val));
	return NULL;
}
char *mCbrt(CarbonObj *parent, CarbonValue *args, CarbonVM *vm) {
	double val = args[0].dbl;
	vm->stack[vm->stackTop++] = CarbonDouble(cbrt(val));
	return NULL;
}
char *mPow(CarbonObj *parent, CarbonValue *args, CarbonVM *vm) {
	double a = args[0].dbl;
	double b = args[1].dbl;
	vm->stack[vm->stackTop++] = CarbonDouble(pow(a, b));
	return NULL;
}
char *mAtan2(CarbonObj *parent, CarbonValue *args, CarbonVM *vm) {
	double a = args[0].dbl;
	double b = args[1].dbl;
	vm->stack[vm->stackTop++] = CarbonDouble(atan2(a, b));
	return NULL;
}

CarbonModule *carbon_initModuleMath(CarbonVM *vm) {
#define str(v) carbon_copyString(v, strlen(v), vm)
#define func(v, s) carbon_newBuiltin(NULL, v, s, vm)

	CarbonModule *mod = carbon_reallocate(0, sizeof(CarbonModule), NULL);
	mod->funcs =
		carbon_reallocate(0, sizeof(CarbonModuleElement) * members, NULL);
	mod->count = members;

	mod->src = src;
	mod->srclen = strlen(src);

	CarbonFunctionSignature *d2d = makeD2D(vm);
	CarbonFunctionSignature *sig2d2d = make2D2D(vm);

	// d2d
	mod->funcs[0] = (CarbonModuleElement){str("sin"), func(mSin, d2d)};
	mod->funcs[1] = (CarbonModuleElement){str("cos"), func(mCos, d2d)};
	mod->funcs[2] = (CarbonModuleElement){str("tan"), func(mTan, d2d)};
	mod->funcs[3] = (CarbonModuleElement){str("asin"), func(mAsin, d2d)};
	mod->funcs[4] = (CarbonModuleElement){str("acos"), func(mAcos, d2d)};
	mod->funcs[5] = (CarbonModuleElement){str("atan"), func(mAtan, d2d)};
	mod->funcs[6] = (CarbonModuleElement){str("deg"), func(mDeg, d2d)};
	mod->funcs[7] = (CarbonModuleElement){str("rad"), func(mRad, d2d)};
	mod->funcs[8] = (CarbonModuleElement){str("ln"), func(mLn, d2d)};
	mod->funcs[9] = (CarbonModuleElement){str("log2"), func(mLog2, d2d)};
	mod->funcs[10] = (CarbonModuleElement){str("log10"), func(mLog10, d2d)};
	mod->funcs[11] = (CarbonModuleElement){str("sqrt"), func(mSqrt, d2d)};
	mod->funcs[12] = (CarbonModuleElement){str("cbrt"), func(mCbrt, d2d)};
	mod->funcs[13] = (CarbonModuleElement){str("pow"), func(mPow, sig2d2d)};
	mod->funcs[14] = (CarbonModuleElement){str("atan2"), func(mAtan2, sig2d2d)};

	return mod;
}
#endif

#pragma once

#include "carbon_object.h"
#include "carbon_value.h"
#include "vm/carbon_vm.h"

typedef struct {
	CarbonString *name;
	CarbonBuiltin *func;
} CarbonModuleElement;

typedef struct {
	char *src;
	CarbonModuleElement *funcs;
	uint32_t count;
	uint32_t srclen;
} CarbonModule;

typedef struct {
	char *name;
	CarbonModule *(*generator)(CarbonVM *vm);
} CarbonModuleHandle;

extern CarbonModuleHandle const carbon_modules[];
extern const uint32_t carbon_modules_count;

#define CARBON_MODULE_MATH
#define CARBON_MODULE_RANDOM
#define CARBON_MODULE_TIME
#define CARBON_MODULE_ERROR
#define CARBON_MODULE_STRING

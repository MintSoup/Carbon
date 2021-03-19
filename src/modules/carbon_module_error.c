#include "carbon_modules.h"

#ifdef CARBON_MODULE_ERROR

#include "carbon_value.h"
#include "utils/carbon_commons.h"
#include "utils/carbon_memory.h"
#include "carbon_object.h"
#include "vm/carbon_vm.h"
#include <string.h>
#include <time.h>

static const uint32_t members = 0;
static char *src = "class @Error: int code; string msg; end";

CarbonModule *carbon_initModuleError(CarbonVM *vm) {

	CarbonModule *mod = carbon_reallocate(0, sizeof(CarbonModule), NULL);
	mod->funcs = NULL;
	mod->count = members;
	mod->src = src;
	mod->srclen = strlen(src);

	return mod;
}
#endif

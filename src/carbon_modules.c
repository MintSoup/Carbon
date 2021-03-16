#include "carbon_modules.h"

#include "modules/carbon_module_math.h"
#include "modules/carbon_module_random.h"
#include "modules/carbon_module_time.h"

// clang-format off
CarbonModuleHandle const carbon_modules[] = {
#ifdef CARBON_MODULE_MATH
	{"math", carbon_initModuleMath},
#endif
#ifdef CARBON_MODULE_RANDOM
	{"random", carbon_initModuleRandom},
#endif
#ifdef CARBON_MODULE_RANDOM
	{"time", carbon_initModuleTime},
#endif
};
// clang-format on
const uint32_t carbon_modules_count =
	sizeof(carbon_modules) / sizeof(*carbon_modules);

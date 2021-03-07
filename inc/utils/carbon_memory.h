#pragma once
#include "utils/carbon_commons.h"
#include "vm/carbon_vm.h"
void *carbon_reallocate(uint32_t oldSize, uint32_t newSize, void *oldptr);
void carbon_gc(CarbonVM *vm);

#pragma once

#include "ast/carbon_expressions.h"
#include "vm/carbon_vm.h"

void carbon_compile(CarbonExpr* expr, CarbonVM* vm);

#pragma once

#include "ast/carbon_expressions.h"
#include "vm/carbon_vm.h"

void carbon_compileExpression(CarbonExpr *expr, CarbonChunk *chunk);

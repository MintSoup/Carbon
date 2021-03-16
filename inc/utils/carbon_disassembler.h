#pragma once

#include "vm/carbon_chunk.h"
#include "vm/carbon_vm.h"
#include <stdio.h>

void carbon_disassemble(CarbonChunk* chunk, CarbonVM* vm);
void carbon_printType(FILE *f, CarbonValueType type);

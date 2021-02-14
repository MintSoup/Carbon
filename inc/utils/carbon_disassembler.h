#pragma once

#include "vm/carbon_chunk.h"
#include <stdio.h>

void carbon_disassemble(CarbonChunk* chunk);
void carbon_printType(FILE *f, CarbonValueType type);

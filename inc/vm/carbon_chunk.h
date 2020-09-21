#pragma once

#include "utils/carbon_commons.h"

typedef enum { OP_RETURN } OpCode;

typedef struct {
	uint8_t *code;
	uint32_t* lines;
	uint32_t capacity;
	uint32_t count;
} CarbonChunk;

void carbon_initChunk(CarbonChunk *chunk);
void carbon_writeToChunk(CarbonChunk *chunk, uint8_t data, uint32_t line);
void carbon_freeChunk(CarbonChunk *chunk);

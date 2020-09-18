#pragma once

#include "utils/carbon_commons.h"

typedef enum { OP_RETURN } OpCode;

typedef struct {
	uint8_t *code;
	uint32_t* lines;
	uint32_t capacity;
	uint32_t count;
} Chunk;

void carbon_initChunk(Chunk *chunk);
void carbon_writeToChunk(Chunk *chunk, uint8_t data, uint32_t line);
void carbon_freeChunk(Chunk* chunk);

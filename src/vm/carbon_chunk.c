#include "vm/carbon_chunk.h"
#include "carbon_value.h"
#include "utils/carbon_commons.h"
#include "utils/carbon_memory.h"

static inline void growChunk(CarbonChunk *chunk) {
	uint32_t oldSize = chunk->capacity;
	uint32_t newSize = 8;
	if (oldSize != 0)
		newSize = oldSize * 2;
	chunk->code = carbon_reallocate(oldSize, newSize, chunk->code);
	chunk->lines = carbon_reallocate(oldSize * sizeof(uint32_t),
									 newSize * sizeof(uint32_t), chunk->lines);
	chunk->capacity = newSize;
}

void carbon_initChunk(CarbonChunk *chunk) {
	chunk->code = NULL;
	chunk->lines = NULL;
	chunk->count = 0;
	chunk->capacity = 0;
	carbon_initValueArray(&chunk->constants);
}

void carbon_writeToChunk(CarbonChunk *chunk, uint8_t data, uint32_t line) {
	if (chunk->capacity <= chunk->count) {
		growChunk(chunk);
	}
	chunk->code[chunk->count] = data;
	chunk->lines[chunk->count] = line;
	chunk->count++;
}
void carbon_freeChunk(CarbonChunk *chunk) {
	carbon_reallocate(chunk->capacity, 0, chunk->code);
	carbon_reallocate(chunk->capacity * sizeof(uint32_t), 0, chunk->lines);
	carbon_freeCarbonValueArray(&chunk->constants);
	carbon_initChunk(chunk);
}
uint16_t carbon_addConstant(CarbonChunk *chunk, CarbonValue constant) {
	return carbon_writeToValueArray(&chunk->constants, constant);
}

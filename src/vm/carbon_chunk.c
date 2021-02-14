#include "vm/carbon_chunk.h"
#include "ast/carbon_expressions.h"
#include "carbon_object.h"
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
	chunk->typeData = NULL;
	chunk->count = 0;
	chunk->capacity = 0;
	chunk->typeCount = 0;
	chunk->typeCapacity = 0;
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
	for (uint16_t i = 0; i < chunk->typeCount; i++) {
		carbon_freeType(chunk->typeData[i]);
	}
	carbon_reallocate(chunk->typeCapacity * sizeof(CarbonValueType), 0,
					  chunk->typeData);
	carbon_freeCarbonValueArray(&chunk->constants);
	carbon_initChunk(chunk);
}
uint16_t carbon_pushType(CarbonChunk *chunk, CarbonValueType type) {
	if (chunk->typeCapacity <= chunk->typeCount) {
		uint32_t oldSize = chunk->typeCount;
		uint32_t newSize = 8;
		if (oldSize != 0)
			newSize = oldSize * 2;
		chunk->typeData = carbon_reallocate(oldSize * sizeof(CarbonValueType),
											newSize * sizeof(CarbonValueType),
											chunk->typeData);
		chunk->typeCapacity = newSize;
	}
	chunk->typeData[chunk->typeCount] = type;
	return chunk->typeCount++;
}

uint16_t carbon_addConstant(CarbonChunk *chunk, CarbonValue constant) {
	for (uint32_t i = 0; i < chunk->constants.count; i++) {
		if (chunk->constants.arr[i].uint == constant.uint)
			return i;
	}
	return carbon_writeToValueArray(&chunk->constants, constant);
}

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
	chunk->capacity = newSize;
}

static inline void growLines(CarbonChunk *chunk) {
	uint32_t old = chunk->lineCapacity;
	uint32_t new = 8;
	if (old != 0)
		new = old * 1.5;
	chunk->lines = carbon_reallocate(old * sizeof(struct carbon_lineInfo),
					  new * sizeof(struct carbon_lineInfo), chunk->lines);
	chunk->lineCapacity = new;
}

void carbon_initChunk(CarbonChunk *chunk) {
	chunk->code = NULL;
	chunk->lines = NULL;
	chunk->count = 0;
	chunk->capacity = 0;
	chunk->lineCount = 0;
	chunk->lineCapacity = 0;
	carbon_initValueArray(&chunk->constants);
}

void carbon_writeToChunk(CarbonChunk *chunk, uint8_t data, uint32_t line) {
	if (chunk->capacity <= chunk->count)
		growChunk(chunk);

	chunk->code[chunk->count] = data;
	chunk->count++;

	if (chunk->lineCount > 0 &&
		chunk->lines[chunk->lineCount - 1].line == line &&
		chunk->lines[chunk->lineCount - 1].line != UINT32_MAX) {
		chunk->lines[chunk->lineCount - 1].count++;
		return;
	}

	if (chunk->lineCapacity < chunk->lineCount + 1)
		growLines(chunk);

	struct carbon_lineInfo *i = &chunk->lines[chunk->lineCount++];
	i->line = line;
	i->count = 0;
}
void carbon_freeChunk(CarbonChunk *chunk) {
	carbon_reallocate(chunk->capacity, 0, chunk->code);
	carbon_reallocate(chunk->lineCapacity * sizeof(struct carbon_lineInfo), 0,
					  chunk->lines);
	carbon_freeCarbonValueArray(&chunk->constants);
	carbon_initChunk(chunk);
}

uint32_t carbon_getLine(CarbonChunk *c, uint32_t n) {
	uint32_t offset = 0;
	for (uint32_t i = 0; i < c->lineCount; i++) {
		struct carbon_lineInfo line = c->lines[i];
		offset += line.count + 1;
		if(n < offset)
			return line.line;
	}
	return -1;
}

uint16_t carbon_addConstant(CarbonChunk *chunk, CarbonValue constant, bool primitive){
	for (uint32_t i = 0; i < chunk->constants.count; i++) {
		if (chunk->constants.arr[i].uint == constant.uint)
			return i;
	}
	return carbon_writeToValueArray(&chunk->constants, constant, primitive);
}

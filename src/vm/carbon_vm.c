#include "vm/carbon_vm.h"
#include "vm/carbon_chunk.h"

void carbon_initVM(CarbonVM *vm){
	vm->stackTop = 0;
	carbon_initChunk(&vm->chunk);
}
void carbon_freeVM(CarbonVM *vm){
	carbon_freeChunk(&vm->chunk);
}


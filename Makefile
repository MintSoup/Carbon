CC := gcc
name := carbon

src := src
inc := inc
obj := obj

srcf := $(shell find ./$(src) -type f -name "*.c")
objf := $(patsubst %.c,%.o,$(srcf))
objf := $(patsubst ./$(src)/%,./$(obj)/%,$(objf))

objdirs = $(dir $(objf))

executable := $(name)

flags := -O3 -Wall -std=c99

debugflags := -g -O0 -Wall -std=c99 -DDebug


$(obj)/%.o: $(src)/%.c
	@mkdir -p $(objdirs)
	$(CC) $(flags) -I$(inc) -c -o $@ $<
	
$(executable): $(objf)
	$(CC) $(objf) -o $(executable)

build: $(executable)

debug: flags = $(debugflags)
debug: $(executable)

run_new: $(executable)
	st sh -c './$(executable) --disassemble test.cbn; pause'

run: $(executable)
	./$(executable)

$(obj): 
	mkdir -p $(obj)

clean:
	rm -f $(objf) $(executable)

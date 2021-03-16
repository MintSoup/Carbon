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

ldflags := -lm

build: $(executable)

$(obj)/%.o: $(src)/%.c
	@mkdir -p $(objdirs)
	$(CC) $(flags) -I$(inc) -c -o $@ $<

$(executable): $(objf)
	$(CC) $(ldflags) $(objf) -o $(executable)

debug: flags = $(debugflags)
debug: $(executable)

run_new: $(executable)
	st bash -c './$(executable) --disassemble test.cbn; read -n1' # using bash to see segfaults

run: $(executable)
	./$(executable)

$(obj): 
	mkdir -p $(obj)

clean:
	rm -f $(objf) $(executable)

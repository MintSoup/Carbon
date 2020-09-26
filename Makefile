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

flags := -O3 -Wall

debugflags := -g -O0 -Wall -DDebug 


$(obj)/%.o: $(src)/%.c $(obj)
	mkdir -p $(objdirs)
	$(CC) $(flags) -I$(inc) -c -o $@ $<
	
$(executable): $(objf)
	$(CC) $(objf) -o $(executable)

build: $(executable)

debug: flags = $(debugflags)
debug: $(executable)

run_new: $(executable)
	st sh -c './$(executable) test.cbn; pause'

run: $(executable)
	./$(executable)

$(obj): 
	mkdir -p $(obj)

clean:
	rm -f $(objf) $(executable)

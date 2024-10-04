include ../../../Makefile.config
include Makefile.objects

FLAGS=$(CFLAGS)

compile: c.exe

c.exe: $(OBJECTS)
	$(CC) $(FLAGS) $(OBJECTS) -o $@ -lm

clean:
	-@/bin/rm -f c.exe 2> /dev/null
	-@/bin/rm -rf $(OBJECTS) 2> /dev/null
	-@/bin/rm -f .afile 2> /dev/null

.SUFFIXES:
.SUFFIXES: .c .o

.c.o:
	$(CC) $(FLAGS) $*.c -o $*.o -c

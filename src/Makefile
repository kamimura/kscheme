P=ksi
OBJECTS =\
	parse.tab.o\
	lex.yy.o\
	object.o \
	memory.o\
	environment.o\
	pair.o\
	identifier.o\
	numberz.o\
	numberq.o\
	numberr.o\
	numberc.o\
	character.o\
	procedures.o

CFLAGS=\
	-DYYDEBUG=1\
	-g -Wall -Werror -O0\
	`pkg-config --cflags glib-2.0`
LDLIBS=\
	`pkg-config --libs glib-2.0`\
	-L/opt/local/lib -lgmp -lmpfr -lmpc -lfl

CC = cc
# CC = clang-mp-3.7
# CC = gcc-mp-5

LEX = flex
YACC = bison -dv

default: $(P)
	./ksi

$(P): $(OBJECTS) main.c
	$(CC) $(CFLAGS) $(LDLIBS) $(OBJECTS) main.c -o $@

parse.tab.c parse.tab.h: parse.y
	$(YACC) parse.y

lex.yy.c: lex.l parse.tab.h
	$(LEX) lex.l

object.o: object.c object.h
	$(CC) $(CFLAGS) -c object.c

memory.o: memory.c memory.h object.h
	$(CC) $(CFLAGS) -c memory.c

environment.o: environment.c environment.h object.h
	$(CC) $(CFLAGS) -c environment.c

pair.o: pair.c pair.h object.h
	$(CC) $(CFLAGS) -c pair.c

identifier.o: identifier.c identifier.h object.h
	$(CC) $(CFLAGS) -c identifier.c

numberz.o: numberz.c numberz.h object.h
	$(CC) $(CFLAGS) -c numberz.c

numberq.o: numberq.c numberq.h object.h
	$(CC) $(CFLAGS) -c numberq.c

numberr.o: numberr.c numberr.h object.h
	$(CC) $(CFLAGS) -c numberr.c

numberc.o: numberc.c numberc.h object.h
	$(CC) $(CFLAGS) -c numberc.c

character.o: character.c character.h object.h
	$(CC) $(CFLAGS) -c character.c

procedures.o: procedures.c procedures.h object.h
	$(CC) $(CFLAGS) -c procedures.c

fmt:
	clang-format-mp-3.7 -i *.[hc] && gmake

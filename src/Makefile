MLCOMPILER=mosmlc -c -liberal
MLLINKER=mosmlc
MLLEX=mosmllex
MLYACC=mosmlyac -v
MLDEP=camlrunm /usr/share/mosml/tools/mosmldep

SRCS:= $(wildcard *.sml) parser.sml lexer.sml
OBJS= $(SRCS:%.sml=%.uo)

.PHONY: all clean depend

all: tiger libtigerruntime.a

clean:
	$(RM) *.uo
	$(RM) *.ui
	$(RM) parser.sml
	$(RM) parser.sig
	$(RM) parser.output
	$(RM) lexer.sml
	$(RM) tiger

tiger: $(OBJS)
	$(MLLINKER) -o $@ main.uo

parser.sml parser.sig: parser.y
	$(MLYACC) $<

lexer.sml: lexer.lex
	$(MLLEX) $<

%.ui: %.sig
	$(MLCOMPILER) $<

%.uo: %.sml
	$(MLCOMPILER) $<

%.o: %.c
	$(CC) -o $@ -c $<

lib%.a: %.o
	ar rcs $@ $<

depend: .depend
.depend: $(SRCS)
	@$(MLDEP) > .depend

-include .depend

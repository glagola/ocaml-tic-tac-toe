FIND=ocamlfind
COMPILER=ocamlc
NCOMPILER=ocamlopt

MAIN=main.ml

FILES=$(shell find . -type f -name "*.ml" ! -name "$(MAIN)"; find . -type f -name "$(MAIN)")

RESULT_NAME=server
OPTIONS=-syntax camlp4o -package lwt.syntax,lwt.unix -linkpkg $(FILES) -o $(RESULT_NAME)


all: build clean

build: 
	make native || make bytecode

native: clean_build
	$(FIND) $(NCOMPILER) $(OPTIONS) 

bytecode: clean_build
	$(FIND) $(COMPILER) $(OPTIONS)

clean_build:
	rm -f $(RESULT_NAME)

clean:
	rm -f *.cmi *.cmo *.o *.cmx

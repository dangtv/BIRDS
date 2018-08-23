PACKAGES=str

# create directory if not exist
DIR_GUARD=@mkdir -p $(@D)

#Name of the final executable file to be generated
EX_NAME=birds

# binary folder for compilation
BIN_DIR=bin
RELEASE_DIR = release

OCAMLC_FLAGS=-I $(BIN_DIR)
OCAMLOPT_FLAGS=-I $(RELEASE_DIR)

#Name of the files that are part of the project
MAIN_FILE=main
FILES=\
    expr\
	utils\
    parser\
    lexer\
	derivation\
	ast2sql\

.PHONY: all release clean
all: $(BIN_DIR)/$(EX_NAME)

#The next variables hold the dependencies of each file
# DEP_lexer=parser 
# DEP_utils=expr 

#Rule for generating the final executable file
$(BIN_DIR)/$(EX_NAME): $(FILES:%=$(BIN_DIR)/%.cmo) $(BIN_DIR)/$(MAIN_FILE).cmo
	$(DIR_GUARD)
	ocamlfind ocamlc $(OCAMLC_FLAGS) -package $(PACKAGES) -thread -linkpkg $(FILES:%=$(BIN_DIR)/%.cmo) $(BIN_DIR)/$(MAIN_FILE).cmo -o $(BIN_DIR)/$(EX_NAME)

#Rule for compiling the main file
$(BIN_DIR)/$(MAIN_FILE).cmo: $(FILES:%=$(BIN_DIR)/%.cmo) src/$(MAIN_FILE).ml
	$(DIR_GUARD)
	ocamlfind ocamlc $(OCAMLC_FLAGS) -package $(PACKAGES) -thread -o $(BIN_DIR)/$(MAIN_FILE) -c src/$(MAIN_FILE).ml 

#Special rule for compiling conn_ops
$(BIN_DIR)/conn_ops.cmo $(BIN_DIR)/conn_ops.cmi: src/conn_ops.ml
	$(DIR_GUARD)
	ocamlfind ocamlc $(OCAMLC_FLAGS) -package $(PACKAGES) -thread -o $(BIN_DIR)/conn_ops -c $<

#Special rules for creating the lexer and parser
src/parser.ml src/parser.mli: src/parser.mly
	ocamlyacc $<
$(BIN_DIR)/parser.cmi:	src/parser.mli
	$(DIR_GUARD)
	ocamlc $(OCAMLC_FLAGS) -o $(BIN_DIR)/parser -c $<
$(BIN_DIR)/parser.cmo:	src/parser.ml $(BIN_DIR)/parser.cmi
	$(DIR_GUARD)
	ocamlc $(OCAMLC_FLAGS) -o $(BIN_DIR)/parser -c $<
src/lexer.ml:	src/lexer.mll
	ocamllex $<

#General rule for compiling
$(BIN_DIR)/%.cmi $(BIN_DIR)/%.cmo: src/%.ml
	$(DIR_GUARD)
	ocamlc $(OCAMLC_FLAGS) -o $(BIN_DIR)/$* -c $<

#Dependencies for binary files:
# $(BIN_DIR)/lexer.cmo: $(DEP_lexer:%=$(BIN_DIR)/%.cmo)
# $(BIN_DIR)/utils.cmo: $(DEP_utils:%=$(BIN_DIR)/%.cmo)

clean:
	rm -f $(BIN_DIR)/*.cmo $(BIN_DIR)/*.cmi $(BIN_DIR)/*.o src/parser.mli src/parser.ml src/lexer.ml 

release: $(RELEASE_DIR)/$(EX_NAME)

#The next variables hold the dependencies of each file
# DEP_lexer=parser 
# DEP_utils=expr 

#Rule for generating the final executable file
$(RELEASE_DIR)/$(EX_NAME): $(FILES:%=$(RELEASE_DIR)/%.cmx) $(RELEASE_DIR)/$(MAIN_FILE).cmx
	$(DIR_GUARD)
	ocamlfind ocamlopt $(OCAMLOPT_FLAGS) -package $(PACKAGES) -thread -linkpkg $(FILES:%=$(RELEASE_DIR)/%.cmx) $(RELEASE_DIR)/$(MAIN_FILE).cmx -o $(RELEASE_DIR)/$(EX_NAME)
	rm -f $(RELEASE_DIR)/*.cmx $(RELEASE_DIR)/*.cmi $(RELEASE_DIR)/*.o 

#Rule for compiling the main file
$(RELEASE_DIR)/$(MAIN_FILE).cmx: $(FILES:%=$(RELEASE_DIR)/%.cmx) src/$(MAIN_FILE).ml
	$(DIR_GUARD)
	ocamlfind ocamlopt $(OCAMLOPT_FLAGS) -package $(PACKAGES) -thread -o $(RELEASE_DIR)/$(MAIN_FILE) -c src/$(MAIN_FILE).ml 

#Special rule for compiling conn_ops
$(RELEASE_DIR)/conn_ops.cmx $(RELEASE_DIR)/conn_ops.cmi: src/conn_ops.ml
	$(DIR_GUARD)
	ocamlfind ocamlopt $(OCAMLOPT_FLAGS) -package $(PACKAGES) -thread -o $(RELEASE_DIR)/conn_ops -c $<

#Special rules for creating the lexer and parser
$(RELEASE_DIR)/parser.cmi:	src/parser.mli
	$(DIR_GUARD)
	ocamlopt $(OCAMLOPT_FLAGS) -o $(RELEASE_DIR)/parser -c $<
$(RELEASE_DIR)/parser.cmx:	src/parser.ml $(RELEASE_DIR)/parser.cmi
	$(DIR_GUARD)
	ocamlopt $(OCAMLOPT_FLAGS) -o $(RELEASE_DIR)/parser -c $<

#General rule for compiling
$(RELEASE_DIR)/%.cmi $(RELEASE_DIR)/%.cmx: src/%.ml
	$(DIR_GUARD)
	ocamlopt $(OCAMLOPT_FLAGS) -o $(RELEASE_DIR)/$* -c $<
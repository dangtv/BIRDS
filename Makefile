PACKAGES=str,num 

# create directory if not exist
DIR_GUARD=@mkdir -p $(@D)

#Name of the final executable file to be generated
EX_NAME=birds

# source folder
SOURCE_DIR=src
LOGIC_SOURCE_DIR=src/logic

# binary folder for compilation
BIN_DIR=bin
LOGIC_BIN_DIR=bin/logic
RELEASE_DIR = release
LOGIC_RELEASE_DIR = release/logic

OCAMLC_FLAGS=-bin-annot -I $(BIN_DIR) -I $(LOGIC_BIN_DIR)
OCAMLOPT_FLAGS=-bin-annot -I $(RELEASE_DIR) -I $(LOGIC_RELEASE_DIR)
OCAMLDEP_FLAGS=-I $(SOURCE_DIR) -I $(LOGIC_SOURCE_DIR)

#Name of the files that are part of the project
MAIN_FILE=main
LOGIC_FILES=\
    lib intro formulas prop fol skolem fol_ex\

TOP_FILES=\
    expr utils parser lexer\
	rule_preprocess derivation\
    ast2sql\

FILES=\
    $(LOGIC_FILES:%=logic/%)\
    $(TOP_FILES)

.PHONY: all release clean depend annot
all: $(BIN_DIR)/$(EX_NAME) annot

#Rule for generating the final executable file
$(BIN_DIR)/$(EX_NAME): $(FILES:%=$(BIN_DIR)/%.cmo) $(BIN_DIR)/$(MAIN_FILE).cmo
	$(DIR_GUARD)
	ocamlfind ocamlc $(OCAMLC_FLAGS) -package $(PACKAGES) -thread -linkpkg $(FILES:%=$(BIN_DIR)/%.cmo) $(BIN_DIR)/$(MAIN_FILE).cmo -o $(BIN_DIR)/$(EX_NAME)

#Rule for compiling the main file
$(BIN_DIR)/$(MAIN_FILE).cmo: $(FILES:%=$(BIN_DIR)/%.cmo) $(SOURCE_DIR)/$(MAIN_FILE).ml
	$(DIR_GUARD)
	ocamlfind ocamlc $(OCAMLC_FLAGS) -package $(PACKAGES) -thread -o $(BIN_DIR)/$(MAIN_FILE) -c $(SOURCE_DIR)/$(MAIN_FILE).ml

#Special rules for creating the lexer and parser
$(SOURCE_DIR)/parser.ml $(SOURCE_DIR)/parser.mli: $(SOURCE_DIR)/parser.mly
	ocamlyacc $<
$(BIN_DIR)/parser.cmi:	$(SOURCE_DIR)/parser.mli
	$(DIR_GUARD)
	ocamlc $(OCAMLC_FLAGS) -o $(BIN_DIR)/parser -c $<
$(BIN_DIR)/parser.cmo:	$(SOURCE_DIR)/parser.ml $(BIN_DIR)/parser.cmi
	$(DIR_GUARD)
	ocamlc $(OCAMLC_FLAGS) -o $(BIN_DIR)/parser -c $<
$(SOURCE_DIR)/lexer.ml:	$(SOURCE_DIR)/lexer.mll
	ocamllex $<

#General rule for compiling
$(BIN_DIR)/%.cmi $(BIN_DIR)/%.cmo $(BIN_DIR)/%.cmt: $(SOURCE_DIR)/%.ml
	$(DIR_GUARD)
	ocamlfind ocamlc $(OCAMLC_FLAGS) -package $(PACKAGES) -thread -o $(BIN_DIR)/$* -c $<

annot: $(LOGIC_FILES:%=$(LOGIC_BIN_DIR)/%.cmt) $(TOP_FILES:%=$(BIN_DIR)/%.cmt)
	mv $(LOGIC_FILES:%=$(LOGIC_BIN_DIR)/%.cmt) $(LOGIC_SOURCE_DIR)/
	mv $(TOP_FILES:%=$(BIN_DIR)/%.cmt) $(SOURCE_DIR)/

include .depend

clean:
	rm -f $(BIN_DIR)/*.cmo $(BIN_DIR)/*.cmi $(BIN_DIR)/*.o $(LOGIC_BIN_DIR)/*.cmo $(LOGIC_BIN_DIR)/*.cmi $(LOGIC_BIN_DIR)/*.o $(SOURCE_DIR)/parser.mli $(SOURCE_DIR)/parser.ml $(SOURCE_DIR)/lexer.ml $(SOURCE_DIR)/*.cmt $(LOGIC_SOURCE_DIR)/*.cmt

depend:
	ocamlfind ocamldep $(OCAMLDEP_FLAGS) $(FILES:%=$(SOURCE_DIR)/%.ml) $(SOURCE_DIR)/lexer.mll $(SOURCE_DIR)/parser.mli |sed -e 's/$(SOURCE_DIR)/$(BIN_DIR)/g' > .depend

release: $(RELEASE_DIR)/$(EX_NAME)

#Rule for generating the final executable file
$(RELEASE_DIR)/$(EX_NAME): $(FILES:%=$(RELEASE_DIR)/%.cmx) $(RELEASE_DIR)/$(MAIN_FILE).cmx
	$(DIR_GUARD)
	ocamlfind ocamlopt $(OCAMLOPT_FLAGS) -package $(PACKAGES) -thread -linkpkg $(FILES:%=$(RELEASE_DIR)/%.cmx) $(RELEASE_DIR)/$(MAIN_FILE).cmx -o $(RELEASE_DIR)/$(EX_NAME)

#Rule for compiling the main file
$(RELEASE_DIR)/$(MAIN_FILE).cmx: $(FILES:%=$(RELEASE_DIR)/%.cmx) $(SOURCE_DIR)/$(MAIN_FILE).ml
	$(DIR_GUARD)
	ocamlfind ocamlopt $(OCAMLOPT_FLAGS) -package $(PACKAGES) -thread -o $(RELEASE_DIR)/$(MAIN_FILE) -c $(SOURCE_DIR)/$(MAIN_FILE).ml

#Special rules for creating the lexer and parser
$(RELEASE_DIR)/parser.cmi:	$(SOURCE_DIR)/parser.mli
	$(DIR_GUARD)
	ocamlopt $(OCAMLOPT_FLAGS) -o $(RELEASE_DIR)/parser -c $<
$(RELEASE_DIR)/parser.cmx:	$(SOURCE_DIR)/parser.ml $(RELEASE_DIR)/parser.cmi
	$(DIR_GUARD)
	ocamlopt $(OCAMLOPT_FLAGS) -o $(RELEASE_DIR)/parser -c $<

#General rule for compiling
$(RELEASE_DIR)/%.cmi $(RELEASE_DIR)/%.cmx: $(SOURCE_DIR)/%.ml
	$(DIR_GUARD)
	ocamlfind ocamlopt $(OCAMLOPT_FLAGS) -package $(PACKAGES) -thread -o $(RELEASE_DIR)/$* -c $<

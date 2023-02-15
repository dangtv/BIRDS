PACKAGES=postgresql,str,num,ocamlgraph

# create directory if not exist
DIR_GUARD=@mkdir -p $(@D)

#Name of the final executable file to be generated
EX_NAME=birds
TEST_EX_NAME=birds_unit_tests

# source folder
SOURCE_DIR=src
LOGIC_SOURCE_DIR=src/logic
TEST_SOURCE_DIR=src/test

# binary folder for compilation
BIN_DIR=bin
LOGIC_BIN_DIR=bin/logic
RELEASE_DIR = release
LOGIC_RELEASE_DIR = release/logic
OBJ_DIR=${SOURCE_DIR}
LOGIC_OBJ_DIR=${LOGIC_SOURCE_DIR}
TEST_OBJ_DIR=${TEST_SOURCE_DIR}

OCAMLC_FLAGS=-bin-annot -w -26  -I $(OBJ_DIR) -I $(LOGIC_OBJ_DIR) -I $(TEST_OBJ_DIR)
OCAMLOPT_FLAGS=-bin-annot -w -26 -I $(RELEASE_DIR) -I $(LOGIC_RELEASE_DIR)
OCAMLDEP_FLAGS=-I $(SOURCE_DIR) -I $(LOGIC_SOURCE_DIR) -I $(TEST_SOURCE_DIR)

#Name of the files that are part of the project
MAIN_FILE=main
TEST_MAIN_FILE=test_main
LOGIC_FILES=\
    lib intro formulas prop fol skolem fol_ex\

LOGIC_FILES_WITH_MLI=\
    lib intro formulas prop fol skolem fol_ex\

TOP_FILES=\
	expr utils parser lexer\
	dependency_graph\
	conn_ops\
	rule_preprocess stratification derivation \
	bottom_up evaluation\
	ast2fol ast2sql ast2ros\
	ast2theorem \
	bx\
	inlining\
	debugger\
	simplification\

TOP_FILES_WITH_MLI=\
	parser expr conversion ast2sql ast2theorem dependency_graph\
	simplification\

TEST_ONLY_FILES=\
    ast2sql_operation_based_conversion_test\
    simplification_test\


FILES=\
    $(LOGIC_FILES:%=logic/%)\
    $(TOP_FILES) \

TEST_FILES=\
    $(FILES)\
    $(TEST_ONLY_FILES:%=test/%)\

.PHONY: all release clean depend test #annot
all: $(BIN_DIR)/$(EX_NAME)

test: $(BIN_DIR)/$(TEST_EX_NAME)
	./$(BIN_DIR)/$(TEST_EX_NAME)

#Rule for generating the final executable file
$(BIN_DIR)/$(EX_NAME): $(FILES:%=$(OBJ_DIR)/%.cmo) $(OBJ_DIR)/$(MAIN_FILE).cmo
	$(DIR_GUARD)
	ocamlfind ocamlc $(OCAMLC_FLAGS) -package $(PACKAGES) -thread -linkpkg $(FILES:%=$(OBJ_DIR)/%.cmo) $(OBJ_DIR)/$(MAIN_FILE).cmo -o $(BIN_DIR)/$(EX_NAME)

#Rule for compiling the main file
$(OBJ_DIR)/$(MAIN_FILE).cmo: $(FILES:%=$(OBJ_DIR)/%.cmo) $(SOURCE_DIR)/$(MAIN_FILE).ml
	$(DIR_GUARD)
	ocamlfind ocamlc $(OCAMLC_FLAGS) -package $(PACKAGES) -thread -o $(OBJ_DIR)/$(MAIN_FILE) -c $(SOURCE_DIR)/$(MAIN_FILE).ml

$(BIN_DIR)/$(TEST_EX_NAME): $(TEST_FILES:%=$(OBJ_DIR)/%.cmo) $(TEST_OBJ_DIR)/$(TEST_MAIN_FILE).cmo
	$(DIR_GUARD)
	ocamlfind ocamlc $(OCAMLC_FLAGS) -package $(PACKAGES) -thread -linkpkg $(TEST_FILES:%=$(OBJ_DIR)/%.cmo) $(TEST_OBJ_DIR)/$(TEST_MAIN_FILE).cmo -o $(BIN_DIR)/$(TEST_EX_NAME)

$(TEST_OBJ_DIR)/$(TEST_MAIN_FILE).cmo: $(TEST_FILES:%=$(OBJ_DIR)/%.cmo) $(TEST_SOURCE_DIR)/$(TEST_MAIN_FILE).ml
	$(DIR_GUARD)
	ocamlfind ocamlc $(OCAMLC_FLAGS) -package $(PACKAGES) -thread -o $(TEST_OBJ_DIR)/$(TEST_MAIN_FILE) -c $(TEST_SOURCE_DIR)/$(TEST_MAIN_FILE).ml

#Special rules for creating the lexer and parser
$(SOURCE_DIR)/parser.ml $(SOURCE_DIR)/parser.mli: $(SOURCE_DIR)/parser.mly
	ocamlyacc $<
$(SOURCE_DIR)/lexer.ml:	$(SOURCE_DIR)/lexer.mll
	ocamllex $<

#With mli files.
$(TOP_FILES_WITH_MLI:%=$(OBJ_DIR)/%.cmi): $(OBJ_DIR)/%.cmi: $(SOURCE_DIR)/%.mli
	$(DIR_GUARD)
	ocamlfind ocamlc $(OCAMLC_FLAGS) -o $(OBJ_DIR)/$* -c $<

$(TOP_FILES_WITH_MLI:%=$(OBJ_DIR)/%.cmo): $(OBJ_DIR)/%.cmo: $(SOURCE_DIR)/%.ml $(OBJ_DIR)/%.cmi
	$(DIR_GUARD)
	ocamlfind ocamlc $(OCAMLC_FLAGS) -package $(PACKAGES) -thread -o $(OBJ_DIR)/$* -c $<

$(LOGIC_FILES_WITH_MLI:%=$(LOGIC_OBJ_DIR)/%.cmi): $(LOGIC_OBJ_DIR)/%.cmi: $(LOGIC_SOURCE_DIR)/%.mli
	$(DIR_GUARD)
	ocamlfind ocamlc $(OCAMLC_FLAGS) -o $(LOGIC_OBJ_DIR)/$* -c $<

$(LOGIC_FILES_WITH_MLI:%=$(LOGIC_OBJ_DIR)/%.cmo): $(LOGIC_OBJ_DIR)/%.cmo: $(LOGIC_SOURCE_DIR)/%.ml $(LOGIC_OBJ_DIR)/%.cmi
	$(DIR_GUARD)
	ocamlfind ocamlc $(OCAMLC_FLAGS) -package $(PACKAGES) -thread -o $(LOGIC_OBJ_DIR)/$* -c $<

#General rule for compiling
$(OBJ_DIR)/%.cmi $(OBJ_DIR)/%.cmo $(OBJ_DIR)/%.cmt: $(SOURCE_DIR)/%.ml
	$(DIR_GUARD)
	ocamlfind ocamlc $(OCAMLC_FLAGS) -package $(PACKAGES) -thread -o $(OBJ_DIR)/$* -c $<

include depend

clean:
	rm -r -f $(BIN_DIR)/* $(RELEASE_DIR)/* $(SOURCE_DIR)/parser.mli $(SOURCE_DIR)/parser.ml $(SOURCE_DIR)/lexer.ml $(OBJ_DIR)/*.cmt $(LOGIC_OBJ_DIR)/*.cmt $(TEST_OBJ_DIR)/*.cmt $(OBJ_DIR)/*.cmti $(LOGIC_OBJ_DIR)/*.cmti $(TEST_OBJ_DIR)/*.cmti $(OBJ_DIR)/*.cmo $(LOGIC_OBJ_DIR)/*.cmo $(TEST_OBJ_DIR)/*.cmo $(OBJ_DIR)/*.cmi $(LOGIC_OBJ_DIR)/*.cmi $(TEST_OBJ_DIR)/*.cmi

depend:
	ocamlfind ocamldep $(OCAMLDEP_FLAGS) $(FILES:%=$(SOURCE_DIR)/%.ml) $(SOURCE_DIR)/lexer.mll $(SOURCE_DIR)/parser.mli |sed -e 's/$(SOURCE_DIR)/$(BIN_DIR)/g' > depend

release: $(RELEASE_DIR)/$(EX_NAME)

#Rule for generating the final executable file
$(RELEASE_DIR)/$(EX_NAME): $(FILES:%=$(RELEASE_DIR)/%.cmx) $(RELEASE_DIR)/$(MAIN_FILE).cmx
	$(DIR_GUARD)
	ocamlfind ocamlopt $(OCAMLOPT_FLAGS) -package $(PACKAGES) -thread -linkpkg $(FILES:%=$(RELEASE_DIR)/%.cmx) $(RELEASE_DIR)/$(MAIN_FILE).cmx -o $(RELEASE_DIR)/$(EX_NAME)
	rm -f $(RELEASE_DIR)/*.cmx $(RELEASE_DIR)/*.cmi $(RELEASE_DIR)/*.o $(LOGIC_RELEASE_DIR)/*.cmx $(LOGIC_RELEASE_DIR)/*.cmi $(LOGIC_RELEASE_DIR)/*.o

#Rule for compiling the main file
$(RELEASE_DIR)/$(MAIN_FILE).cmx: $(FILES:%=$(RELEASE_DIR)/%.cmx) $(SOURCE_DIR)/$(MAIN_FILE).ml
	$(DIR_GUARD)
	ocamlfind ocamlopt $(OCAMLOPT_FLAGS) -package $(PACKAGES) -thread -o $(RELEASE_DIR)/$(MAIN_FILE) -c $(SOURCE_DIR)/$(MAIN_FILE).ml

#Special rules for creating the lexer and parser
$(RELEASE_DIR)/parser.cmi:	$(SOURCE_DIR)/parser.mli
	$(DIR_GUARD)
	ocamlfind ocamlopt $(OCAMLOPT_FLAGS) -o $(RELEASE_DIR)/parser -c $<
$(RELEASE_DIR)/parser.cmx:	$(SOURCE_DIR)/parser.ml $(RELEASE_DIR)/parser.cmi
	$(DIR_GUARD)
	ocamlfind ocamlopt $(OCAMLOPT_FLAGS) -o $(RELEASE_DIR)/parser -c $<

#With mli files.
$(TOP_FILES_WITH_MLI:%=$(RELEASE_DIR)/%.cmi): $(RELEASE_DIR)/%.cmi: $(SOURCE_DIR)/%.mli
	$(DIR_GUARD)
	ocamlfind ocamlopt $(OCAMLOPT_FLAGS) -o $(RELEASE_DIR)/$* -c $<

$(TOP_FILES_WITH_MLI:%=$(RELEASE_DIR)/%.cmo): $(RELEASE_DIR)/%.cmo: $(SOURCE_DIR)/%.ml $(RELEASE_DIR)/%.cmi
	$(DIR_GUARD)
	ocamlfind ocamlopt $(OCAMLOPT_FLAGS) -package $(PACKAGES) -thread -o $(RELEASE_DIR)/$* -c $<

$(TOP_FILES_WITH_MLI:%=$(RELEASE_DIR)/%.cmx): $(RELEASE_DIR)/%.cmx: $(SOURCE_DIR)/%.ml $(RELEASE_DIR)/%.cmi
	$(DIR_GUARD)
	ocamlfind ocamlopt $(OCAMLOPT_FLAGS) -package $(PACKAGES) -thread -o $(RELEASE_DIR)/$* -c $<

$(LOGIC_FILES_WITH_MLI:%=$(LOGIC_RELEASE_DIR)/%.cmi): $(LOGIC_RELEASE_DIR)/%.cmi: $(LOGIC_SOURCE_DIR)/%.mli
	$(DIR_GUARD)
	ocamlfind ocamlopt $(OCAMLOPT_FLAGS) -o $(LOGIC_RELEASE_DIR)/$* -c $<

$(LOGIC_FILES_WITH_MLI:%=$(LOGIC_RELEASE_DIR)/%.cmo): $(LOGIC_RELEASE_DIR)/%.cmo: $(LOGIC_SOURCE_DIR)/%.ml $(LOGIC_RELEASE_DIR)/%.cmi
	$(DIR_GUARD)
	ocamlfind ocamlopt $(OCAMLOPT_FLAGS) -package $(PACKAGES) -thread -o $(RELEASE_DIR)/$* -c $<

$(LOGIC_FILES_WITH_MLI:%=$(LOGIC_RELEASE_DIR)/%.cmx): $(LOGIC_RELEASE_DIR)/%.cmx: $(LOGIC_SOURCE_DIR)/%.ml $(LOGIC_RELEASE_DIR)/%.cmi
	$(DIR_GUARD)
	ocamlfind ocamlopt $(OCAMLOPT_FLAGS) -package $(PACKAGES) -thread -o $(LOGIC_RELEASE_DIR)/$* -c $<

#General rule for compiling
$(RELEASE_DIR)/%.cmi $(RELEASE_DIR)/%.cmx: $(SOURCE_DIR)/%.ml
	$(DIR_GUARD)
	ocamlfind ocamlopt $(OCAMLOPT_FLAGS) -package $(PACKAGES) -thread -o $(RELEASE_DIR)/$* -c $<

install:
	make release
	mv release/birds /usr/local/bin/

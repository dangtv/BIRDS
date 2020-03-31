PACKAGES=postgresql,str,num 

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
OBJ_DIR=${SOURCE_DIR}
LOGIC_OBJ_DIR=${LOGIC_SOURCE_DIR}

OCAMLC_FLAGS=-bin-annot -w -26  -I $(OBJ_DIR) -I $(LOGIC_OBJ_DIR)
OCAMLOPT_FLAGS=-bin-annot -w -26 -I $(RELEASE_DIR) -I $(LOGIC_RELEASE_DIR)
OCAMLDEP_FLAGS=-I $(SOURCE_DIR) -I $(LOGIC_SOURCE_DIR)

#Name of the files that are part of the project
# MAIN_FILE=eval_main
MAIN_FILE=main
LOGIC_FILES=\
    lib intro formulas prop fol skolem fol_ex\

TOP_FILES=\
    expr utils parser lexer\
    conn_ops\
	rule_preprocess stratification derivation \
	ast2fol ast2sql \
	ast2theorem \
	bx\

FILES=\
    $(LOGIC_FILES:%=logic/%)\
    $(TOP_FILES)
	
.PHONY: all release clean depend #annot
all: $(BIN_DIR)/$(EX_NAME)

#Rule for generating the final executable file
$(BIN_DIR)/$(EX_NAME): $(FILES:%=$(OBJ_DIR)/%.cmo) $(OBJ_DIR)/$(MAIN_FILE).cmo
	$(DIR_GUARD)
	ocamlfind ocamlc $(OCAMLC_FLAGS) -package $(PACKAGES) -thread -linkpkg $(FILES:%=$(OBJ_DIR)/%.cmo) $(OBJ_DIR)/$(MAIN_FILE).cmo -o $(BIN_DIR)/$(EX_NAME)

#Rule for compiling the main file
$(OBJ_DIR)/$(MAIN_FILE).cmo: $(FILES:%=$(OBJ_DIR)/%.cmo) $(SOURCE_DIR)/$(MAIN_FILE).ml
	$(DIR_GUARD)
	ocamlfind ocamlc $(OCAMLC_FLAGS) -package $(PACKAGES) -thread -o $(OBJ_DIR)/$(MAIN_FILE) -c $(SOURCE_DIR)/$(MAIN_FILE).ml

#Special rules for creating the lexer and parser
$(SOURCE_DIR)/parser.ml $(SOURCE_DIR)/parser.mli: $(SOURCE_DIR)/parser.mly
	ocamlyacc $<
$(OBJ_DIR)/parser.cmi:	$(SOURCE_DIR)/parser.mli
	$(DIR_GUARD)
	ocamlc $(OCAMLC_FLAGS) -o $(OBJ_DIR)/parser -c $<
$(OBJ_DIR)/parser.cmo:	$(SOURCE_DIR)/parser.ml $(OBJ_DIR)/parser.cmi
	$(DIR_GUARD)
	ocamlc $(OCAMLC_FLAGS) -o $(OBJ_DIR)/parser -c $<
$(SOURCE_DIR)/lexer.ml:	$(SOURCE_DIR)/lexer.mll
	ocamllex $<

#General rule for compiling
$(OBJ_DIR)/%.cmi $(OBJ_DIR)/%.cmo $(OBJ_DIR)/%.cmt: $(SOURCE_DIR)/%.ml
	$(DIR_GUARD)
	ocamlfind ocamlc $(OCAMLC_FLAGS) -package $(PACKAGES) -thread -o $(OBJ_DIR)/$* -c $<

# annot: $(FILES:%=$(SOURCE_DIR)/%.cmt) $(MAIN_FILE:%=$(SOURCE_DIR)/%.cmt)

# $(FILES:%=$(SOURCE_DIR)/%.cmt) $(MAIN_FILE:%=$(SOURCE_DIR)/%.cmt): $(LOGIC_FILES:%=$(LOGIC_OBJ_DIR)/%.cmt) $(TOP_FILES:%=$(OBJ_DIR)/%.cmt) $(MAIN_FILE:%=$(OBJ_DIR)/%.cmt)
# 	cp $(LOGIC_FILES:%=$(LOGIC_OBJ_DIR)/%.cmt) $(LOGIC_SOURCE_DIR)/
# 	cp $(TOP_FILES:%=$(OBJ_DIR)/%.cmt) $(SOURCE_DIR)/
# 	cp $(MAIN_FILE:%=$(OBJ_DIR)/%.cmt) $(SOURCE_DIR)/

include depend

clean:
	rm -r -f $(BIN_DIR)/* $(RELEASE_DIR)/* $(SOURCE_DIR)/parser.mli $(SOURCE_DIR)/parser.ml $(SOURCE_DIR)/lexer.ml $(OBJ_DIR)/*.cmt $(LOGIC_OBJ_DIR)/*.cmt $(OBJ_DIR)/*.cmti $(LOGIC_OBJ_DIR)/*.cmti $(OBJ_DIR)/*.cmo $(LOGIC_OBJ_DIR)/*.cmo $(OBJ_DIR)/*.cmi $(LOGIC_OBJ_DIR)/*.cmi

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
	ocamlopt $(OCAMLOPT_FLAGS) -o $(RELEASE_DIR)/parser -c $<
$(RELEASE_DIR)/parser.cmx:	$(SOURCE_DIR)/parser.ml $(RELEASE_DIR)/parser.cmi
	$(DIR_GUARD)
	ocamlopt $(OCAMLOPT_FLAGS) -o $(RELEASE_DIR)/parser -c $<

#General rule for compiling
$(RELEASE_DIR)/%.cmi $(RELEASE_DIR)/%.cmx: $(SOURCE_DIR)/%.ml
	$(DIR_GUARD)
	ocamlfind ocamlopt $(OCAMLOPT_FLAGS) -package $(PACKAGES) -thread -o $(RELEASE_DIR)/$* -c $<

install: 
	make release
	mv release/birds /usr/local/bin/
MODULES=main animation gui state homemode dolphin 
OBJECTS=$(MODULES:=.cmo)
MLS=$(MODULES:=.ml)
MLIS=$(MODULES:=.mli)
TEST=test.byte
MAIN=main.byte
GUI=gui.byte
ANIMATION=animation.byte
HOMEMODE=homemode.byte
STATE=state.byte
DOLPHIN=dolphin.byte
OCAMLBUILD=ocamlbuild -use-ocamlfind

default: build
	OCAMLRUNPARAM=b utop

build:
	$(OCAMLBUILD) $(OBJECTS)

test:
	$(OCAMLBUILD) -tag 'debug' $(TEST) && ./$(TEST) -runner sequential

play:
	$(OCAMLBUILD) -tag 'debug' $(MAIN) && OCAMLRUNPARAM=b ./$(MAIN)

view:
	$(OCAMLBUILD) -tag 'debug' $(VIEW) && OCAMLRUNPARAM=b ./$(VIEW)

gui:
	$(OCAMLBUILD) -tag 'debug' $(GUI) && OCAMLRUNPARAM=b ./$(GUI)

dolphin:
	$(OCAMLBUILD) -tag 'debug' $(DOLPHIN) && OCAMLRUNPARAM=b ./$(DOLPHIN)

homemode:
	$(OCAMLBUILD) -tag 'debug' $(HOMEMODE) && OCAMLRUNPARAM=b ./$(HOMEMODE)

state:
	$(OCAMLBUILD) -tag 'debug' $(STATE) && OCAMLRUNPARAM=b ./$(STATE)
	
# check:
# 	@bash check.sh
	
# finalcheck:
# 	@bash check.sh final

# zip:
# 	zip adventure.zip *.ml* *.json *.sh _tags .merlin .ocamlformat .ocamlinit LICENSE Makefile	
	
docs: docs-public docs-private
	
docs-public: build
	mkdir -p _doc.public
	ocamlfind ocamldoc -I _build -package yojson,ANSITerminal,Graphics \
		-html -stars -d _doc.public $(MLIS)

docs-private: build
	mkdir -p _doc.private
	ocamlfind ocamldoc -I _build -package yojson,ANSITerminal,Graphics \
		-html -stars -d _doc.private \
		-inv-merge-ml-mli -m A $(MLIS) $(MLS)

clean:
	ocamlbuild -clean
	rm -rf _doc.public _doc.private adventure.zip

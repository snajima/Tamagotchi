MODULES=main animation gui state homemode dolphin dolphinview drum drumview elementals elementalsview
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
DOLPHINVIEW=dolphinview.byte
DRUM=drum.byte
DRUMVIEW=drumview.byte
ELEMENTALS=elementalist.byte
ELEMENTALISTVIEW=elementalistview.byte
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

homemode:
	$(OCAMLBUILD) -tag 'debug' $(HOMEMODE) && OCAMLRUNPARAM=b ./$(HOMEMODE)

state:
	$(OCAMLBUILD) -tag 'debug' $(STATE) && OCAMLRUNPARAM=b ./$(STATE)

dolphin:
	$(OCAMLBUILD) -tag 'debug' $(DOLPHIN) && OCAMLRUNPARAM=b ./$(DOLPHIN)

dolphinview:
	$(OCAMLBUILD) -tag 'debug' $(DOLPHINVIEW) && OCAMLRUNPARAM=b ./$(DOLPHINVIEW)	

drum:
	$(OCAMLBUILD) -tag 'debug' $(DRUM) && OCAMLRUNPARAM=b ./$(DRUM)

drumview:
	$(OCAMLBUILD) -tag 'debug' $(DRUMVIEW) && OCAMLRUNPARAM=b ./$(DRUMVIEW)	

elementals:
	$(OCAMLBUILD) -tag 'debug' $(ELEMENTALS) && OCAMLRUNPARAM=b ./$(ELEMENTALS)	

elementalsview:
	$(OCAMLBUILD) -tag 'debug' $(ELEMENTALSVIEW) && OCAMLRUNPARAM=b ./$(ELEMENTALSVIEW)	


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

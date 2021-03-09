# Build the cpdf command line tools and top level
MODS = xmlm cpdfstrftime cpdf cpdfcommand

SOURCES = $(foreach x,$(MODS),$(x).ml $(x).mli) cpdfcommandrun.ml

RESULT = cpdf
ANNOTATE = true
PACKS = camlpdf

OCAMLNCFLAGS = -g -bin-annot -safe-string -w -3 -annot
OCAMLBCFLAGS = -g -bin-annot -safe-string -w -3 -annot
OCAMLLDFLAGS = -g

all : native-code native-code-library byte-code-library top htdoc

clean ::
	rm -rf doc foo foo2 out.pdf out2.pdf *.cmt

LIBINSTALL_FILES = cpdf.a cpdf.cma cpdf.cmxa \
$(foreach x,$(MODS),$x.mli) $(foreach x,$(MODS),$x.cmi) \
$(foreach x,$(MODS),$x.cmx) $(foreach x,$(MODS),$x.cmt)

install : libinstall

-include OCamlMakefile


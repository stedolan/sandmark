.PHONY: FORCE all dune

all: compilers/.dune-workspace dune
	@dune b --workspace=$^ --release @buildbench

# The BASE_OCAML version is used to build dune
BASE_OCAML=4.12.0
DUNE=tools/dune/_build/install/default/bin/dune

dune: $(DUNE)

$(DUNE): compilers/$(BASE_OCAML)
	PATH="$$(pwd)/compilers/$(BASE_OCAML)/_install/bin:$$PATH" $(MAKE) -C tools/dune release

compilers/.dune-workspace: FORCE dune
	@{ cd compilers; echo '(lang dune 2.8)'; \
	  if [ "$$(echo */_install/bin)" = "*/_install/bin" ]; then \
	    echo "No installed compilers found" 1>&2; exit 1; \
	  fi; \
	  for i in */_install/bin; do \
	    echo "(context (default (name $${i%/_install/bin}) (paths (PATH (\"$$(pwd)/$$i\" :standard)))))"; \
	  done } > $@

compilers/%:
	@if [ -z '$(SRC)' ]; then echo 'error: no SRC specified for compiler $*' 1>&2; fi
	mkdir '$@'
	wget -qO- '$(SRC)' | tar xz --strip-components 1 -C '$@'
	( cd '$@'; ./configure --quiet --disable-debugger --disable-ocamldoc --prefix "`pwd`/_install" $(CONF); )
	$(MAKE) -s -C '$@' world.opt
	$(MAKE) -s -C '$@' install
	rm -rf '$@'/_install/lib/ocaml/compiler-libs '$@'/_install/lib/ocaml/*.cmt*

compilers/4.08.0: SRC=https://github.com/ocaml/ocaml/archive/4.08.0.tar.gz
compilers/4.08.0: CONF=CC='gcc -fcommon'

compilers/4.12.0: SRC=https://github.com/ocaml/ocaml/archive/4.12.0.tar.gz
compilers/trunk: SRC=https://github.com/ocaml/ocaml/archive/trunk.tar.gz

compilers/4.10.0+multicore: SRC=https://github.com/ocaml-multicore/ocaml-multicore/archive/parallel_minor_gc.tar.gz

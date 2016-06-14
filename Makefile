.PHONY: build clean install uninstall check
OB=ocamlbuild -use-ocamlfind
PACKAGE=multipart-form-data

build:
	$(OB) $(PACKAGE).otarget

install: uninstall

install:
	ocamlfind install $(PACKAGE) META $(shell cat $(PACKAGE).itarget)

uninstall:
	ocamlfind remove $(PACKAGE)

check:
	$(OB) tests.native --

clean:
	$(OB) -clean

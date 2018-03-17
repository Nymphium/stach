OCAMLBUILD = ocamlbuild
OPTS =
BUILD_OPTS = -use-ocamlfind -use-menhir -I $(SRC)
MAKE=make

SRC = src
TARGET = main.native

.PHONY: all clean

all: $(TARGET)

$(TARGET): OPTS=$(BUILD_OPTS)
$(TARGET): _tmpl

clean: OPTS=-clean
clean: TARGET=
clean: _tmpl

_tmpl:
	$(OCAMLBUILD) $(OPTS) $(TARGET)


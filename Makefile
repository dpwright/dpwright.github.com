BIN_DIR=bin
WORK_DIR=obj/`arch`
UTILS=$(addprefix $(BIN_DIR)/,new-post bl)

HC=ghc
HCFLAGS=-odir $(WORK_DIR) -hidir $(WORK_DIR)

CROSSPOSTS=$(shell grep -l "^xp:" posts/*)

.PHONY: all build check utils
all: build

site: site.hs
	$(HC) --make $^

build: site utils
	./site build

check: build
	./site check

utils: $(UTILS)

crosspost: $(BIN_DIR)/bl $(CROSSPOSTS)
	for POST in $(CROSSPOSTS); do $(BIN_DIR)/bl $$POST --publish; done

$(BIN_DIR)/new-post: posts/2013-06-03-literate-haskell-script-to-create-a-new-hakyll-post.lhs
	@mkdir -p $(BIN_DIR)
	$(HC) $(HCFLAGS) -o $@ $^

$(BIN_DIR)/bl: pending-posts/2013-10-01-cross-posting-between-hakyll-and-wordpress.lhs
	$(HC) $(HCFLAGS) -o $@ $^

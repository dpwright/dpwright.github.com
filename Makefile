BIN_DIR=bin
WORK_DIR=obj/`arch`
UTILS=$(addprefix $(BIN_DIR)/,new-post)

HC=ghc
HCFLAGS=-odir $(WORK_DIR) -hidir $(WORK_DIR)

.PHONY: all build utils
all: site build utils

site: site.hs
	$(HC) --make $^

build: site
	./site build

utils: $(UTILS)

$(BIN_DIR)/new-post: posts/2013-06-03-literate-haskell-script-to-create-a-new-hakyll-post.lhs
	@mkdir -p $(BIN_DIR)
	$(HC) $(HCFLAGS) -o $@ $^

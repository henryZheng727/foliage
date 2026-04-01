MODEL_DIR := foliage-model
VIEW_DIR := foliage-view

# js_of_ocaml output from dune, copied into the React project
MODEL_JS_SRC := $(MODEL_DIR)/_build/default/bin/main.bc.js
MODEL_JS_DEST := $(VIEW_DIR)/src/model/foliage_model.js

.PHONY: all model view dev clean

all: model view

# --- OCaml model (compiled to JS via js_of_ocaml) ---

model:
	dune build --root $(MODEL_DIR) bin/main.bc.js
	@mkdir -p $(dir $(MODEL_JS_DEST))
	@rm -f $(MODEL_JS_DEST)
	cp $(MODEL_JS_SRC) $(MODEL_JS_DEST)

# --- React view ---

view: model
	cd $(VIEW_DIR) && npm run build

dev: model
	cd $(VIEW_DIR) && npm run dev

# --- Utilities ---

clean:
	dune clean --root $(MODEL_DIR)
	rm -rf $(VIEW_DIR)/dist
	rm -f $(MODEL_JS_DEST)

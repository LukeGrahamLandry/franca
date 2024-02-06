grammar: grammar/grammar.js
	cd grammar && ./node_modules/.bin/tree-sitter generate
    
run: grammar
	cargo run --features logging

test: grammar
	cargo test

.PHONY: run test

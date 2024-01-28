grammar: grammar/grammar.js
	cd grammar && ./node_modules/.bin/tree-sitter generate
    
run: grammar
	cargo run

test: grammar
	cargo test

.PHONY: run test

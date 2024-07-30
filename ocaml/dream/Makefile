.PHONY: deps
deps:
	opam switch create . 5.2.0 --yes
	opam install --deps-only --yes .

.PHONY: serve
serve:
	dune exec --root . ./server.exe

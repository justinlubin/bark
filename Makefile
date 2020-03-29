run:
	dune exec examples/examples.exe

examples:
	dune build examples/examples.exe

clean:
	dune clean

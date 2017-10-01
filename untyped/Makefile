all: Untyped

Tokens.hs : Tokens.x
	alex Tokens.x

Grammar.hs : Grammar.y
	happy Grammar.y

Untyped : Tokens.hs Grammar.hs Eval.hs Untyped.hs
	ghc --make Untyped

install: all
	sudo mv Untyped /usr/local/bin/

run: install
	Untyped

clean:
	rm -f Untyped Grammar.hs Tokens.hs *.o *.hi

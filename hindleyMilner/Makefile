all: Main

Tokens.hs : Tokens.x
	alex Tokens.x

Grammar.hs : Grammar.y
	happy Grammar.y

Main : Tokens.hs Terms.hs Grammar.hs Eval.hs Main.hs
	ghc --make Main

install: all
	sudo mv SimplyTyped /usr/local/bin/

run: install
	Main

clean:
	rm -f Main Grammar.hs Tokens.hs *.o *.hi

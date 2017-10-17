all: SimplyTyped

Tokens.hs : Tokens.x
	alex Tokens.x

Grammar.hs : Grammar.y
	happy Grammar.y

SimplyTyped : Tokens.hs Grammar.hs TypeCheck.hs Eval.hs SimplyTyped.hs Types.hs
	ghc --make SimplyTyped

install: all
	sudo mv SimplyTyped /usr/local/bin/

run: install
	SimplyTyped

clean:
	rm -f SimplyTyped Grammar.hs Tokens.hs *.o *.hi

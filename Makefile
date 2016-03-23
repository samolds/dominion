all: client

client: client.hs
	@ghc --make client.hs
	@rm -f *.o *.hi *.dyn_*

test: test.hs
	@ghc --make test.hs
	@rm -f *.o *.hi *.dyn_*

clean:
	@rm -f *.o *.hi *.dyn_* client test

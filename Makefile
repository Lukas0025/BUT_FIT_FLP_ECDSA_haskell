.PHONY: clean build

build: clean
	mkdir build
	ghc -dynamic -Wall -outputdir build/ src/* -o flp22-fun

test:
	./flp22-fun -2 test.in

clean:
	rm -rf build
	rm -f app
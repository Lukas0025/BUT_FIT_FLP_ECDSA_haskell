.PHONY: clean build

build: clean
	mkdir build
	ghc -dynamic --make -outputdir build/ src/* -o app

clean:
	rm -rf build
	rm -f app
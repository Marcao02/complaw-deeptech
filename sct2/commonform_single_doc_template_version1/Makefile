
all: build

clean:
	rm -rf generated build

build:
	mkdir -p build
	cp package.json tsconfig.json build/
	cp Makefile.build build/Makefile
	cd build; npm install --only=dev
	$(MAKE) -C build
	

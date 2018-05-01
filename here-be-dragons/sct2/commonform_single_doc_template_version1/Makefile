
all: generated build

clean:
	rm -rf generated build

generated:
	mkdir generated
	cp Makefile.generated generated/Makefile
	$(MAKE) -C generated
	python gen_SAFE.py

build: generated
	mkdir -p build
	cp package.json tsconfig.json build/
	cp Makefile.build build/Makefile
	cd build; npm install
	$(MAKE) -C build
	

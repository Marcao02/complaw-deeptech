
all: generated build

clean:
	rm -rf generated build

generated:
	mkdir generated
	python gen_SAFE.py

build:
	mkdir -p build
	cp package.json tsconfig.json build/
	cp Makefile.build build/Makefile
	cd build; npm install
	$(MAKE) -C build
	

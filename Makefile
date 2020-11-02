IDRIS2 := idris2

.PHONY: build install clean

build:
	${IDRIS2} --build oyster.ipkg

install: 
	${IDRIS2} --install oyster.ipkg

uninstall:
	echo ${IDRIS2_HOME}

clean:
	rm -rf build
	rm -rf samples/build
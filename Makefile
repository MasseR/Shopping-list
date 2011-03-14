TESTDEST=/var/www/lighttpd
DEST=${HOME}/programming/haskell/site/ostoslista
all: ostoslista.cgi

.PHONY: testinstall install uninstall clean
install: ostoslista-static
	install ostoslista-static ${DEST}/cgi-bin/ostoslista.cgi

testinstall: ostoslista.cgi list
	install -o lighttpd -g lighttpd ostoslista.cgi ${TESTDEST}/ostoslista.cgi
	strip ${TESTDEST}/ostoslista.cgi
	install -o lighttpd -g lighttpd list ${TESTDEST}/list

list:
	echo "[]" > list
uninstall:
	rm -f ${TESTDEST}/ostoslista.cgi
	rm -f ${TESTDEST}/list

clean:
	rm -f ostoslista.cgi
	rm -f ostoslista.hi
	rm -f ostoslista.o
	rm -f list

ostoslista.cgi: ostoslista.hs
	ghc --make -O2 -o ostoslista.cgi ostoslista.hs

ostoslista-static: ostoslista.hs
	ghc --make -O2 -o ostoslista-static -static -optl-static -optl-pthread ostoslista.hs

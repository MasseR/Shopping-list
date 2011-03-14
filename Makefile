DEST=/var/www/lighttpd
all: ostoslista.cgi

.PHONY: install uninstall clean
install: ostoslista.cgi list
	install -o lighttpd -g lighttpd ostoslista.cgi ${DEST}/ostoslista.cgi
	strip ${DEST}/ostoslista.cgi
	install -o lighttpd -g lighttpd list ${DEST}/list

list:
	echo "[]" > list
uninstall:
	rm -f ${DEST}/ostoslista.cgi
	rm -f ${DEST}/list

clean:
	rm -f ostoslista.cgi
	rm -f ostoslista.hi
	rm -f ostoslista.o
	rm -f list

ostoslista.cgi: ostoslista.hs
	ghc --make -O2 -o ostoslista.cgi ostoslista.hs

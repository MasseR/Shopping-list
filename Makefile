TESTDEST=/var/www/lighttpd
DEST=${HOME}/programming/haskell/site/ostoslista
CSS=css/style.css css/jquery.autocomplete.css
JS=js/setfocus.js js/autocomplete.js js/jquery.autocomplete.js
STATIC=${CSS} ${JS}
all: ostoslista-debug

.PHONY: testinstall install uninstall clean test
install: ostoslista-static
	mkdir -p ${DEST}/
	install ostoslista-static ${DEST}/ostoslista.cgi
	strip ${DEST}/ostoslista.cgi

testinstall: ostoslista-debug list
	install -o lighttpd -g lighttpd ostoslista-debug ${TESTDEST}/ostoslista.cgi
	for file in $(STATIC); do\
	    echo "Installing  $$file"; \
	    install -o lighttpd -g lighttpd $$file ${TESTDEST}/$$file; \
	done;
	strip ${TESTDEST}/ostoslista.cgi
	install -o lighttpd -g lighttpd list ${TESTDEST}/list

list:
	echo "[]" > list
uninstall:
	rm -f ${TESTDEST}/ostoslista.cgi
	rm -f ${TESTDEST}/list

clean:
	find ${PWD} -iname '*.hi' -or -iname '*.o' | xargs rm
	rm -f ostoslista-static
	rm -f ostoslista-debug
	rm -f list

ostoslista-debug: ostoslista.hs Transaction.hs Data/ShoppingList.hs Data/ShoppingList/Persist.hs Network/CGI/Text.hs
	ghc --make -O2 -rtsopts -o $@ ostoslista.hs

ostoslista-static: ostoslista.hs Transaction.hs Transaction.hs Data/ShoppingList.hs Data/ShoppingList/Persist.hs Network/CGI/Text.hs
	ghc --make -O2 -o $@ -static -optl-static -optl-pthread ostoslista.hs

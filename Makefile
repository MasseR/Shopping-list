TESTDEST=/var/www/lighttpd
DEST=${HOME}/programming/haskell/site/ostoslista
CSS=css/style.css css/jquery.autocomplete.css
JS=js/setfocus.js js/autocomplete.js js/jquery.autocomplete.js
STATIC=${CSS} ${JS}
all: debug

debug: ostoslista-debug completion-debug
deploy: ostoslista-static completion-static

.PHONY: testinstall install uninstall clean test
install: deploy
	mkdir -p ${DEST}/
	install ostoslista-static ${DEST}/ostoslista.cgi
	install completion-static ${DEST}/autocomplete.cgi
	for file in $(STATIC); do\
	    echo "Installing  $$file"; \
	    install -o lighttpd -g lighttpd $$file ${DEST}/$$file; \
	done;
	strip ${DEST}/ostoslista.cgi
	strip ${DEST}/autocomplete.cgi

testinstall: debug
	install -o lighttpd -g lighttpd ostoslista-debug ${TESTDEST}/ostoslista.cgi
	install -o lighttpd -g lighttpd completion-debug ${TESTDEST}/autocomplete.cgi
	for file in $(STATIC); do\
	    echo "Installing  $$file"; \
	    install -o lighttpd -g lighttpd $$file ${TESTDEST}/$$file; \
	done;
	strip ${TESTDEST}/ostoslista.cgi

list:
	echo "[]" > list
uninstall:
	rm -f ${TESTDEST}/ostoslista.cgi
	rm -f ${TESTDEST}/list

clean:
	find ${PWD} -iname '*.hi' -or -iname '*.o' | xargs rm
	rm -f ostoslista-static
	rm -f ostoslista-debug
	rm -f completion-debug
	rm -f completion-static
	rm -f list

completion-debug: autocomplete.hs Data/ShoppingList.hs
	ghc --make -O2 -rtsopts -o $@ autocomplete.hs

completion-static: autocomplete.hs Data/ShoppingList.hs
	ghc --make -O2 -o $@ -optl-static -optl-pthread autocomplete.hs

ostoslista-debug: ostoslista.hs Transaction.hs Data/ShoppingList.hs Data/ShoppingList/Persist.hs Network/CGI/Text.hs
	ghc --make -O2 -rtsopts -o $@ ostoslista.hs

ostoslista-static: ostoslista.hs Transaction.hs Transaction.hs Data/ShoppingList.hs Data/ShoppingList/Persist.hs Network/CGI/Text.hs
	ghc --make -O2 -o $@ -static -optl-static -optl-pthread ostoslista.hs

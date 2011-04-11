VERSION=`perl -ne 'print $$1 if /;; Version: (.*)/' auto-complete.el`
PACKAGE=auto-complete-${VERSION}

byte-compile:
	/Applications/Emacs.app/Contents/MacOS/Emacs -Q -L . -batch -f batch-byte-compile *.el

install:
	/Applications/Emacs.app/Contents/MacOS/Emacs -Q -L . -batch -l etc/install ${DIR}

clean:
	rm -f *.elc
	rm -f doc/*.html
	rm -rf ${PACKAGE}
	rm -f ${PACKAGE}.zip ${PACKAGE}.tar.bz2

package: clean
	mkdir ${PACKAGE}
	cp -r *.el Makefile README.txt TODO.txt COPYING.*.txt doc etc dict ${PACKAGE}

tar.bz2: package
	tar cjf ${PACKAGE}.tar.bz2 ${PACKAGE}

zip: package
	zip -r ${PACKAGE}.zip ${PACKAGE}

# Compiling AUCTeX

AUCTeX is included as a submodule in src/auctec-src but must be compiled in order to work. To force auctex to compile everything in the local tree, configure it as follows (inside src/auctex-src):

$ ./configure --prefix=/Users/kjhealy/.emacs.d/src --without-texmf-dir --with-emacs=/Applications/Emacs.app/Contents/MacOS/Emacs --with-lispdir=/Users/kjhealy/.emacs.d/src

This should all be on one line and assumes your Emacs is a recent Emacs.app on Mac OS X. 

Then doing 

make
make install

will put everything in src/, with a new auctex/ directory and a bunch of manuals and docfiles in src/share/ and src/var, together with auctex.el, preview-latex.el, and tex-site.el in /src.

It's a complete pain, but works. Roll on including AUCTeX by default in Emacs.

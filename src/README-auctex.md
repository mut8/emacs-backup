# Compiling AUCTeX

AUCTeX is included as a submodule in src/auctec-src but must be compiled in order to work. To force auctex to compile everything in the local tree, configure it as follows (inside src/auctex-src). Replace the two instances of USERNAME in the ./configure command with the name of your home directory. To get that, from the terminal do 

$ cd ~/.emacs.d/src/
$ pwd  

Then do

$ cd ~/.emacs.d/src/auctex-src

$ ./configure --prefix=/Users/USERNAME/.emacs.d/src --without-texmf-dir --with-emacs=/Applications/Emacs.app/Contents/MacOS/Emacs --with-lispdir=/Users/USERNAME/.emacs.d/src

This last command should all be on one line and it assumes your Emacs is a recent Emacs.app on Mac OS X in the Applications/ directory. 

Then doing 

make
make install

will put everything in src/ in a new auctex/ directory, and a bunch of manuals and docfiles in src/share/ and src/var, together with auctex.el, preview-latex.el, and tex-site.el in /src. When you run the starter kit, Emacs will find it properly.

It's a complete pain, but works. Roll on including AUCTeX by default in Emacs.

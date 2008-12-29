;; Kieran Healy's .emacs file
;; Made to be used together with
;; http://github.com/technomancy/emacs-starter-kit/tree/master
;; dotemacs.el should be ln -s ~/elisp/dotemacs.el ~/.emacs

;; identity
(setq user-full-name "Kieran Healy")
(setq user-mail-address "kjhealy@gmail.com")
(setq mail-host-address "gmail.com")

;; identity for stater-kit customization file
(setq system-name "iolar")
(setq user-login-name "kjhealy")

;; Start the Emacs server (needed for synctex/skim integration below)
(server-start)

;; location of various local packages (in elisp/vendor)
;; because I don't want to keep them in /Applications/Emacs.app/ or in /usr/share/local/
(add-to-list 'load-path "~/elisp")
(progn (cd "~/elisp") (normal-top-level-add-subdirs-to-load-path))

;; starter-kit files and all further customizations
(load-file "~/.emacs.d/init.el")



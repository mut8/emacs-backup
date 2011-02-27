;;; init.el --- Where all the magic begins
;;
;; Part of the Emacs Starter Kit
;;
;; This is the first thing to get loaded.
;;

;; org-mode windmove compatibility
(setq org-replace-disputed-keys t)

;; setq dotfiles-dir (file-name-directory (or (buffer-file-name) load-file-name)))
(setq dotfiles-dir (file-name-directory (or load-file-name (buffer-file-name))))


(add-to-list 'load-path (expand-file-name
                         "lisp" (expand-file-name
                                 "org" (expand-file-name
                                        "src" dotfiles-dir))))

;; Package Locations
;; Location of various local packages (in .emacs.d/vendor or .emacs.d/src)
;;  because I don't want to keep them in =/Applications/Emacs.app/= or in
;;  =/usr/share/local/=.

(if (fboundp 'normal-top-level-add-subdirs-to-load-path)
    (let* ((my-lisp-dir "~/.emacs.d/")
           (default-directory my-lisp-dir))
      (setq load-path (cons my-lisp-dir load-path))
      (normal-top-level-add-subdirs-to-load-path)))

;; Font-face setup. If PragmataPro exists, set it as the default font
;; immediately, together with the background and foreground colors
;; we'll use later when the color theme is set. I do this here to
;; prevent some of the irritating flickering and resizing that
;; otherwise goes on during startup. If you don't have PragmataPro
;; installed, nothing will happen here. Alternatively, you can replace
;; "PragmataPro" with the name of your font of choice.
(defun font-existsp (font)
  (if (null (x-list-fonts font))
      nil t))

(if (font-existsp "PragmataPro")
    (set-face-attribute 'default nil :background "#1E1E1E" :foreground "#CACACA" :height 141 :font "PragmataPro"))

;; Load up Org Mode and Babel
(require 'org-install)

;; load up the main file
(org-babel-load-file (expand-file-name "starter-kit.org" dotfiles-dir))

;;; init.el ends here

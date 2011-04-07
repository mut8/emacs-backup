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

;; Font-face setup. Check the availability of a some default fonts, in
;; order of preference. The first of these alternatives to be found is
;; set as the default font, together with base size and fg/bg
;; colors. If none of the preferred fonts is found, nothing happens
;; and Emacs carries on with the default setup. We do this here to
;; prevent some of the irritating flickering and resizing that
;; otherwise goes on during startup. You can reorder or replace the
;; options here with the names of your preferred choices.

(defun font-existsp (font)
  "Check to see if the named FONT is available."
  (if (null (x-list-fonts font))
      nil t))

;; Set default font. First one found is selected.
(cond
 ((eq window-system nil) nil)
 ((font-existsp "PragmataPro")
  (set-face-attribute 'default nil :height 141 :font "PragmataPro"))
 ((font-existsp "Menlo")
  (set-face-attribute 'default nil :height 141 :font "Menlo"))
 ((font-existsp "Consolas")
  (set-face-attribute 'default nil :height 141 :font "Consolas"))
 ((font-existsp "Inconsolata")
  (set-face-attribute 'default nil :height 141 :font "Inconsolata"))
 )

;; Load up Org Mode and Babel
(require 'org-install)

;; load up the main file
(org-babel-load-file (expand-file-name "starter-kit.org" dotfiles-dir))

;;; init.el ends here

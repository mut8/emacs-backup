;; Start the Emacs server (needed for synctex/skim integration below)
(server-start)

;; Auto-raise Emacs on activation (from Skim, usually)
(defun raise-emacs-on-aqua() 
    (shell-command "osascript -e 'tell application \"Emacs\" to activate' &"))
(add-hook 'server-switch-hook 'raise-emacs-on-aqua)


;; local load path 
(add-to-list 'load-path "~/elisp")
(progn (cd "~/elisp") (normal-top-level-add-subdirs-to-load-path))

;; no splash screen
(setq inhibit-splash-screen t)

;; identity
(setq user-full-name "Kieran Healy")
(setq user-mail-address "kjhealy@gmail.com")
(setq mail-host-address "gmail.com")

;; Auto fill mode --- automatic line wrapping.
(add-hook 'text-mode-hook
  '(lambda () (auto-fill-mode 1))) 

;; Color Theme
(require 'color-theme)
(color-theme-initialize)
(load-file "~/elisp/custom-color-themes.el")
(color-theme-twilighter)


;; Git managed version control
;(load-file "~/elisp/dvc/dvc-load.el") 
;(require 'dvc-autoloads)
(load-file "~/elisp/magit//magit.el")
(require 'magit)

;; AUCTeX
(load "auctex.el" nil t t)
(load "preview-latex.el" nil t t)

; Synctex with Skim
(require 'tex-site)
(add-hook 'TeX-mode-hook
    (lambda ()
        (add-to-list 'TeX-output-view-style
            '("^pdf$" "."
              "/Applications/Skim.app/Contents/SharedSupport/displayline %n %o %b")))
)

; Make XeLaTeX the default latex engine
(setq-default TeX-engine "xetex")
(setq-default LaTeX-XeTeX-command "xelatex -synctex=1")

;; ESS
(load "~/elisp/ess/lisp/ess-site.el")

;; R-noweb mode, for Sweave.
 (defun Rnw-mode ()
   (require 'ess-noweb)
   (noweb-mode)
   (if (fboundp 'R-mode)
       (setq noweb-default-code-mode 'R-mode)))

 (add-to-list 'auto-mode-alist '("\\.Rnw\\'" . Rnw-mode))
 (add-to-list 'auto-mode-alist '("\\.Snw\\'" . Rnw-mode))

 (setq reftex-file-extensions
       '(("Snw" "Rnw" "nw" "tex" ".tex" ".ltx") ("bib" ".bib")))
 (setq TeX-file-extensions
       '("Snw" "Rnw" "nw" "tex" "sty" "cls" "ltx" "texi" "texinfo"))

;; (add-hook 'LaTeX-mode-hook
;;           (function (lambda ()
;;                       (add-to-list 'LaTeX-command-style
;;                                    '("\\`fontspec\\'" "xelatex %S%(PDFout)"))
;;                       )
;;                     )     
;; )

  (autoload 'reftex-mode     "reftex" "RefTeX Minor Mode" t)
  (autoload 'turn-on-reftex  "reftex" "RefTeX Minor Mode" nil)
  (autoload 'reftex-citation "reftex-cite" "Make citation" nil)
  (autoload 'reftex-index-phrase-mode "reftex-index" "Phrase mode" t)
  (add-hook 'LaTeX-mode-hook 'turn-on-reftex)   ; with AUCTeX LaTeX mode
  (add-hook 'latex-mode-hook 'turn-on-reftex)   ; with Emacs latex mode

;; Make RefTeX faster
   (setq reftex-enable-partial-scans t)
   (setq reftex-save-parse-info t)
   (setq reftex-use-multiple-selection-buffers t)
   (setq reftex-plug-into-AUCTeX t)

;; Make RefTex able to find my local bib files
   (setq reftex-bibpath-environment-variables
      '("/Users/kjhealy/Documents/bibs/"))

;; RefTex format for biblatex
(setq reftex-cite-format 
  '((?\C-m . "\\cite[]{%l}") 
   (?t    . "\\textcite[]{%l}") 
   (?p    . "\\parencite[]{%l}") 
   (?o    . "\\citepr[]{%l}") 
   (?n    . "\\nocite{%l}"))) 
(setq reftex-cite-prompt-optional-args t) 

; put as much syntax highlighting into documents as possible
(require 'font-lock)

; (global-font-lock-mode 3)
(if (fboundp 'global-font-lock-mode)
    (global-font-lock-mode 1); Emacs
  (setq font-lock-auto-fontify t)); XEmacs

;; PDF mode for latex
(setq-default TeX-PDF-mode t)

;; Make emacs aware of multi-file projects
(setq-default TeX-master nil)

;; markdown mode
(autoload 'markdown-mode "markdown-mode.el"
   "Major mode for editing Markdown files" t)
(setq auto-mode-alist
   (cons '("\\.Markdown" . markdown-mode) auto-mode-alist)
)
(setq auto-mode-alist
   (cons '("\\.MarkDown" . markdown-mode) auto-mode-alist)
)
(setq auto-mode-alist
   (cons '("\\.markdown" . markdown-mode) auto-mode-alist)
)
(setq auto-mode-alist
   (cons '("\\.md" . markdown-mode) auto-mode-alist)
)

;; Misc
; Make all "yes or no" prompts show "y or n" instead
(fset 'yes-or-no-p 'y-or-n-p)

; sane line wrapping at last
(global-visual-line-mode t)

; parenthesis matching by default
(show-paren-mode t)

;;name and path of file in title bar
(setq frame-title-format '("%S:" (buffer-file-name "%f" (dired-directory dired-directory "%b"))))

; turn on recent files menu
(require 'recentf)
(recentf-mode 1)

; use cocoAspell instead of ispell
(setq ispell-program-name "~/Library/PreferencePanes/Spelling.prefPane/Contents/MacOS/cocoAspell")

;; ispell --- make ispell skip \citep, \citet etc in .tex files.
(setq ispell-tex-skip-alists
  '((;;("%\\[" . "%\\]") ; AMStex block comment...
     ;; All the standard LaTeX keywords from L. Lamport's guide:
     ;; \cite, \hspace, \hspace*, \hyphenation, \include, \includeonly, \input,
     ;; \label, \nocite, \rule (in ispell - rest included here)
     ("\\\\addcontentsline"              ispell-tex-arg-end 2)
     ("\\\\add\\(tocontents\\|vspace\\)" ispell-tex-arg-end)
     ("\\\\\\([aA]lph\\|arabic\\)"	 ispell-tex-arg-end)
     ;;("\\\\author"			 ispell-tex-arg-end)
     ;; New regexp here --- kjh
     ("\\\\cite\\(t\\|p\\|year\\|yearpar\\)" ispell-tex-arg-end)
     ("\\\\bibliographystyle"		 ispell-tex-arg-end)
     ("\\\\makebox"			 ispell-tex-arg-end 0)
     ("\\\\e?psfig"			 ispell-tex-arg-end)
     ("\\\\document\\(class\\|style\\)" .
      "\\\\begin[ \t\n]*{[ \t\n]*document[ \t\n]*}"))
    (;; delimited with \begin.  In ispell: displaymath, eqnarray, eqnarray*,
     ;; equation, minipage, picture, tabular, tabular* (ispell)
     ("\\(figure\\|table\\)\\*?"	 ispell-tex-arg-end 0)
     ("list"				 ispell-tex-arg-end 2)
     ("program"		. "\\\\end[ \t\n]*{[ \t\n]*program[ \t\n]*}")
     ("verbatim\\*?"	. "\\\\end[ \t\n]*{[ \t\n]*verbatim\\*?[ \t\n]*}"))))


(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(LaTeX-XeTeX-command "xelatex -synctex=1")
 '(TeX-engine (quote xetex))
 '(text-mode-hook (quote (turn-on-auto-fill text-mode-hook-identify))))
(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 )


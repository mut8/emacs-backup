;; Kieran Healy's .emacs file
;; Made to be used together with http://github.com/technomancy/emacs-starter-kit/tree/master

;; location of various local packages (in elisp/vendor)
;; because I don't want to keep them in /Applications/Emacs.app/ or in
;; /usr/share/local/
(if (fboundp 'normal-top-level-add-subdirs-to-load-path)
        (let* ((my-lisp-dir "~/elisp/")
              (default-directory my-lisp-dir))
           (setq load-path (cons my-lisp-dir load-path))
           (normal-top-level-add-subdirs-to-load-path)))

;; Color Theme
(require 'color-theme)
(load-file "~/elisp/color-custom/color-theme-blackboard-kjh.el")
(color-theme-blackboardkjh)

;; require maxframe package to conveniently maxmimize the window with M-x mf
(require 'maxframe)
(global-set-key (kbd "C-c m") 'x-maximize-frame)

;; resizing 'windows' (i.e., inside the frame)
(global-set-key (kbd "S-C-<left>") 'shrink-window-horizontally)
(global-set-key (kbd "S-C-<right>") 'enlarge-window-horizontally)
(global-set-key (kbd "S-C-<down>") 'shrink-window)
(global-set-key (kbd "S-C-<up>") 'enlarge-window)

(require 'highline)
(highline-mode 1)
 ;; To customize the background color
(set-face-background 'highline-face "#262626")

;; fn to turn off highline in specific modes
(defun highline-mode-off () (highline-mode 0))
;; Turn off local highlighting for LaTeX, Magit, eshell
(add-hook 'magit-mode-hook #'highline-mode-off)
(add-hook 'eshell-mode-hook #'highline-mode-off)
;;(add-hook 'TeX-mode-hook #'highline-mode-off)

;;egg magit with more bling (a bit too much, so we turned it off)
;;(require 'egg)


;;; -----------------------------
;;; LATEX STUFF
;;; -----------------------------

;; AUCTeX
(load "auctex.el" nil t t)
(load "preview-latex.el" nil t t)

;; Turn off hl-line when writing latex 
;;(add-hook 'TeX-mode-hook 'local-hl-line-mode-off)


;; Synctex with Skim
(require 'tex-site)
(add-hook 'TeX-mode-hook
          (lambda ()
            (add-to-list 'TeX-output-view-style
                         '("^pdf$" "."
                           "/Applications/Skim.app/Contents/SharedSupport/displayline %n %o %b")))
          )

;; Make XeLaTeX the default latex engine
(setq-default TeX-engine "xetex")
(setq-default LaTeX-XeTeX-command "xelatex -synctex=1")

;; PDF mode for latex
(setq-default TeX-PDF-mode t)

;; Make emacs aware of multi-file projects
(setq-default TeX-master nil)

;; Auto-raise Emacs on activation (from Skim, usually)
(defun raise-emacs-on-aqua()
  (shell-command "osascript -e 'tell application \"Emacs\" to activate' &"))
(add-hook 'server-switch-hook 'raise-emacs-on-aqua)

;; RefTeX
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
      '("/Users/kjhealy/Library/texmf/bibtex/bib"))

;; Default bibliography
(setq reftex-default-bibliography
      '("/Users/kjhealy/Documents/bibs/socbib.bib"))

;(setq reftex-bibpath-environment-variables
;      '("/Users/kjhealy/Documents/bibs:"))

;; RefTeX formats for biblatex
(setq reftex-cite-format
      '((?\C-m . "\\cite[]{%l}")
        (?t    . "\\textcite[]{%l}")
        (?p    . "\\parencite[]{%l}")
        (?o    . "\\citepr[]{%l}")
        (?n    . "\\nocite{%l}")))
(setq reftex-cite-prompt-optional-args t)

;; CDLaTex minor mode: tab-trigger environments, paired paren
;; insertion, etc

(autoload 'cdlatex-mode "cdlatex" "CDLaTeX Mode" t)
(autoload 'turn-on-cdlatex "cdlatex" "CDLaTeX Mode" nil)
(add-hook 'LaTeX-mode-hook 'turn-on-cdlatex)   ; with AUCTeX LaTeX
                                        ; mode
;; (setq cdlatex-paired-parens
;;        '("$[{(")) ; set which characters are autopaired SET IN
;; CUSTOM.EL don't know why it's not working here.


;;; -----------------------------
;;; R STUFF
;;; -----------------------------

;; ESS: Emacs Speaks Statistics
(load "~/elisp/vendor/ess/lisp/ess-site.el") 

;; roxygen more for generating Rd and packages
(require 'ess-roxygen)

;; R-noweb mode, for Sweave files.
(defun Rnw-mode ()
  (require 'ess-noweb)
  (noweb-mode)
  (if (fboundp 'R-mode)
      (setq noweb-default-code-mode 'R-mode)))

(add-to-list 'auto-mode-alist '("\\.Rnw\\'" . Rnw-mode))
(add-to-list 'auto-mode-alist '("\\.Snw\\'" . Rnw-mode))

;; Make TeX and RefTex aware of Snw and Rnw files
(setq reftex-file-extensions
      '(("Snw" "Rnw" "nw" "tex" ".tex" ".ltx") ("bib" ".bib")))
(setq TeX-file-extensions
      '("Snw" "Rnw" "nw" "tex" "sty" "cls" "ltx" "texi" "texinfo"))

;;; -----------------------------
;;; Markdown documents
;;; -----------------------------

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

;;; -----------------------------
;; Misc things not in starter-kit,
;; or starter-kit overrides
;;; -----------------------------

;; use ido mode for M-command completion
;; as well as file/buffer completion
(setq ido-execute-command-cache nil)
(defun ido-execute-command ()
  (interactive)
  (call-interactively
   (intern
    (ido-completing-read
     "M-x "
     (progn
       (unless ido-execute-command-cache
         (mapatoms (lambda (s)
                     (when (commandp s)
                       (setq ido-execute-command-cache
                             (cons (format "%S" s) ido-execute-command-cache))))))
       ido-execute-command-cache)))))

(add-hook 'ido-setup-hook
          (lambda ()
            (setq ido-enable-flex-matching t)
            (global-set-key "\M-x" 'ido-execute-command)))

(add-hook 'ido-setup-hook 
          (lambda () 
            (define-key ido-completion-map [tab] 'ido-complete)))

                                        ; Sane line wrapping for long documents and papers
(global-visual-line-mode t)

                                        ; Use cocoAspell instead of ispell
(setq ispell-program-name "~/Library/PreferencePanes/Spelling.prefPane/Contents/MacOS/cocoAspell")

;; ispell --- make ispell skip \citep, \citet etc in .tex files.
(setq ispell-tex-skip-alists
      '((;;("%\\[" . "%\\]") ; AMStex block comment...
         ;; All the standard LaTeX keywords from L. Lamport's guide:
         ;; \cite, \hspace, \hspace*, \hyphenation, \include, \includeonly, \input,
         ;; \label, \nocite, \rule (in ispell - rest included here)
         ("\\\\addcontentsline"              ispell-tex-arg-end 2)
         ("\\\\add\\(tocontents\\|vspace\\)" ispell-tex-arg-end)
         ("\\\\\\([aA]lph\\|arabic\\)"   ispell-tex-arg-end)
         ;;("\\\\author"                         ispell-tex-arg-end)
         ;; New regexps here --- kjh
         ("\\\\\\(text\\|paren\\)cite" ispell-tex-arg-end)
         ("\\\\cite\\(t\\|p\\|year\\|yearpar\\)" ispell-tex-arg-end)
         ("\\\\bibliographystyle"                ispell-tex-arg-end)
         ("\\\\makebox"                  ispell-tex-arg-end 0)
         ("\\\\e?psfig"                  ispell-tex-arg-end)
         ("\\\\document\\(class\\|style\\)" .
          "\\\\begin[ \t\n]*{[ \t\n]*document[ \t\n]*}"))
        (;; delimited with \begin.  In ispell: displaymath, eqnarray, eqnarray*,
         ;; equation, minipage, picture, tabular, tabular* (ispell)
         ("\\(figure\\|table\\)\\*?"     ispell-tex-arg-end 0)
         ("list"                                 ispell-tex-arg-end 2)
         ("program"             . "\\\\end[ \t\n]*{[ \t\n]*program[ \t\n]*}")
         ("verbatim\\*?"        . "\\\\end[ \t\n]*{[ \t\n]*verbatim\\*?[ \t\n]*}"))))


;; Enable skeleton mode in ESS for paired insertion
(require 'skeleton)
(dolist (hook '(ess-mode-hook
                inferior-ess-mode-hook))
  (add-hook hook (lambda ()
              (make-local-variable 'skeleton-pair)
              (make-local-variable 'skeleton-pair-on-word)
              (make-local-variable 'skeleton-pair-filter-function)
              (make-local-variable 'skeleton-pair-alist)
              (setq skeleton-pair-on-word t
                    skeleton-pair t)
              (local-set-key (kbd "\"") 'skeleton-pair-insert-maybe)
              (local-set-key (kbd "(") 'skeleton-pair-insert-maybe)
              (local-set-key (kbd "(") 'skeleton-pair-insert-maybe)
              (local-set-key (kbd "[") 'skeleton-pair-insert-maybe)
              (local-set-key (kbd "{") 'skeleton-pair-insert-maybe)))
  )


;; Put the menu bar back
(menu-bar-mode 1)

;; Base dir
(cd "~/")


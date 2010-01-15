;; Kieran Healy's .emacs file
;; Made to be used together with http://github.com/technomancy/emacs-starter-kit/tree/master

;; location of various local packages (in elisp/vendor)
;; because I don't want to keep them in /Applications/Emacs.app/ or in
;; /usr/share/local/
(if (fboundp 'normal-top-level-add-subdirs-to-load-path)
        (let* ((my-lisp-dir "~/.emacs.d/")
              (default-directory my-lisp-dir))
           (setq load-path (cons my-lisp-dir load-path))
           (normal-top-level-add-subdirs-to-load-path)))

;; magit
(require 'magit)

;; Color Theme from vendor directory
(add-to-list 'load-path "~/.emacs.d/vendor/color-theme/")
(require 'color-theme)
(eval-after-load "color-theme"
  '(progn
     (color-theme-initialize))
 )

(load-file "~/.emacs.d/color-custom/color-theme-twilight.el")
(color-theme-twilight)

;; Highlight current line (twilight theme builds in support for this)
;;(global-hl-line-mode 1)
(require 'highline)
(highline-mode 1)

;; To customize the background color
(set-face-background 'highline-face "#2D2D2D")

;; require maxframe package to conveniently maxmimize the window with M-x mf
(require 'maxframe)
(global-set-key (kbd "C-c m") 'x-maximize-frame)

;; resizing 'windows' (i.e., inside the frame)
(global-set-key (kbd "S-C-<left>") 'shrink-window-horizontally)
(global-set-key (kbd "S-C-<right>") 'enlarge-window-horizontally)
(global-set-key (kbd "S-C-<down>") 'shrink-window)
(global-set-key (kbd "S-C-<up>") 'enlarge-window)

;;; -----------------------------
;;; Windmove
;;; -----------------------------

;; If you don't set this, you keep getting elisp errors when you try
;; to wrap around. 
(setq windmove-wrap-around t)

;;; -----------------------------
;;; LATEX STUFF
;;; -----------------------------

;; AUCTeX
(load "auctex.el" nil t t)
(load "preview-latex.el" nil t t)

;; Synctex with Skim
(require 'tex-site)
(add-hook 'TeX-mode-hook
          (lambda ()
            (add-to-list 'TeX-output-view-style
                         '("^pdf$" "."
                           "/Applications/Skim.app/Contents/SharedSupport/displayline %n %o %b")))
          )

;; PDF mode for latex
(setq-default TeX-PDF-mode t)

;; Make emacs aware of multi-file projects
(setq-default TeX-master nil)


;; Make XeLaTeX the default latex engine // Doesn't work. Set in
;; custom below instead.
;; (add-hook 'TeX-mode-hook
;;           (function (lambda ()
;;                       (setq TeX-engine "xetex")
;;                       (setq LaTeX-XeTeX-command "xelatex -synctex=1")
;;               )
;;             )
;;   )

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

;; RefTeX formats for biblatex (not natbib)
(setq reftex-cite-format
     '(
       (?\C-m . "\\cite[]{%l}")
       (?t . "\\textcite{%l}")
       (?a . "\\autocite[]{%l}")
       (?p . "\\parencite{%l}")
       (?f . "\\footcite[][]{%l}")
       (?F . "\\fullcite[]{%l}")
       (?x . "[]{%l}")
       (?X . "{%l}")
       ))

(setq font-latex-match-reference-keywords
'(("cite" "[{")
  ("cites" "[{}]")
        ("footcite" "[{")
        ("footcites" "[{")
        ("parencite" "[{")
        ("textcite" "[{")
        ("fullcite" "[{") 
        ("citetitle" "[{") 
        ("citetitles" "[{") 
        ("headlessfullcite" "[{")))

(setq reftex-cite-prompt-optional-args nil)
(setq reftex-cite-cleanup-optional-args t)

;; CDLaTex minor mode: tab-trigger environments, paired paren
;; insertion, etc
;; (autoload 'cdlatex-mode "cdlatex" "CDLaTeX Mode" t)
;; (autoload 'turn-on-cdlatex "cdlatex" "CDLaTeX Mode" nil)
;; (add-hook 'LaTeX-mode-hook 'turn-on-cdlatex)   ; with AUCTeX LaTeX
;;                                         ; mode

;; set which characters are autopaired // Doesn't work. Set in custom, below.
;; (add-hook 'cdlatex-mode-hook
;;   (function (lambda ()
;;               (setq cdlatex-paired-parens
;;                     '("$[{("))
;;             )))

;;; -----------------------------
;;; R STUFF
;;; -----------------------------

;; ESS: Emacs Speaks Statistics
(load "~/elisp/vendor/ess/lisp/ess-site.el") 

;; Use shift-enter to split window & launch R (if not running), execute highlighted
;; region (if R running & area highlighted), or execute current line
;; (and move to next line, skipping comments). Nice. 
;; See http://www.emacswiki.org/emacs/EmacsSpeaksStatistics,
;; FelipeCsaszar. Adapted to spilit vertically instead of
;; horizontally. 

(setq ess-ask-for-ess-directory nil)
  (setq ess-local-process-name "R")
  (setq ansi-color-for-comint-mode 'filter)
  (setq comint-prompt-read-only t)
  (setq comint-scroll-to-bottom-on-input t)
  (setq comint-scroll-to-bottom-on-output t)
  (setq comint-move-point-for-output t)
  (defun my-ess-start-R ()
    (interactive)
    (if (not (member "*R*" (mapcar (function buffer-name) (buffer-list))))
      (progn
	(delete-other-windows)
	(setq w1 (selected-window))
	(setq w1name (buffer-name))
	(setq w2 (split-window w1 nil t))
	(R)
	(set-window-buffer w2 "*R*")
	(set-window-buffer w1 w1name))))
  (defun my-ess-eval ()
    (interactive)
    (my-ess-start-R)
    (if (and transient-mark-mode mark-active)
	(call-interactively 'ess-eval-region)
      (call-interactively 'ess-eval-line-and-step)))
  (add-hook 'ess-mode-hook
	    '(lambda()
	       (local-set-key [(shift return)] 'my-ess-eval)))
  (add-hook 'inferior-ess-mode-hook
	    '(lambda()
	       (local-set-key [C-up] 'comint-previous-input)
	       (local-set-key [C-down] 'comint-next-input)))
  (require 'ess-site)

;; roxygen more for generating Rd and packxfages
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
            (global-set-key "\M-x" 'ido-execute-command)
            (define-key ido-completion-map [tab] 'ido-complete)))

(setq ido-work-directory-list '("~/" "~/Desktop" "~/Documents"))

;; Sane line wrapping for long documents and papers
(global-visual-line-mode t)

;; Use cocoAspell instead of ispell
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
(setq skeleton-pair t)
(defvar my-skeleton-pair-alist
  '((?\) . ?\()
    (?\] . ?\[)
    (?} . ?{)
    (?" . ?")))

(defun my-skeleton-pair-end (arg)
  "Skip the char if it is an ending, otherwise insert it."
  (interactive "*p")
  (let ((char last-command-char))
    (if (and (assq char my-skeleton-pair-alist)
             (eq char (following-char)))
        (forward-char)
      (self-insert-command (prefix-numeric-value arg)))))

(dolist (pair my-skeleton-pair-alist)
  (global-set-key (char-to-string (first pair))
                  'my-skeleton-pair-end)
  ;; If the char for begin and end is the same,
  ;; use the original skeleton
  (global-set-key (char-to-string (rest pair))
                  'skeleton-pair-insert-maybe))

;;; prefer auto-fill to visual line wrap in ESS mode
(add-hook 'ess-mode-hook 'turn-on-auto-fill)
(add-hook 'inferior-ess-mode-hook 'turn-on-auto-fill) 

;; Page down/up move the point, not the screen.
;; In practice, this means that they can move the
;; point to the beginning or end of the buffer.
(global-set-key [next]
  (lambda () (interactive)
    (condition-case nil (scroll-up)
      (end-of-buffer (goto-char (point-max))))))

(global-set-key [prior]
  (lambda () (interactive)
    (condition-case nil (scroll-down)
      (beginning-of-buffer (goto-char (point-min))))))

;; Put the menu bar back
(menu-bar-mode 1)

;; yasnippet
(require 'yasnippet)
(yas/initialize)
(yas/load-directory "~/elisp/vendor/snippets")

;; Tweak to ergo keybindings for commenting regions of text
(global-set-key (kbd "M-'") 'comment-or-uncomment-region)

;;
;; Run the VC command before running xelatex
(fset 'run-vc-then-xelatex
   [?\M-! ?v ?c return ?\C-c ?\C-c return])
(global-set-key (kbd "C-c c") 'run-vc-then-xelatex)

;; Load CEDET.
;; See cedet/common/cedet.info for configuration details.
;; Local
(load-file "~/elisp/vendor/cedet/common/cedet.el")
;; Included in current Emacs CVS but it doesn't work with ECB yet. So just use the released Emacs for now.
;;(load-file "/Applications/Emacs64.app/Contents/Resources/lisp/cedet/cedet.elc")


(global-ede-mode 1)    ; Enable the Project management system
(global-srecode-minor-mode 1)            ; Enable template insertion menu

;; * This enables some tools useful for coding, such as summary mode
;;   imenu support, and the semantic navigator
(semantic-load-enable-code-helpers)

;; * This enables even more coding tools such as intellisense mode
;;   decoration mode, and stickyfunc mode (plus regular code helpers)
;;(semantic-load-enable-gaudy-code-helpers)

;; * This enables the use of Exuberent ctags if you have it installed.
;;   If you use C++ templates or boost, you should NOT enable it.
;; (semantic-load-enable-all-exuberent-ctags-support)

;; Enable SRecode (Template management) minor-mode.
;; (global-srecode-minor-mode 1)  

;; Load ECB
(add-to-list 'load-path 
	"~/elisp/vendor/ecb")

(require 'ecb)
(require 'ecb-autoloads)

;; r-tags support.
;; http://dsarkar.fhcrc.org/rtags/rtags.html
(visit-tags-table "~/rtags/TAGS")


;; Base dir
(cd "~/")

;; custom variables kludge. Why can't I get these to work via setq?
(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
  '(LaTeX-XeTeX-command "xelatex -synctex=1")
  '(TeX-engine (quote xetex))
  ;; '(cdlatex-paired-parens "$[{(")
 )


;; from https://github.com/nilcons/emacs-use-package-fast/tree/a9cc00c5713a2a85d65399731abc4349b46756b4#a-trick-less-gc-during-startup
(setq gc-cons-threshold 64000000)
(add-hook 'after-init-hook
          (lambda ()
            ;; restore after startup
            (setq gc-cons-threshold 800000)))

;; some saner defaults
(setq vc-make-backup-files t)                                 ; make backups file even when in version controlled dir
(setq backup-directory-alist `(("." . "~/.emacs.d/backups"))) ; which directory to put backups file
(setq inhibit-startup-screen t)                               ; inhibit useless and old-school startup screen
(set-language-environment "UTF-8")
(setq default-fill-column 80)                                 ; toggle wrapping text at the 80th character

(defalias 'yes-or-no-p 'y-or-n-p)

;; recentf
(recentf-mode 1)
(setq recentf-max-menu-items 50)

;; spaces not tabs
(setq-default indent-tabs-mode nil)
(setq tab-width 4)

(setq package-enable-at-startup nil) ; tells emacs not to load any packages before starting up
;; the following lines tell emacs where on the internet to look up
;; for new packages.
(setq package-archives '(("org"       . "http://orgmode.org/elpa/")
                         ("gnu"       . "http://elpa.gnu.org/packages/")
                         ("melpa"     . "http://melpa.org/packages/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")))
(package-initialize) ; guess what this one does ?

(defun package--save-selected-packages (&rest opt) nil)

;; use-package

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)

(setq use-package-always-ensure t)

;; bars
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

;; server - from https://stackoverflow.com/a/5571983/7345298
(load "server")
(unless (server-running-p) (server-start))

;; parentheses
(electric-pair-mode 1)

;; save undo history
(setq undo-tree-auto-save-history t
      undo-tree-history-directory-alist '(("." . "~/.emacs.d/undo")))

;; use #' - from http://endlessparentheses.com/get-in-the-habit-of-using-sharp-quote.html
(defun elisp-insert-sharp ()
  "Insert #' unless in a string or comment."
  (interactive)
  (call-interactively #'self-insert-command)
  (let ((ppss (syntax-ppss)))
    (unless (or (elt ppss 3)
                (elt ppss 4)
                (eq (char-after) ?'))
      (insert "'"))))
(define-key emacs-lisp-mode-map "#" #'elisp-insert-sharp)

;; https://emacs.stackexchange.com/a/3008
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; disambiguate C-i and TAB
(define-key input-decode-map [?\C-i] [C-i])

;; font
(set-frame-font "Consolas 10" nil t)

;; smooth scrolling
(setq scroll-preserve-screen-position t
      scroll-margin 3
      scroll-conservatively 101)

;; Sacha Chua's typing timer
(defun timer-go ()
  "Quick keyboard timer."
  (interactive)
  (insert "GO\n")
  (run-with-timer 3 nil (lambda () (insert "\n")))  ; for warmup
  (run-with-timer 15 nil (lambda () ; 12 seconds + the 3-second warmup
                           (let ((col (- (point) (line-beginning-position))))
                             (insert (format " | %d | \n" col))))))
                           
(global-set-key (kbd "<f7>") #'timer-go)

;; line numbers
(use-package linum-relative
  :init
  (global-linum-mode t)
  :config
  (linum-relative-mode)
  (setq linum-relative-current-symbol ""))


;; theme
(use-package doom-themes
  :defer t
  :config
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t))
(use-package solarized-theme
  :defer t)
(load-theme 'doom-one t)

;; hl-todo
(use-package hl-todo
  :config
  (global-hl-todo-mode))

;; fill-column-indicator
(use-package fill-column-indicator
  :defer t
  :config
  (setq fci-always-use-textual-rule t
        fci-rule-character 9474))

;; evil
(use-package evil
  :defer .1
  :init
  (setq-default evil-move-beyond-eol t
                evil-ex-substitute-global t
                evil-ex-visual-char-range t
                evil-want-C-u-scroll t
                evil-want-C-d-scroll t
                evil-want-C-i-jump t)
  :config
  (evil-set-initial-state 'helpful-mode 'emacs)
  (evil-set-initial-state 'flycheck-error-list-mode 'emacs)
  (evil-mode 1))
(use-package evil-escape
  :after evil
  :init (evil-escape-mode)
  :config
  (setq-default evil-escape-key-sequence "jw"
                evil-escape-delay 0.2))
(use-package evil-surround
  :after evil
  :config
  (global-evil-surround-mode 1))
(use-package evil-snipe
  :after evil
  :config
  (setq evil-snipe-scope 'buffer
        evil-snipe-smart-case t)
  (evil-snipe-mode 1)
  (evil-snipe-override-mode 1))
(use-package evil-nerd-commenter
  :after evil
  :config
  (define-key evil-normal-state-map "gc" #'evilnc-comment-operator))
(use-package evil-exchange
  :after evil
  :config
  (evil-exchange-install))
(use-package evil-unimpaired
  :after evil
  :load-path "~/.emacs.d/evil-unimpaired"
  :config
  (evil-unimpaired-mode t)
  (evil-unimpaired-define-pair "q" '(flycheck-previous-error . flycheck-next-error)))
(use-package evil-search-highlight-persist
  :after evil
  :config
  (global-evil-search-highlight-persist t))
(use-package evil-god-state
  :after evil
  :config
  (add-hook 'evil-god-state-entry-hook (lambda () (setq cursor-type 'hollow)))
  (add-hook 'evil-god-state-exit-hook (lambda () (setq cursor-type 'box))))
(use-package evil-matchit
  :after evil
  :config
  (global-evil-matchit-mode 1))
(use-package evil-indent-plus
  :after evil
  :config
  (evil-indent-plus-default-bindings))

;; magit
(use-package magit
  :defer 3
  :config
  (evil-define-key 'normal with-editor-mode-map
    (kbd "KK") 'with-editor-finish
    (kbd "Kk") 'with-editor-finish
    (kbd "KC") 'with-editor-cancel
    (kbd "Kc") 'with-editor-cancel))
(use-package evil-magit :after magit)

;; company
(use-package company
  :commands global-company-mode
  :init
  (add-hook 'after-init-hook #'global-company-mode)

  :config
  ;; from spacemacs
  (define-key company-active-map (kbd "C-j") #'company-select-next)
  (define-key company-active-map (kbd "C-k") #'company-select-previous)
  (define-key company-active-map (kbd "C-l") #'company-complete-selection))
(use-package helm-company
  :after company
  :commands helm-company
  :init
  (define-key company-mode-map   (kbd "C-:") #'helm-company)
  (define-key company-active-map (kbd "C-:") #'helm-company))

;; rainbow-delimiters & parinfer
(use-package rainbow-delimiters
  :config
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))
(use-package parinfer
  :ensure t
  :bind (("C-," . parinfer-toggle-mode))
  :init
  (setq parinfer-extensions
        '(defaults       ; should be included.
          pretty-parens  ; different paren styles for different modes.
          evil           ; If you use Evil.
          smart-tab      ; C-b & C-f jump positions and smart shift with tab & S-tab.
          smart-yank))   ; Yank behavior depend on mode.
  (add-hook 'emacs-lisp-mode-hook #'parinfer-mode)
  (add-hook 'lisp-mode-hook       #'parinfer-mode))

;; flycheck
(use-package flycheck
  :commands global-flycheck-mode
  :init
  (add-hook 'prog-mode-hook #'flycheck-mode))
(use-package flycheck-inline
  :after flycheck
  :config
  (flycheck-inline-mode)

  (defun flycheck-inline-display-errors-unless-error-list (errors)
    "Show messages of ERRORS unless the error list is visible.

Like `flycheck-inline-display-errors', but only if the error
list (see `flycheck-list-errors') is not visible in any window in
the current frame."
    (unless (flycheck-get-error-list-window 'current-frame)
      (flycheck-inline-display-errors errors)))

  (setq-default flycheck-display-errors-function #'flycheck-inline-display-errors-unless-error-list))

;; flyspell
(setq ispell-dictionary "british")
(setq-default ispell-program-name "c:/cygwin64/bin/aspell.exe")

;; helm
(use-package helm
  :defer .1
  :config
  (helm-mode 1)
  (global-set-key (kbd "M-x") #'helm-M-x)

  (add-hook 'helm-major-mode-hook
            (lambda ()
              (setq auto-composition-mode nil)))

  ;; always display helm at bottom - adapted from spacemacs
  (setq helm-display-function
        (lambda (buffer &optional resume)
          (let ((display-buffer-alist
                 (list '("*.*Helm.*Help.**")
                       '("*.*helm.**"
                         (display-buffer-in-side-window)
                         (inhabit-same-window . t)
                         (side . bottom)        ; the important line!!
                         (window-width . 0.6)
                         (window-height . 0.4)))))
            (helm-default-display-buffer buffer))))

  ;; from https://github.com/syl20bnr/spacemacs/blob/c7a103a772d808101d7635ec10f292ab9202d9ee/layers/%2Bspacemacs/spacemacs-completion/funcs.el#L78
  (define-key helm-map (kbd "C-j") #'helm-next-line)
  (define-key helm-map (kbd "C-k") #'helm-previous-line)
  (define-key helm-map (kbd "C-h") #'helm-next-source)
  (define-key helm-map (kbd "C-S-h") #'describe-key)
  (define-key helm-map (kbd "C-l") (kbd "RET"))
  (with-eval-after-load 'helm-files
    (dolist (keymap (list helm-find-files-map helm-read-file-map))
      (define-key keymap (kbd "C-l") #'helm-execute-persistent-action)
      (define-key keymap (kbd "C-h") #'helm-find-files-up-one-level)
      ;; rebind `describe-key' for convenience
      (define-key keymap (kbd "C-S-h") #'describe-key))))
(use-package helm-flx
  :config
  (setq helm-flx-for-helm-find-files nil)
  (helm-flx-mode))
(use-package helm-themes
  :defer t)

;; init-open-recentf
(use-package init-open-recentf
  :config
  (init-open-recentf)
  (define-key recentf-dialog-mode-map (kbd "f") 'helm-find-files)
  (define-key recentf-dialog-mode-map (kbd "j") 'next-line)
  (define-key recentf-dialog-mode-map (kbd "k") 'previous-line)
  (define-key recentf-dialog-mode-map (kbd "l") 'widget-button-press))

;; helpful
(use-package helpful
  :config
  (global-set-key (kbd "C-h f") #'helpful-callable)
  (global-set-key (kbd "C-h v") #'helpful-variable)
  (global-set-key (kbd "C-h k") #'helpful-key)
  (global-set-key (kbd "C-h c") #'helpful-command))

;; thanks http://emacsredux.com/blog/2013/05/18/instant-access-to-init-dot-el/
(defun find-user-init-file ()
  "Edit the `user-init-file', in another window."
  (interactive)
  (find-file user-init-file))

(use-package avy)

(use-package which-key
  :config
  (which-key-mode)
  (which-key-enable-god-mode-support))

;; thanks https://emacs.stackexchange.com/a/18064/20375
(defun switch-to-previous-buffer ()
  (interactive)
  (switch-to-buffer (other-buffer)))

;; from https://github.com/flycheck/flycheck/issues/710#issue-98533622
(defun flycheck-list-errors-toggle ()
  "Toggle the error list for the current buffer."
  (interactive)
  (let ((flycheck-errors-window (get-buffer-window flycheck-error-list-buffer)))
    (if (not (window-live-p flycheck-errors-window))
        (call-interactively 'flycheck-list-errors)
      (delete-window flycheck-errors-window))))

(defun avy-goto-char-forward-char (char &optional arg)
  "Run `avy-goto-char', then move forward one character."
  (interactive (list (read-char "char: " t)
                     current-prefix-arg))
  (avy-goto-char char arg)
  (forward-char))

(defun set-selective-display-current-column ()
  (interactive)
  (set-selective-display
   (if selective-display nil (1+ (current-column)))))

(use-package general)

(general-create-definer spc-leader-define-key
 :states '(normal visual insert emacs)
 :prefix "SPC"
 :non-normal-prefix "C-SPC")

(general-create-definer mode-leader-define-key
 :states '(normal visual insert emacs)
 :prefix "K"
 :non-normal-prefix "C-0")

(spc-leader-define-key
 "SPC" #'helm-M-x
 "TAB" #'switch-to-previous-buffer
 "RET" #'evil-execute-in-god-state
 "$"   #'set-selective-display-current-column
 "!"   #'async-shell-command

 "b"   '(:ignore t :which-key "buffers")
 "bb"  #'helm-buffers-list
 "bd"  #'kill-this-buffer
 
 "c"   #'evil-search-highlight-persist-remove-all

 "e"   '(:ignore t :which-key "errors")
 "el"  #'flycheck-list-errors-toggle
 "e["  #'flycheck-previous-error
 "e]"  #'flycheck-next-error
 
 "f"   '(:ignore t :which-key "files")
 "ff"  #'helm-find-files
 "fi"  #'find-user-init-file
 "fr"  #'helm-recentf
 "fs"  #'save-buffer

 "h"   #'help-command
 "i"   '(:ignore t :which-key "UI")
 "if"  #'fci-mode
 "iu"  #'undo-tree-visualize
 "it"  #'helm-themes

 "g"   #'magit-status

 "j"   '(:ignore t :which-key "jump")
 "ji"  #'helm-semantic-or-imenu
 "jj"  #'evil-avy-goto-char
 "jJ"  #'evil-avy-goto-char-2
 "jk"  #'avy-goto-char-forward-char

 "u"   #'universal-argument
 "-"   #'negative-argument

 "w"   '(:ignore t :which-key "window")
 "wF"  #'make-frame
 "wh"  #'evil-window-left
 "wj"  #'evil-window-down
 "wk"  #'evil-window-up
 "wl"  #'evil-window-right
 "wH"  #'evil-window-move-far-left
 "wL"  #'evil-window-move-far-right
 "wJ"  #'evil-window-move-very-bottom
 "wK"  #'evil-window-move-very-top
 "w-"  #'split-window-below
 "w/"  #'split-window-right)

;; keybindings for elisp
(mode-leader-define-key emacs-lisp-mode-map
 "e"  '(:ignore t :which-key "eval")
 "ee" 'eval-defun
 "eh" 'eval-last-sexp
 "er" 'eval-region
 "ex" 'eval-expression
 "p"  '(:ignore t :which-key "eval with pp")
 "pe" 'pp-eval-last-sexp
 "px" 'pp-eval-expression
 "pm" 'pp-macroexpand-last-sexp)

;; define-word
(use-package define-word
    :defer t
    :init
    (spc-leader-define-key
     "d" 'define-word-at-point))

;; winum

(use-package winum
  :config
  (setq winum-auto-assign-0-to-minibuffer nil)
  (set-face-attribute 'winum-face nil :weight 'bold :inverse-video t)
  (spc-leader-define-key
   "0" 'winum-select-window-0-or-10
   "1" 'winum-select-window-1
   "2" 'winum-select-window-2
   "3" 'winum-select-window-3
   "4" 'winum-select-window-4
   "5" 'winum-select-window-5
   "6" 'winum-select-window-6
   "7" 'winum-select-window-7
   "8" 'winum-select-window-8
   "9" 'winum-select-window-9)

  ;; adapted from spacemacs
  (push '(("\\(.*\\) 0" . "winum-select-window-0-or-10") . ("\\1 0..9" . "window 0..9"))
        which-key-replacement-alist)
  (push '((nil . "winum-select-window-[1-9]") . t) which-key-replacement-alist)

  (winum-mode))

;; scratch-el
(use-package scratch
  :commands scratch
  :init
  (spc-leader-define-key
   "s" #'scratch))

;; s
(use-package s)

(require 'cl)

;; haskell
(use-package intero
  :commands intero-mode
  :preface
  ;; from spacemacs
  (defun haskell-intero-display-repl (&optional prompt-options)
    (interactive "P")
    (let ((buffer (intero-repl-buffer prompt-options)))
      (unless (get-buffer-window buffer 'visible)
        (display-buffer buffer))))

  (defun haskell-intero-pop-to-repl (&optional prompt-options)
    (interactive "P")
    (pop-to-buffer (intero-repl-buffer prompt-options)))

  ;; fix indent
  (setq-default haskell-indentation-layout-offset     4
                haskell-indentation-left-offset       4
                haskell-indentation-starter-offset    4
                haskell-indentation-where-post-offset 4
                haskell-indentation-where-pre-offset  4)

  :init
  ;; from spacemacs haskell layer docs (slightly modified)
  (defun haskell-indentation-advice ()
    (when (and (< 1 (line-number-at-pos))
               (save-excursion
                 (forward-line -1)
                 (let ((prev-line (s-trim (buffer-substring (line-beginning-position) (line-end-position)))))
                   (or (string= "" prev-line)
                       (string-prefix-p "import" prev-line)))))
      (delete-region (line-beginning-position) (point))))
  (advice-add #'haskell-indentation-newline-and-indent :after 'haskell-indentation-advice)
  (advice-add #'haskell-indentation-indent-line        :after 'haskell-indentation-advice)
  (add-hook 'haskell-mode-hook #'intero-mode)

  :config
  (defun haskell-hoogle-lookup-from-local-wrapper ()
    (interactive)
    (unless (haskell-hoogle-server-live-p) (haskell-hoogle-start-server))
    (haskell-hoogle-lookup-from-local))
  (mode-leader-define-key haskell-mode-map
   "d"  #'intero-goto-definition
   "ir" #'intero-restart
   "is" #'intero-apply-suggestions
   "it" #'intero-targets
   "r"  '(:ignore t :which-key "repl")
   "rb" #'intero-repl-load
   "rs" #'haskell-intero-display-repl
   "rS" #'haskell-intero-pop-to-repl
   "hh" #'hoogle
   "hH" #'haskell-hoogle-lookup-from-local-wrapper))

(use-package company-cabal
  :config
  (add-to-list 'company-backends 'company-cabal))
(use-package hasky-extensions
  :commands (hasky-extensions hasky-extensions-browse-docs)
  :init
  (mode-leader-define-key haskell-mode-map
   "xx" #'hasky-extensions
   "xd" #'hasky-extensions-browse-docs))
(use-package hasky-stack
  :commands hasky-stack-execute
  :init
  (mode-leader-define-key haskell-mode-map
   "s" #'hasky-stack-execute)
  :config
  (defun hasky-stack-run (cmd)
    "Execute \"stack run\" command running CMD."
    (interactive
     (list (read-string "Command to run: ")))
    (cl-destructuring-bind (app . args)
        (progn
          (string-match
           "^[[:blank:]]*\\(?1:[^[:blank:]]+\\)[[:blank:]]*\\(?2:.*\\)$"
           cmd)
          (cons (match-string 1 cmd)
                (match-string 2 cmd)))
      (hasky-stack--exec-command
       hasky-stack--project-name
       hasky-stack--last-directory
       (if (string= args "")
           (concat "run " app)
         (concat "run " app " -- " args)))))
  (magit-define-popup-action 'hasky-stack-root-popup
    ?r "Run" 'hasky-stack-run))

;; LaTeX - partly lifted from spacemacs
(use-package tex
  :ensure auctex
  :mode ("\\.tex\\'" . TeX-latex-mode)

  :preface
  ;; from spacemacs
  (defun LaTeX-build ()
    (interactive)
    (progn
      (let ((TeX-save-query nil))
        (TeX-save-document (TeX-master-file)))
      (TeX-command "LaTeX" 'TeX-master-file -1)))

  :init
  (setq TeX-command-default "LaTeX"
        TeX-auto-save t
        TeX-parse-self t
        TeX-syntactic-comment t
        ;; Synctex support
        TeX-source-correlate-start-server nil
        ;; Don't insert line-break at inline math
        LaTeX-fill-break-at-separators nil
        preview-gs-command "c:/Program Files (x86)/gs/gs9.22/bin/gswin32c.exe"
        preview-auto-reveal t)
  (add-hook 'LaTeX-mode-hook 'TeX-source-correlate-mode)
  (add-hook 'LaTeX-mode-hook 'TeX-PDF-mode)

  :config
  (mode-leader-define-key LaTeX-mode-map
   "a"   #'TeX-command-run-all
   "b"   #'LaTeX-build
   "v"   #'TeX-view
   "h"   #'TeX-doc
   "e"   #'LaTeX-environment
   "c"   #'LaTeX-close-environment
   "i"   #'LaTeX-insert-item
   "s"   #'LaTeX-section)

  ;; Rebindings for TeX-font - lifted from spacemacs
  (defun latex/font-bold         () (interactive) (TeX-font nil ?\C-b))
  (defun latex/font-code         () (interactive) (TeX-font nil ?\C-t))
  (defun latex/font-emphasis     () (interactive) (TeX-font nil ?\C-e))
  (defun latex/font-italic       () (interactive) (TeX-font nil ?\C-i))
  (defun latex/font-small-caps   () (interactive) (TeX-font nil ?\C-c))
  (defun latex/font-sans-serif   () (interactive) (TeX-font nil ?\C-f))

  (evil-define-key '(insert visual) TeX-mode-map (kbd "C-b") #'latex/font-bold)
  (evil-define-key '(insert visual) TeX-mode-map (kbd "C-t") #'latex/font-code)
  (evil-define-key '(insert visual) TeX-mode-map (kbd "C-e") #'latex/font-emphasis)
  (evil-define-key '(insert visual) TeX-mode-map (kbd "<C-i>") #'latex/font-italic)
  (evil-define-key '(insert visual) TeX-mode-map (kbd "C-s") #'latex/font-small-caps)
  (evil-define-key '(insert visual) TeX-mode-map (kbd "C-f") #'latex/font-sans-serif)

  ;; from https://stackoverflow.com/a/3466855/7345298
  (defun LaTeX-maybe-math ()
    "If in math mode, act as a prefix key for `LaTeX-math-keymap'.
  Otherwise act as `self-insert-command'."
    (interactive)
    (if (texmathp)
        (let* ((events (let ((overriding-local-map LaTeX-math-keymap))
                         (read-key-sequence "math: ")))
               (binding (lookup-key LaTeX-math-keymap events)))
          (call-interactively binding))
      (call-interactively 'self-insert-command)))
  (define-key TeX-mode-map "`" 'LaTeX-maybe-math)

  (setq font-latex-fontify-sectioning 'color
        font-latex-fontify-script     nil)
  (add-hook 'LaTeX-mode-hook
            (lambda ()
              (set (make-local-variable 'indent-line-function) 'indent-relative)))
  (add-hook 'LaTeX-mode-hook
            (lambda ()
              (flyspell-mode)
              (flyspell-buffer)))

  ;; adapted from https://lists.nongnu.org/archive/html/auctex/2009-11/msg00016.html
  (evil-define-key 'insert TeX-mode-map (kbd "C-\\") 'TeX-electric-macro)

  ;; from https://tex.stackexchange.com/questions/286028/inverse-search-with-emacs-auctex-and-sumatrapdf-on-windows-10
  (setq TeX-PDF-mode t)
  (setq TeX-source-correlate-mode t)
  (setq TeX-source-correlate-method 'synctex)
  (setq TeX-view-program-list
        '(("Sumatra PDF" ("\"C:/Program Files/SumatraPDF/SumatraPDF.exe\" -reuse-instance" (mode-io-correlate " -forward-search \"%b\" %n ") " %o"))))
  ;; (add-to-list 'TeX-expand-list '("%(cntxcom)" (lambda () "c:/context/bin/context1.bat")))

  (assq-delete-all 'output-pdf TeX-view-program-selection)
  (add-to-list 'TeX-view-program-selection '(output-pdf "Sumatra PDF")))

;; writeroom
(use-package writeroom-mode
  :commands writeroom-mode
  :init
  (spc-leader-define-key
    "iw" #'writeroom-mode))

;; org
(use-package org
  :config
  (add-hook 'org-mode-hook
            (lambda ()
              (flyspell-mode)
              (flyspell-buffer)))
  (mode-leader-define-key org-mode-map
    "i" #'org-insert-item))

;; lisp - SLY
(use-package sly
  :init
  (setq inferior-lisp-program "ros -Q run")
  :config
  (mode-leader-define-key lisp-mode-map
    "'"  #'sly

    "c"  '(:ignore t :which-key "compile")
    "cl" #'sly-compile-and-load-file

    "e"  '(:ignore t :which-key "eval")
    "ee" #'sly-eval-defun
    "eh" #'sly-eval-last-expression

    "h"  '(:ignore t :which-key "help")
    "hd" #'sly-describe-symbol
    "hs" #'hyperspec-lookup))

(message "Time taken: %s" (emacs-init-time))

;; custom
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["#1B2229" "#ff6c6b" "#98be65" "#ECBE7B" "#51afef" "#c678dd" "#46D9FF" "#DFDFDF"])
 '(custom-safe-themes
   (quote
    ("b54826e5d9978d59f9e0a169bbd4739dd927eead3ef65f56786621b53c031a7c" "ecba61c2239fbef776a72b65295b88e5534e458dfe3e6d7d9f9cb353448a569e" "6b2636879127bf6124ce541b1b2824800afc49c6ccd65439d6eb987dbf200c36" default)))
 '(fci-rule-color "#5B6268")
 '(jdee-db-active-breakpoint-face-colors (cons "#1B2229" "#51afef"))
 '(jdee-db-requested-breakpoint-face-colors (cons "#1B2229" "#98be65"))
 '(jdee-db-spec-breakpoint-face-colors (cons "#1B2229" "#3f444a"))
 '(safe-local-variable-values (quote ((TeX-command-extra-options . "-shell-escape"))))
 '(vc-annotate-background "#282c34")
 '(vc-annotate-color-map
   (list
    (cons 20 "#98be65")
    (cons 40 "#b4be6c")
    (cons 60 "#d0be73")
    (cons 80 "#ECBE7B")
    (cons 100 "#e6ab6a")
    (cons 120 "#e09859")
    (cons 140 "#da8548")
    (cons 160 "#d38079")
    (cons 180 "#cc7cab")
    (cons 200 "#c678dd")
    (cons 220 "#d974b7")
    (cons 240 "#ec7091")
    (cons 260 "#ff6c6b")
    (cons 280 "#cf6162")
    (cons 300 "#9f585a")
    (cons 320 "#6f4e52")
    (cons 340 "#5B6268")
    (cons 360 "#5B6268")))
 '(vc-annotate-very-old-color nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(fixed-pitch ((t nil))))

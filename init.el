;; from https://blog.d46.us/advanced-emacs-startup/
;; Use a hook so the message doesn't get clobbered by other messages.
(add-hook 'emacs-startup-hook
          (lambda ()
            (message "Emacs ready in %s with %d garbage collections."
                     (format "%.2f seconds"
                             (float-time
                              (time-subtract after-init-time before-init-time)))
                     gcs-done)))

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
(setq recentf-auto-cleanup 'never)
(recentf-mode 1)
(setq recentf-max-menu-items 50)
(run-with-idle-timer 2 nil #'recentf-cleanup)

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
(defun start-server-if-not-running ()
  "Start the Emacs server if it is not already running."
  (interactive)
  (load "server")
  (unless (server-running-p) (server-start)))

;; (run-with-idle-timer 1 nil #'start-server-if-not-running)

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
(if (eq system-type 'gnu/linux)
    (set-frame-font "Ubuntu Mono 12" nil t)
  (set-frame-font "Consolas 10" nil t))

;; smooth scrolling
(setq scroll-preserve-screen-position t
      scroll-margin 3
      scroll-conservatively 101)

;; line numbers
(use-package linum-relative
  :defer t
  :init
  (add-hook 'emacs-startup-hook
            (lambda () (linum-relative-mode)))
  :config
  (global-linum-mode t)
  (column-number-mode t)  ; this is as good a place as any to put this
  (setq linum-relative-current-symbol ""))


;; theme
(use-package doom-themes
  :defer t
  :config
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t)
  (doom-themes-org-config))
(use-package solarized-theme
  :defer
  :init
  (setq solarized-use-variable-pitch nil
        solarized-scale-org-headlines nil))
(use-package gruvbox-theme
  :defer)
;; (load-theme 'doom-one t)

;; hl-todo
(use-package hl-todo
  :defer .1
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
                evil-want-C-i-jump t
                evil-want-minibuffer t)
  :config
  (evil-set-initial-state 'dashboard-mode 'emacs)
  (evil-set-initial-state 'sly-db-mode 'emacs)
  (evil-set-initial-state 'flycheck-error-list-mode 'emacs)
  (evil-select-search-module 'evil-search-module 'evil-search)
  (evil-mode 1)

  ;; don't need digraphs, but DO need to input C-k!
  (define-key evil-insert-state-map (kbd "C-k") nil)

  (evil-define-operator evil-delete-trailing-whitespace (beg end)
    :type line
    :move-point nil
    "Delete trailing whitespace in the region."
    (delete-trailing-whitespace beg end))
  (evil-define-key '(normal visual) 'global "g$" 'evil-delete-trailing-whitespace))
(use-package evil-escape
  :after evil
  :init
  (setq-default evil-escape-key-sequence "jw"
                evil-escape-delay 0.2)
  :config
  (evil-escape-mode))
(use-package evil-surround
  :after evil
  :config
  (global-evil-surround-mode 1))
(use-package evil-snipe
  :after evil
  :config
  (setq evil-snipe-scope 'buffer
        evil-snipe-smart-case t)
  (evil-snipe-mode 1))
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
;; (use-package evil-search-highlight-persist
;;   :after evil
;;   :config
;;   (global-evil-search-highlight-persist t))
(use-package evil-god-state
  :after evil
  :config
  (evil-global-set-key 'normal "\\" 'evil-execute-in-god-state)
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
(use-package evil-lion
  :after evil
  :config
  (evil-lion-mode))
(use-package evil-numbers
  :after evil)

;; magit
(use-package magit
  :defer
  :config
  (evil-define-key 'normal with-editor-mode-map
    (kbd "KK") 'with-editor-finish
    (kbd "Kk") 'with-editor-finish
    (kbd "KC") 'with-editor-cancel
    (kbd "Kc") 'with-editor-cancel)
  (add-hook 'git-commit-setup-hook 'evil-insert-state))
(use-package evil-magit :after magit)

;; yasnippet
(use-package yasnippet
  :after evil
  :config
  (yas-global-mode))
(use-package yasnippet-snippets :after yasnippet)
(use-package helm-c-yasnippet
  :after (yasnippet helm)
  :init
  (eval-after-load 'general
    (spc-leader-define-key
        "x" 'helm-yas-complete)))

;; company
(use-package company
  :commands global-company-mode
  :init
  (setq company-idle-delay 0.1
        company-dabbrev-downcase nil)
  (add-hook 'after-init-hook #'global-company-mode)
  (add-hook 'LaTeX-mode-hook (lambda () (company-mode -1)))
  :config
  ;; from spacemacs
  (define-key company-active-map (kbd "C-j") #'company-select-next)
  (define-key company-active-map (kbd "C-k") #'company-select-previous)
  (define-key company-active-map (kbd "C-l") #'company-complete-selection)
  (spc-leader-define-key "im" #'company-mode))
(use-package helm-company
  :after (company helm)
  :commands helm-company
  :init
  (define-key company-mode-map   (kbd "C-:") #'helm-company)
  (define-key company-active-map (kbd "C-:") #'helm-company))

;; origami
(use-package origami
  :defer
  :init
  (add-hook 'prog-mode-hook #'origami-mode)
  (add-hook 'LaTeX-mode-hook #'origami-mode))

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
  (setq-default flycheck-display-errors-function #'flycheck-display-error-messages-unless-error-list)
  (add-hook 'prog-mode-hook #'flycheck-mode))
;; (use-package flycheck-inline
;;   :after flycheck
;;   :config
;;   (flycheck-inline-mode)

;;   (defun flycheck-inline-display-errors-unless-error-list (errors)
;;     "Show messages of ERRORS unless the error list is visible.

;; Like `flycheck-inline-display-errors', but only if the error
;; list (see `flycheck-list-errors') is not visible in any window in
;; the current frame."
;;     (unless (flycheck-get-error-list-window 'current-frame)
;;       (flycheck-inline-display-errors errors)))

;;   (setq-default flycheck-display-errors-function #'flycheck-inline-display-errors-unless-error-list))

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

  (setq helm-split-window-inside-p t)

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
  :commands (helm-themes helm-themes--load-theme))

;; helpful
(use-package helpful
  :commands (helpful-callable helpful-variable helpful-key helpful-command)
  :init
  (global-set-key (kbd "C-h f") #'helpful-callable)
  (global-set-key (kbd "C-h v") #'helpful-variable)
  (global-set-key (kbd "C-h k") #'helpful-key)
  (global-set-key (kbd "C-h c") #'helpful-command)
  :config
  (evil-define-key 'normal helpful-mode-map "q" 'quit-window))

;; thanks http://emacsredux.com/blog/2013/05/18/instant-access-to-init-dot-el/
(defun find-user-init-file ()
  "Edit the `user-init-file', in another window."
  (interactive)
  (find-file user-init-file))

(use-package which-key
  :config
  (setq which-key-allow-evil-operators t)
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

(defun set-selective-display-current-column ()
  (interactive)
  (set-selective-display
   (if selective-display nil (1+ (current-column)))))

(use-package general
  :config
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
   "RET" #'evil-execute-in-emacs-state
   "$"   #'set-selective-display-current-column
   "!"   #'shell-command
   "&"   #'async-shell-command

   "b"   '(:ignore t :which-key "buffers")
   "bb"  #'helm-buffers-list
   "bd"  #'kill-this-buffer
   "bi"  #'ibuffer
   "bs"  #'start-server-if-not-running
   "bci" #'clone-indirect-buffer
   "bco" #'clone-indirect-buffer-other-window

   "c"   #'evil-ex-nohighlight

   "e"   #'flycheck-list-errors-toggle

   "f"   '(:ignore t :which-key "files")
   "ff"  #'helm-find-files
   "fi"  #'find-user-init-file
   "fr"  #'helm-recentf
   "fs"  #'save-buffer
   "ft"  #'helm-etags-select
   "fx"  #'delete-file

   "h"   #'help-command

   "i"   '(:ignore t :which-key "UI")
   "if"  #'fci-mode
   "iu"  #'undo-tree-visualize
   "it"  '(:ignore t :which-key "themes")
   "itdo" (lambda () (interactive) (helm-themes--load-theme "doom-one"))
   "itle" (lambda () (interactive) (helm-themes--load-theme "leuven"))
   "itsd" (lambda () (interactive) (helm-themes--load-theme "solarized-dark"))
   "itsl" (lambda () (interactive) (helm-themes--load-theme "solarized-light"))
   "itgs" (lambda () (interactive) (helm-themes--load-theme "gruvbox-light-soft"))
   "itt" #'helm-themes
   "iw"  #'whitespace-mode
   "ic"  '(:ignore t :which-key "customise")
   "icg" #'customize-group
   "icf" #'customize-face
   "icv" #'customize-variable
   "il"  '(:ignore t :which-key "calc")
   "ilq" #'quick-calc
   "ilc" #'calc
   "is"  '(:ignore t :which-key "flyspell")
   "iss" #'flyspell-mode
   "isx" #'flyspell-buffer

   "g"   #'magit-status

   "j"   '(:ignore t :which-key "jump")
   "ji"  #'helm-semantic-or-imenu
   "jw"  #'subword-mode

   "k"   #'kill-compilation

   "n"   '(:ignore t :which-key "numbers")
   "n+"  #'evil-numbers/inc-at-pt
   "n-"  #'evil-numbers/dec-at-pt

   "r"   #'revert-buffer
   "u"   #'universal-argument

   "-"   #'negative-argument

   "w"   '(:ignore t :which-key "window")
   "wf"  #'make-frame
   "wh"  #'evil-window-left
   "wj"  #'evil-window-down
   "wk"  #'evil-window-up
   "wo"  #'delete-other-windows
   "wx"  #'ace-delete-window
   "wl"  #'evil-window-right
   "wq"  #'delete-window
   "wH"  #'evil-window-move-far-left
   "wL"  #'evil-window-move-far-right
   "wJ"  #'evil-window-move-very-bottom
   "wK"  #'evil-window-move-very-top
   "wQ"  #'kill-buffer-and-window
   "w-"  #'split-window-below
   "w/"  #'split-window-right))

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

;; avy
(defun avy-goto-char-forward-char (char &optional arg)
  "Run `avy-goto-char', then move forward one character.
CHAR and ARG are as in avy."
  (interactive (list (read-char "char: " t)
                     current-prefix-arg))
  (avy-goto-char char arg)
  (forward-char))
(use-package avy
  :defer t
  :commands avy-goto-char-forward-char
  :init
  (spc-leader-define-key
   "jj"  #'evil-avy-goto-char
   "jJ"  #'evil-avy-goto-char-2
   "jk"  #'avy-goto-char-forward-char
   "jl"  #'avy-goto-line))

;; define-word
(use-package define-word
    :defer t
    :init
    (spc-leader-define-key
     "d" 'define-word-at-point))

;; winum

(use-package winum
  :defer t
  :init
  (setq winum-auto-assign-0-to-minibuffer nil)
  (add-hook 'emacs-startup-hook
            (lambda () (winum-mode)))
  :config
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
  (push '((nil . "winum-select-window-[1-9]") . t) which-key-replacement-alist))

;; scratch-el
(use-package scratch
  :commands scratch
  :init
  (spc-leader-define-key
   "s" #'scratch))

;; try
(use-package try
  :commands try)

;; projectile
(use-package projectile
  :defer
  :init
  (defun autoload-projectile ()
    (interactive)
    (autoload-do-load #'projectile-switch-project))
  (spc-leader-define-key
      "pf" 'projectile-find-file
      "pp" 'projectile-switch-project
      "pl" #'autoload-projectile)
  :config
  (projectile-mode +1)
  (setq projectile-completion-system 'helm)
        ;; projectile-globally-ignored-file-suffixes
        ;; (append '("~")
                ;; projectile-globally-ignored-file-suffixes))
  (projectile-register-project-type 'haskell-stack '("stack.yaml")
                                    :compile "stack build"
                                    :run "stack run"
                                    :test "stack build --test"
                                    :test-suffix "Spec")
  (spc-leader-define-key
      "p" 'projectile-command-map))

;; highlight-indent-guides
(use-package highlight-indent-guides
  :commands highlight-indent-guides-mode
  :init
  (setq highlight-indent-guides-method 'character
        highlight-indent-guides-auto-character-face-perc 10
        highlight-indent-guides-responsive 'top)
  (spc-leader-define-key
   "ig" #'highlight-indent-guides-mode))

;; idle-highlight-mode
(use-package idle-highlight-mode
  :defer
  :init
  (spc-leader-define-key
    "ih" #'idle-highlight-mode))

;; move-text
(use-package move-text
  :after evil-unimpaired
  :config
  (evil-unimpaired-define-pair "e" '(move-text-up . move-text-down)))

;; s
(use-package s)

(require 'cl)

;; haskell
(defun haskell-run-yesod-devel ()
  (interactive)
  (projectile-with-default-dir (projectile-ensure-project (projectile-project-root))
    (async-shell-command "stack exec -- yesod devel")))
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
  (defun haskell-run-glade (file)
    (interactive "fGlade file: ")
    (async-shell-command (concat "stack exec -- glade " file)))
  (mode-leader-define-key haskell-mode-map
   "d"  #'intero-goto-definition
   "c"  '(:ignore t :which-key "commands")
   "cg" #'haskell-run-glade
   "cy" #'haskell-run-yesod-devel
   "ig" #'haskell-mode-generate-tags
   "ii" #'intero-info
   "ir" #'intero-restart
   "is" #'intero-apply-suggestions
   "it" #'intero-targets
   "r"  '(:ignore t :which-key "repl")
   "rl" #'intero-repl-load
   "rs" #'haskell-intero-display-repl
   "rp" #'haskell-intero-pop-to-repl
   "t"  #'intero-type-at
   "hh" #'hoogle
   "hH" #'haskell-hoogle-lookup-from-local-wrapper))

(use-package company-cabal
  :after (company intero)
  :config
  (add-to-list 'company-backends 'company-cabal))
(use-package hasky-extensions
  :commands (hasky-extensions hasky-extensions-browse-docs)
  :init
  (mode-leader-define-key haskell-mode-map
   "xx" #'hasky-extensions
   "xd" #'hasky-extensions-browse-docs)
  :config
  (add-to-list 'hasky-extensions "OverloadedLabels" t))
(use-package hasky-stack
  :commands hasky-stack-execute
  :init
  (mode-leader-define-key haskell-mode-map
   "s" #'hasky-stack-execute))

(use-package shakespeare-mode
  :defer
  :config
  (add-hook 'shakespeare-hamlet-mode-hook
            (lambda ()
              (setq sgml-basic-offset 4)))
  (defun switch-between-hamlet-julius ()
    "Switch between corresponding XXX.hamlet and XXX.julius files."
    (interactive)
    (let ((sx (file-name-sans-extension buffer-file-name)))
      (pcase (file-name-extension buffer-file-name)
        ("hamlet" (find-file (concat sx "." "julius")))
        ("julius" (find-file (concat sx "." "hamlet"))))))
  (mode-leader-define-key shakespeare-mode-map
    "cy" #'haskell-run-yesod-devel
    "x"  #'switch-between-hamlet-julius))

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
   "l"   #'TeX-recenter-output-buffer
   "e"   #'LaTeX-environment
   "c"   #'LaTeX-close-environment
   "i"   #'LaTeX-insert-item
   "s"   #'LaTeX-section
   "q"   #'TeX-next-error
   "\\"  #'TeX-electric-macro

   "p"   '(:ignore t :which-key "preview")
   "pb"  #'preview-buffer
   "pp"  #'preview-at-point
   "pr"  #'preview-region
   "pc"  '(:ignore t :which-key "clearout")
   "pcb" #'preview-clearout-buffer
   "pcp" #'preview-clearout-at-point)

  ;; Rebindings for TeX-font - lifted from spacemacs
  (defun latex/font-bold         () (interactive) (TeX-font nil ?\C-b))
  (defun latex/font-code         () (interactive) (TeX-font nil ?\C-t))
  (defun latex/font-emphasis     () (interactive) (TeX-font nil ?\C-e))
  (defun latex/font-italic       () (interactive) (TeX-font nil ?\C-i))
  (defun latex/font-small-caps   () (interactive) (TeX-font nil ?\C-c))
  (defun latex/font-sans-serif   () (interactive) (TeX-font nil ?\C-f))

  (defvar latex-current-insert-command "textit" "The command inserted by C-w.")
  (defun latex/font-current (arg)
    (interactive "*P")
    (if arg
        (progn
          (call-interactively 'TeX-insert-macro)
          (setq latex-current-insert-command TeX-default-macro))
      (TeX-insert-macro latex-current-insert-command)))

  (evil-define-key '(insert visual) TeX-mode-map (kbd "C-b") #'latex/font-bold)
  (evil-define-key '(insert visual) TeX-mode-map (kbd "C-t") #'latex/font-code)
  (evil-define-key '(insert visual) TeX-mode-map (kbd "C-e") #'latex/font-emphasis)
  (evil-define-key '(insert visual) TeX-mode-map (kbd "<C-i>") #'latex/font-italic)
  (evil-define-key '(insert visual) TeX-mode-map (kbd "C-s") #'latex/font-small-caps)
  (evil-define-key '(insert visual) TeX-mode-map (kbd "C-f") #'latex/font-sans-serif)
  (evil-define-key '(insert visual) TeX-mode-map (kbd "C-w") #'latex/font-current)

  (custom-set-variables '(LaTeX-math-abbrev-prefix "M-n"))
  (add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)

  (setq font-latex-fontify-sectioning 'color
        font-latex-fontify-script     nil)
  (add-hook 'LaTeX-mode-hook
            (lambda ()
              (set (make-local-variable 'indent-line-function) 'indent-relative)))
  (add-hook 'LaTeX-mode-hook
            (lambda ()
              (flyspell-mode)
              (flyspell-buffer)))
  (add-hook 'LaTeX-mode-hook
            (lambda ()
              (start-server-if-not-running)))

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

;; olivetti
(use-package olivetti
  :after general
  :commands olivetti-mode
  :init
  (spc-leader-define-key
    "io"  '(:ignore t :which-key "olivetti")
    "ioo" #'olivetti-mode
    "io[" #'olivetti-shrink
    "io]" #'olivetti-expand
    "iow" #'olivetti-set-width))

;; org
(use-package org
  :defer
  :init
  (spc-leader-define-key
    "ia" '(:ignore t :which-key "org-agenda")
    "iaa" #'org-agenda
    "iac" #'org-capture
    "iak" #'org-cycle-agenda-files)
  :config

  (evil-define-key 'emacs 'org-agenda-mode-map "\\" #'evil-execute-in-god-state)
  (mode-leader-define-key org-mode-map
    "e" #'org-export-dispatch
    "i" #'org-insert-item
    "t" #'org-todo
    "w" #'org-refile
    "K" #'org-ctrl-c-ctrl-c
    "r" #'org-reveal
    "s" '(:ignore t :which-key "scheduling")
    "ss" #'org-schedule
    "sd" #'org-deadline)
  (mode-leader-define-key org-agenda-mode-map
    "c" #'org-agenda-columns
    "o" #'org-agenda-open-link
    "t" #'org-agenda-todo
    "s" '(:ignore t :which-key "scheduling")
    "ss" #'org-agenda-schedule
    "sd" #'org-agenda-deadline)

  (setq org-agenda-files '("~/Dropbox/org")
        org-todo-keywords
        '((sequence "TODO(t)" "NEXT(n)" "INPROGRESS(p)" "|" "DONE(d)")
          (sequence "WAITING(w@)" "HOLD(h@)"))
        org-agenda-custom-commands
        '((" " "Block agenda"
           ;; ((agenda "" ((org-agenda-span 'day)))
           ((tags "REFILE"
                  ((org-agenda-overriding-header "To refile")))
            (tags "-university/INPROGRESS"
                  ((org-agenda-overriding-header "In progress")))
            (tags "-university/NEXT"
                  ((org-agenda-overriding-header "Next tasks")))
            (tags "-university/WAITING"
                  ((org-agenda-overriding-header "Waiting tasks")))
            (tags "-university/TODO"
                  ((org-agenda-overriding-header "Other TODOs")))
            (tags "-university/HOLD"
                  ((org-agenda-overriding-header "Hold tasks")))))
            ;; (agenda "" nil)))
          ("u" "University"
           ;; ((tags-todo "university&SCHEDULED<\"<today>\"|university&DEADLINE<\"<today>\""
           ;;             ((org-agenda-overriding-header "Overdue")))
           ((tags-todo "+ALLTAGS={university}+DEADLINE=\"\"+SCHEDULED=\"\"/!-INPROGRESS"
                       ((org-agenda-overriding-header "No due date")))
            (tags-todo "+university/INPROGRESS"
                       ((org-agenda-overriding-header "In progress")))
            (agenda ""
                    ((org-agenda-span 14)
                     (org-agenda-entry-types '(:deadline :scheduled))
                     ;; (org-agenda-time-grid nil)
                     (org-deadline-warning-days 0))))))
        org-agenda-show-outline-path t
        org-refile-targets
        '((nil :maxlevel . 9)
          (org-agenda-files :maxlevel . 9))
        org-refile-use-outline-path 'file
        org-refile-allow-creating-parent-nodes 'confirm
        org-capture-templates
        '(("t" "todo" entry (file "~/Dropbox/org/refile.org")
           "* TODO %^{Description}"))
        org-archive-mark-done nil
        org-archive-location "%s_archive::* Archived Tasks"
        org-enforce-todo-dependencies t
        org-track-ordered-property-with-tag nil
        ;; org-agenda-dim-blocked-tasks t
        org-enforce-todo-checkbox-dependencies t
        org-log-into-drawer t)

  ;; from https://lists.gnu.org/archive/html/emacs-orgmode/2015-06/msg00266.html
  (defun org-agenda-delete-empty-blocks ()
    "Remove empty agenda blocks.
  A block is identified as empty if there are fewer than 2
  non-empty lines in the block (excluding the line with
  `org-agenda-block-separator' characters)."
    (when org-agenda-compact-blocks
      (user-error "Cannot delete empty compact blocks"))
    (setq buffer-read-only nil)
    (save-excursion
      (goto-char (point-min))
      (let* ((blank-line-re "^\\s-*$")
             (content-line-count (if (looking-at-p blank-line-re) 0 1))
             (start-pos (point))
             (block-re (format "%c\\{10,\\}" org-agenda-block-separator)))
        (while (and (not (eobp)) (forward-line))
          (cond
           ((looking-at-p block-re)
            (when (< content-line-count 2)
              (delete-region start-pos (1+ (point-at-bol))))
            (setq start-pos (point))
            (forward-line)
            (setq content-line-count (if (looking-at-p blank-line-re) 0 1)))
           ((not (looking-at-p blank-line-re))
            (setq content-line-count (1+ content-line-count)))))
        (when (< content-line-count 2)
          (delete-region start-pos (point-max)))
        (goto-char (point-min))
        ;; The above strategy can leave a separator line at the beginning
        ;; of the buffer.
        (when (looking-at-p block-re)
          (delete-region (point) (1+ (point-at-eol))))))
    (setq buffer-read-only t))
  (add-hook 'org-agenda-finalize-hook #'org-agenda-delete-empty-blocks))
(use-package evil-org
  :ensure t
  :after (org evil)
  :config
  (add-hook 'org-mode-hook 'evil-org-mode)
  (add-hook 'evil-org-mode-hook
            (lambda ()
              (evil-org-set-key-theme)))
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys)
  (evil-define-key 'motion org-agenda-mode-map
    "X" #'org-agenda-columns))

;; lisp - SLY
(use-package sly
  :defer
  :init
  (setq inferior-lisp-program "sbcl")
  (mode-leader-define-key lisp-mode-map
    "'"  #'sly

    "c"  '(:ignore t :which-key "compile")
    "cl" #'sly-compile-and-load-file
    "cc" #'sly-compile-defun

    "e"  '(:ignore t :which-key "eval")
    "ee" #'sly-eval-defun
    "eh" #'sly-eval-last-expression

    "h"  '(:ignore t :which-key "help")
    "hd" #'sly-describe-symbol
    "hs" #'hyperspec-lookup))

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
 '(fixed-pitch ((t nil)))
 '(font-latex-script-char-face ((t nil)))
 '(linum ((t (:height 100))))
 '(preview-reference-face ((t (:height 1.5)))))

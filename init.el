;; from https://blog.d46.us/advanced-emacs-startup/
;; Use a hook so the message doesn't get clobbered by other messages.
(add-hook 'emacs-startup-hook
          (lambda ()
            (message "Emacs ready in %s with %d garbage collections."
                     (format "%.2f seconds"
                             (float-time
                              (time-subtract after-init-time before-init-time)))
                     gcs-done)))

;; some saner defaults
(setq gc-cons-threshold 100000000)
(setq vc-make-backup-files t)                                 ; make backups file even when in version controlled dir
(setq backup-directory-alist `(("." . "~/.emacs.d/backups"))) ; which directory to put backups file
(setq inhibit-startup-screen t)                               ; inhibit useless and old-school startup screen
(set-language-environment "UTF-8")
(setq default-fill-column 80)                                 ; toggle wrapping text at the 80th character
(setq-default inhibit-compacting-font-caches t)               ; dont compact large fonts
(setq backup-by-copying t)
(setq read-process-output-max (* 1024 1024))                  ; increase amount of data read from process - recommended by lsp docs

(defalias 'yes-or-no-p 'y-or-n-p)

;; recentf
(setq recentf-auto-cleanup 'never)
(recentf-mode 1)
(setq recentf-max-menu-items 50)

;; spaces not tabs
(setq-default indent-tabs-mode nil)
(setq tab-width 4)

;; scratch buffer is fundamental
(setq initial-major-mode 'fundamental-mode)

(setq package-enable-at-startup nil) ; tells emacs not to load any packages before starting up
;; the following lines tell emacs where on the internet to look up
;; for new packages.
(setq package-archives '(("org"       . "http://orgmode.org/elpa/")
                         ("gnu"       . "http://elpa.gnu.org/packages/")
                         ("melpa"     . "http://melpa.org/packages/")))
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
(global-undo-tree-mode 1)

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
(add-to-list 'default-frame-alist
             (if (eq system-type 'gnu/linux)
                 '(font . "Ubuntu Mono 12")
               '(font . "Consolas 10")))

(use-package unicode-fonts
  :preface

  ;; see https://github.com/rolandwalker/unicode-fonts/issues/3#issuecomment-93392177
  (defun my-hook-unicode-fonts-setup (frame)
  "Run unicode-fonts-setup, then remove the hook."
  (progn
      (select-frame frame)
      (unicode-fonts-setup)
      (message "Removing unicode-fonts-setup to after-make-frame-functions hook")
      (remove-hook 'after-make-frame-functions 'my-hook-unicode-fonts-setup)))

  :init
  (add-hook 'after-make-frame-functions 'my-hook-unicode-fonts-setup nil))

;; smooth scrolling
(setq scroll-preserve-screen-position t
      scroll-margin 3
      scroll-conservatively 101)

;; open Hoogle URLs with Qutebrowser
(setq browse-url-qute-arguments nil)
(setq browse-url-qute-program '"c:/Program Files/qutebrowser/qutebrowser.exe")
(defun browse-url-qute (url &optional _new-window)
  "Ask the Qutebrowser WWW browser to load URL.
Default to the URL around or before point.  The strings in
variable `browse-url-qute-arguments' are also passed to
Qutebrowser.
The optional argument NEW-WINDOW is not used."
  (interactive (browse-url-interactive-arg "URL: "))
  (setq url (browse-url-encode-url url))
  (let* ((process-environment (browse-url-process-environment)))
    (apply 'start-process
	   (concat "qutebrowser " url) nil
	   browse-url-qute-program
	   (append
	    browse-url-qute-arguments
	    (list url)))))
(setq browse-url-browser-function
      '(("hoogle" . browse-url-qute)
        ("." . browse-url-default-browser)))

;; line numbers - see https://emacs.stackexchange.com/a/50136/20375
(global-linum-mode -1)
(setq display-line-numbers-type 'visual)
(global-display-line-numbers-mode)

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
  ;; :defer .1
  :init
  (setq-default evil-move-beyond-eol t
                evil-ex-substitute-global t
                evil-ex-visual-char-range t
                evil-want-C-u-scroll t
                evil-want-C-d-scroll t
                evil-want-C-i-jump t
                evil-want-minibuffer t
                evil-want-keybinding nil
                evil-want-integration t
                evil-undo-system 'undo-tree)
  :config
  (evil-set-initial-state 'dashboard-mode 'emacs)
  (evil-set-initial-state 'sly-db-mode 'emacs)
  (evil-set-initial-state 'flycheck-error-list-mode 'emacs)
  (evil-set-initial-state 'dired-mode 'emacs)
  (evil-select-search-module 'evil-search-module 'evil-search)
  (evil-mode 1)

  (define-key evil-normal-state-map "j" 'evil-next-visual-line)
  (define-key evil-normal-state-map "k" 'evil-previous-visual-line)
  (define-key evil-visual-state-map "j" 'evil-next-visual-line)
  (define-key evil-visual-state-map "k" 'evil-previous-visual-line)

  ;; don't need digraphs, but DO need to input C-k!
  (define-key evil-insert-state-map (kbd "C-k") nil)

  (evil-define-operator evil-delete-trailing-whitespace (beg end)
    :type line
    :move-point nil
    "Delete trailing whitespace in the region."
    (delete-trailing-whitespace beg end))
  (evil-define-key '(normal visual) 'global "g$" 'evil-delete-trailing-whitespace)

  ;; setup for nerd-commenter
  (define-key evil-normal-state-map "gc" #'evilnc-comment-operator))
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
  :commands evilnc-comment-operator)
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

(use-package evil-collection
  :after evil
  :config
  (with-eval-after-load 'magit (evil-collection-magit-setup)))

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

;; yasnippet
(use-package yasnippet
  :commands yas-minor-mode
  :init
  ;; don’t use yas-global-mode - it takes too much time at startup
  ;; instead only enable it in modes where I use it:
  (add-hook 'LaTeX-mode-hook #'yas-minor-mode)
  (add-hook 'haskell-mode-hook #'yas-minor-mode)
  (add-hook 'yas-minor-mode-hook #'yas-reload-all))
(use-package yasnippet-snippets :after yasnippet)
;; (use-package helm-c-yasnippet
;;   :after (yasnippet helm)
;;   :commands helm-yas-complete)
(use-package ivy-yasnippet
  :after (yasnippet ivy)
  :commands ivy-yasnippet)

;; company
(use-package company
  :commands global-company-mode
  :init
  (setq company-idle-delay 0.2
        company-dabbrev-downcase nil)
  (add-hook 'prog-mode-hook #'(lambda () (global-company-mode 1)))
  (add-hook 'comint-mode-hook #'(lambda () (global-company-mode 1)))
  (add-hook 'LaTeX-mode-hook (lambda () (company-mode -1)))
  :config
  ;; from spacemacs
  (define-key company-active-map (kbd "C-j") #'company-select-next)
  (define-key company-active-map (kbd "C-k") #'company-select-previous)
  (define-key company-active-map (kbd "C-l") #'company-complete-selection)
  (with-eval-after-load 'counsel
    (define-key company-mode-map   (kbd "C-:") #'counsel-company)
    (define-key company-active-map (kbd "C-:") #'counsel-company))
  (spc-leader-define-key "im" #'company-mode))
;; (use-package helm-company
;;   :after (company helm)
;;   :commands helm-company
;;   :init
;;   (define-key company-mode-map   (kbd "C-:") #'helm-company)
;;   (define-key company-active-map (kbd "C-:") #'helm-company))

;; origami
(use-package origami
  :defer
  :init
  (add-hook 'prog-mode-hook #'origami-mode)
  (add-hook 'LaTeX-mode-hook #'origami-mode))

;; rainbow-delimiters
(use-package rainbow-delimiters
  :init
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))
(use-package lispy
  :defer
  :init
  (add-hook 'emacs-lisp-mode-hook (lambda () (lispy-mode 1)))
  (add-hook 'lisp-mode-hook       (lambda () (lispy-mode 1)))
  (add-hook 'lispy-mode-hook #'lispyville-mode)
  :config
  (setq lispy-close-quotes-at-end-p t))
(use-package lispyville
  :after (lispy yasnippet)
  :commands lispyville-mode
  :init
  (with-eval-after-load 'evil
    (let ((pos (memq 'evil-mode-line-tag mode-line-format)))
      (setcdr pos (cons
                   '(:eval (when (featurep 'lispyville)
                             (lispyville-mode-line-string "l-special" "")))
                   (cdr pos)))))
  :config
  (lispyville-set-key-theme
   '(operators
     c-w
     text-objects
     ;; atom-movement
     additional-movement
     commentary
     slurp/barf-cp
     wrap
     additional
     additional-insert
     escape)))

;; (use-package parinfer
;;   :ensure t
;;   :bind (("C-," . parinfer-toggle-mode))
;;   :init
;;   (setq parinfer-extensions
;;         '(defaults       ; should be included.
;;           pretty-parens  ; different paren styles for different modes.
;;           evil           ; If you use Evil.
;;           smart-tab      ; C-b & C-f jump positions and smart shift with tab & S-tab.
;;           smart-yank))   ; Yank behavior depend on mode.
;;   (add-hook 'emacs-lisp-mode-hook #'parinfer-mode)
;;   (add-hook 'lisp-mode-hook       #'parinfer-mode)
;;   (add-hook 'extempore-mode-hook  #'parinfer-mode))

;; flycheck
(use-package flycheck
  :commands global-flycheck-mode
  :init
  (setq-default flycheck-display-errors-function #'flycheck-display-error-messages-unless-error-list)
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (setq flycheck-navigation-minimum-level 'error)
  (add-hook 'prog-mode-hook #'flycheck-mode))
  ;; :config
  ;; (setq flycheck-error-list-format
  ;;       `[("File" 6)
  ;;         ("Line" 5 flycheck-error-list-entry-< :right-align t)
  ;;         ("Col" 3 nil :right-align t)
  ;;         ("Level" 8 flycheck-error-list-entry-level-<)
  ;;         ("ID" 6 t)
  ;;         (,(flycheck-error-list-make-last-column "Message" 'Checker) 0 t)])
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

;; ivy
(use-package ivy
  :config
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers nil)
  (setq ivy-count-format "(%d/%d) ")
  (setq ivy-wrap t)
  (setq ivy-height 20)
  (setq ivy-re-builders-alist '((t . ivy--regex-ignore-order)))
  (assq-delete-all 'org-refile ivy-initial-inputs-alist)
  (assq-delete-all 'org-agenda-refile ivy-initial-inputs-alist)
  (assq-delete-all 'org-capture-refile ivy-initial-inputs-alist)
  (add-to-list 'ivy-initial-inputs-alist '(TeX-electric-macro . "^"))

  (define-key ivy-minibuffer-map (kbd "C-j") #'ivy-next-line)
  (define-key ivy-minibuffer-map (kbd "C-k") #'ivy-previous-line)
  (define-key ivy-switch-buffer-map (kbd "C-k") #'ivy-previous-line)
  (define-key ivy-minibuffer-map (kbd "C-l") #'ivy-alt-done)
  (define-key ivy-minibuffer-map (kbd "<C-return>") #'ivy-immediate-done))

(use-package counsel
  :after ivy

  :config
  (global-set-key (kbd "M-x") #'counsel-M-x)
  (define-key counsel-find-file-map (kbd "C-h") #'counsel-up-directory)

  ;; from https://oremacs.com/2015/06/23/counsel-load-theme/
  (defun counsel--load-theme-action (x)
    "Disable current themes and load theme X."
    (condition-case nil
        (progn
          (mapc #'disable-theme custom-enabled-themes)
          (load-theme (intern x))
          (when (fboundp 'powerline-reset)
            (powerline-reset)))
      (error "Problem loading theme %s" x)))

  (defun counsel-load-theme ()
    "Forward to `load-theme'.
  Usable with `ivy-resume', `ivy-next-line-and-call' and
  `ivy-previous-line-and-call'."
    (interactive)
    (ivy-read "Load custom theme: "
              (mapcar 'symbol-name
                      (custom-available-themes))
              :action #'counsel--load-theme-action)))

;; ;; helm
;; (use-package helm
;;   ;; :defer .1
;;   :defer 2
;;   :config
;;   (helm-mode 1)
;;   (global-set-key (kbd "M-x") #'helm-M-x)

;;   (add-hook 'helm-major-mode-hook
;;             (lambda ()
;;               (setq auto-composition-mode nil)))

;;   ;; always display helm at bottom - adapted from spacemacs
;;   (setq helm-display-function
;;         (lambda (buffer &optional resume)
;;           (let ((display-buffer-alist
;;                  (list '("*.*Helm.*Help.**")
;;                        '("*.*helm.**"
;;                          (display-buffer-in-side-window)
;;                          (inhabit-same-window . t)
;;                          (side . bottom)        ; the important line!!
;;                          (window-width . 0.6)
;;                          (window-height . 0.4)))))
;;             (helm-default-display-buffer buffer))))

;;   (setq helm-split-window-inside-p t)

;;   ;; from https://github.com/syl20bnr/spacemacs/blob/c7a103a772d808101d7635ec10f292ab9202d9ee/layers/%2Bspacemacs/spacemacs-completion/funcs.el#L78
;;   (define-key helm-map (kbd "C-j") #'helm-next-line)
;;   (define-key helm-map (kbd "C-k") #'helm-previous-line)
;;   (define-key helm-map (kbd "C-h") #'helm-next-source)
;;   (define-key helm-map (kbd "C-S-h") #'describe-key)
;;   (define-key helm-map (kbd "C-l") (kbd "RET"))
;;   (with-eval-after-load 'helm-files
;;     (dolist (keymap (list helm-find-files-map helm-read-file-map))
;;       (define-key keymap (kbd "C-l") #'helm-execute-persistent-action)
;;       (define-key keymap (kbd "C-h") #'helm-find-files-up-one-level)
;;       ;; rebind `describe-key' for convenience
;;       (define-key keymap (kbd "C-S-h") #'describe-key))))
;; (use-package helm-flx
;;   :config
;;   (setq helm-flx-for-helm-find-files nil)
;;   (helm-flx-mode))
;; (use-package helm-themes
;;   :commands (helm-themes helm-themes--load-theme))

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
    "SPC" #'counsel-M-x
    "TAB" #'switch-to-previous-buffer
    "RET" #'evil-execute-in-emacs-state
    "$"   #'set-selective-display-current-column
    "!"   #'shell-command
    "&"   #'async-shell-command
    "'"   #'shell

    "b"   '(:ignore t :which-key "buffers")
    "bb"  #'ivy-switch-buffer
    "bd"  #'kill-this-buffer
    "bi"  #'ibuffer
    "bci" #'clone-indirect-buffer
    "bco" #'clone-indirect-buffer-other-window

    "c"   #'evil-ex-nohighlight

    "ee"   #'flycheck-list-errors-toggle
    "ec"   #'flycheck-buffer

    "f"   '(:ignore t :which-key "files")
    "ff"  #'counsel-find-file
    "fi"  #'find-user-init-file
    "fr"  #'counsel-recentf
    "fs"  #'save-buffer
    ;; "ft"  #'helm-etags-select
    "fx"  #'delete-file

    "h"   #'help-command

    "i"   '(:ignore t :which-key "UI")
    "if"  #'fci-mode
    "iu"  #'undo-tree-visualize
    "it"  '(:ignore t :which-key "themes")
    "itdo" (lambda () (interactive) (counsel--load-theme-action "doom-one"))
    "itle" (lambda () (interactive) (counsel--load-theme-action "leuven"))
    "itsd" (lambda () (interactive) (counsel--load-theme-action "solarized-dark"))
    "itsl" (lambda () (interactive) (counsel--load-theme-action "solarized-light"))
    "itgs" (lambda () (interactive) (counsel--load-theme-action "gruvbox-light-soft"))
    "itt" #'counsel-load-theme
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
    "ip"  #'show-paren-mode

    "g"   #'magit-status

    "j"   '(:ignore t :which-key "jump")
    ;; "ji"  #'helm-semantic-or-imenu
    "jw"  #'subword-mode

    "k"   #'kill-compilation

    "n"   '(:ignore t :which-key "numbers")
    "n+"  #'evil-numbers/inc-at-pt
    "n-"  #'evil-numbers/dec-at-pt

    "r"   #'revert-buffer

    "s"   '(:ignore t :which-key "server")
    "ss"  #'start-server-if-not-running
    "sx"  #'kill-emacs
    "sn"  #'server-edit

    "u"   #'universal-argument

    "-"   #'negative-argument

    "w"   '(:ignore t :which-key "window")
    "wf"  #'make-frame
    "wh"  #'evil-window-left
    "wj"  #'evil-window-down
    "wk"  #'evil-window-up
    "wo"  #'delete-other-windows
    "wx"  #'ace-delete-window
    "wz"  #'delete-frame
    "wl"  #'evil-window-right
    "wq"  #'delete-window
    "wH"  #'evil-window-move-far-left
    "wL"  #'evil-window-move-far-right
    "wJ"  #'evil-window-move-very-bottom
    "wK"  #'evil-window-move-very-top
    "wQ"  #'kill-buffer-and-window
    "w-"  #'split-window-below
    "w/"  #'split-window-right

    "x"   #'ivy-yasnippet))

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
   "bs" #'scratch))

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
  (setq projectile-completion-system 'ivy)
  (setq projectile-indexing-method 'hybrid)
  (setq projectile-git-submodule-command nil)
  (setq projectile-generic-command "c:/msys64/usr/bin/find.exe . -type f -print0")
        ;; projectile-globally-ignored-file-suffixes
        ;; (append '("~")
                ;; projectile-globally-ignored-file-suffixes))
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

;; (use-package attrap
;;   :after dante)
;; (use-package dante
;;   ;; :demand t
;;   ;; :after haskell-mode
;;   :commands 'dante-mode
;;   :init
;;   (add-hook 'haskell-mode-hook 'flycheck-mode)
;;   (add-hook 'haskell-mode-hook 'dante-mode)
;;   ;; (setq-default dante-repl-command-line '("stack" "ghci"))
;;   :config
;;   (add-hook 'dante-mode-hook
;;    '(lambda () (flycheck-add-next-checker 'haskell-dante
;;                 '(warning . haskell-hlint))))
;;   (add-hook 'dante-mode-hook
;;             (lambda ()
;;               (setq-local company-idle-delay 0.5)))
;;   ;; fix indent
;;   (setq-default haskell-indentation-layout-offset     4
;;                 haskell-indentation-left-offset       4
;;                 haskell-indentation-starter-offset    4
;;                 haskell-indentation-where-post-offset 4
;;                 haskell-indentation-where-pre-offset  4)

;;   (defun haskell-hoogle-lookup-from-local-wrapper ()
;;     (interactive)
;;     (unless (haskell-hoogle-server-live-p) (haskell-hoogle-start-server))
;;     (haskell-hoogle-lookup-from-local))
;;   (mode-leader-define-key haskell-mode-map
;;     "hh" #'hoogle
;;     "hH" #'haskell-hoogle-lookup-from-local-wrapper))

(use-package lsp-mode
  :hook
  ((haskell-mode . lsp)
   (lsp-mode . lsp-enable-which-key-integration))
  :commands lsp
  :init
  (setq-default lsp-modeline-diagnostics-enable nil)
  :config
  ;; mostly copied from Spacemacs
  (spc-leader-define-key
    "l" '(:ignore t :which-key "lsp")
    ;; format
    "l=" '(:ignore t :which-key "format")
    "l=b" #'lsp-format-buffer
    "l=r" #'lsp-format-region
    "l=o" #'lsp-organize-imports
    ;; code actions
    "la" #'lsp-execute-code-action
    ;; goto
    ;; N.B. implementation and references covered by xref bindings / lsp provider...
    "lg" '(:ignore t :which-key "goto")
    "lgt" #'lsp-find-type-definition
    "lgd" #'lsp-find-definition
    "lgM" #'lsp-ui-imenu
    ;; help
    "lh" '(:ignore t :which-key "help")
    "lhh" #'lsp-describe-thing-at-point
    ;; backend
    "lb" '(:ignore t :which-key "backend")
    "lbd" #'lsp-describe-session
    "lbr" #'lsp-workspace-restart
    "lbs" #'lsp-workspace-shutdown
    "lbv" #'lsp-version
    ;; peek
    "lp" '(:ignore t :which-key "peek")
    "lpd" #'lsp-ui-peek-find-definitions
    "lpr" #'lsp-ui-peek-find-references
    ;; refactor
    "lr" '(:ignore t :which-key "refactor")
    "lrr" #'lsp-rename
    ;; toggles
    "lT" '(:ignore t :which-key "toggle")
    "lTd" #'lsp-ui-doc-mode
    "lTs" #'lsp-ui-sideline-mode
    "lTF" '(:which-key "doc-include-signature" :def (lambda () (interactive) (setq lsp-ui-doc-include-signature (not lsp-ui-doc-include-signature))))
    "lTS" '(:which-key "sideline-show-symbol" :def (lambda () (interactive) (setq lsp-ui-sideline-show-symbol (not lsp-ui-sideline-show-symbol))))
    "lTI" '(:which-key "sideline-ignore-duplicate" :def (lambda () (interactive) (setq lsp-ui-sideline-ignore-duplicate (not lsp-ui-sideline-ignore-duplicate))))
    "lTl" #'lsp-lens-mode
    ;; ;; folders
    ;; "F" "folder"
    ;; "Fs" #'lsp-workspace-folders-open
    ;; "Fr" #'lsp-workspace-folders-remove
    ;; "Fa" #'lsp-workspace-folders-add
    ;; text/code
    "lx" '(:ignore t :which-key "text/code")
    "lxh" #'lsp-document-highlight
    "lxl" #'lsp-lens-show
    "lxL" #'lsp-lens-hide))
(use-package lsp-ui
  :commands lsp-ui-mode
  :init
  (setq lsp-ui-sideline-actions-icon nil
        lsp-ui-sideline-show-diagnostics nil
        lsp-ui-peek-always-show t))
(use-package helm-lsp :commands helm-lsp-workspace-symbol)
;; (use-package company-lsp :commands company-lsp)
(use-package lsp-haskell
  :defer
  :init
  ;; fix indent
  (setq-default haskell-indentation-layout-offset     4
                haskell-indentation-left-offset       4
                haskell-indentation-starter-offset    4
                haskell-indentation-where-post-offset 4
                haskell-indentation-where-pre-offset  4)
  :config
  (defun haskell-hoogle-lookup-from-local-wrapper ()
    (interactive)
    (unless (haskell-hoogle-server-live-p) (haskell-hoogle-start-server))
    (haskell-hoogle-lookup-from-local))
  (mode-leader-define-key haskell-mode-map
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
   "s" #'hasky-stack-execute)
  :config
  (magit-define-popup-switch 'hasky-stack-build-popup ?C "No color" "--color=never" t))

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

;; (use-package tidal
;;   :defer
;;   :init
;;   (setq tidal-interpreter "stack"
;;         tidal-interpreter-arguments '("ghci" "--package" "tidal")
;;         tidal-boot-script-path
;;         (concat (substring
;;                  (shell-command-to-string "stack exec --package tidal -- bash -c \"ghc-pkg describe $(ghc-pkg latest tidal) | grep data-dir | cut -f2 -d' '\"")
;;                  0 -1)
;;                 "/BootTidal.hs")))

;; html

(use-package emmet-mode
  :defer t
  :init
  (add-hook 'sgml-mode-hook 'emmet-mode) ;; Auto-start on any markup modes
  (add-hook 'css-mode-hook  'emmet-mode) ;; enable Emmet's css abbreviation.
  :config
  (define-key emmet-mode-keymap (kbd "TAB") #'emmet-expand-line)
  )

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
        preview-gs-command "c:/Program Files/gs/gs9.53.3/bin/gswin64c.exe"
        preview-auto-reveal t)
  (add-hook 'LaTeX-mode-hook 'TeX-source-correlate-mode)
  (add-hook 'LaTeX-mode-hook 'TeX-PDF-mode)

  ;; reftex
  (add-hook 'LaTeX-mode-hook 'turn-on-reftex)
  (setq reftex-plug-into-AUCTeX t
        reftex-use-fonts t
        reftex-insert-label-flags '("s" "sfte"))

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
   "k"   #'TeX-kill-job
   "q"   #'TeX-next-error
   "\\"  #'TeX-electric-macro

   "p"   '(:ignore t :which-key "preview")
   "pb"  #'preview-buffer
   "pp"  #'preview-at-point
   "pr"  #'preview-region
   "pc"  '(:ignore t :which-key "clearout")
   "pcb" #'preview-clearout-buffer
   "pcp" #'preview-clearout-at-point

   "r"   '(:ignore t :which-key "reftex")
   "rc"  #'reftex-citation
   "rl"  #'reftex-label
   "rr"  #'reftex-reference
   "rv"  #'reftex-view-crossref)

  ;; Rebindings for TeX-font - lifted from spacemacs
  (defun latex/font-bold         () (interactive) (TeX-font nil ?\C-b))
  (defun latex/font-code         () (interactive) (TeX-font nil ?\C-t))
  (defun latex/font-emphasis     () (interactive) (TeX-font nil ?\C-e))
  (defun latex/font-italic       () (interactive) (TeX-font nil ?\C-i))
  (defun latex/font-small-caps   () (interactive) (TeX-font nil ?\C-c))
  (defun latex/font-sans-serif   () (interactive) (TeX-font nil ?\C-f))

  (defvar latex-current-insert-command nil "The command inserted by C-w.")
  (defun latex/font-current (arg)
    (interactive "*P")
    (if arg
        (progn
          (call-interactively 'TeX-insert-macro)
          (setq latex-current-insert-command TeX-default-macro))
      (if latex-current-insert-command
          (TeX-insert-macro latex-current-insert-command)
        (call-interactively 'TeX-insert-macro)
        (setq latex-current-insert-command TeX-default-macro))))

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
  ;; (add-hook 'LaTeX-mode-hook
  ;;           (lambda ()
  ;;             (start-server-if-not-running)))

  ;; adapted from https://lists.nongnu.org/archive/html/auctex/2009-11/msg00016.html
  (evil-define-key 'insert TeX-mode-map (kbd "C-\\") 'TeX-electric-macro)

  (add-to-list 'TeX-tree-roots "c:/Users/bradn/AppData/Roaming/MiKTeX/2.9/")

  ;; from https://tex.stackexchange.com/questions/286028/inverse-search-with-emacs-auctex-and-sumatrapdf-on-windows-10
  (setq TeX-PDF-mode t)
  (setq TeX-source-correlate-mode t)
  (setq TeX-source-correlate-method 'synctex)
  (setq TeX-view-program-list
        '(("Sumatra PDF" ("\"C:/Users/bradn/AppData/Local/SumatraPDF/SumatraPDF.exe\" -reuse-instance" (mode-io-correlate " -forward-search \"%b\" %n ") " %o"))))
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
(add-to-list 'package-pinned-packages '(org . "gnu"))
(unless (package--user-installed-p 'org)
  ;; taken from use-package’s use-package-ensure-elpa
  (condition-case-unless-debug err
      (progn
        (when (assoc package (bound-and-true-p
                              package-pinned-packages))
          (package-read-all-archive-contents))
        (if (assoc package package-archive-contents)
            (package-install package)
          (package-refresh-contents)
          (when (assoc package (bound-and-true-p
                                package-pinned-packages))
            (package-read-all-archive-contents))
          (package-install package))
        t)
    (error
     (display-warning 'use-package
                      (format "Failed to install %s: %s"
                              name (error-message-string err))
                      :error))))

(use-package org
  ;; :ensure t
  ;; :pin org
  :defer
  :init
  (spc-leader-define-key
    "o" '(:ignore t :which-key "org-agenda")
    "oa" #'org-agenda
    "ob" #'org-switchb
    "oc" #'org-cycle-agenda-files
    "ok" #'org-capture
    "os" #'org-save-all-org-buffers)
  :config

  ;; org-protocol
  (require 'org-protocol)

  (defun org-protocol-capture-notemplate (info)
    (raise-frame)
    (org-capture)
    (message "Item captured.")
    ;; Make sure we do not return a string, as `server-visit-files',
    ;; through `server-edit', would interpret it as a file name.
    nil)
  (add-to-list 'org-protocol-protocol-alist
               '("org-capture-notemplate" :protocol "capturent" :function org-protocol-capture-notemplate :kill-client t))

  (defun toggle-org-hide-stars ()
    (interactive)
    (setq-local org-hide-leading-stars (not org-hide-leading-stars))
    ;; restart font-lock-mode
    (font-lock-mode nil)
    (font-lock-mode t))
  (mode-leader-define-key org-mode-map
    "e" #'org-export-dispatch
    "i" #'org-insert-item
    "t" #'org-todo
    "w" #'org-refile
    "K" #'org-ctrl-c-ctrl-c
    "r" #'org-reveal
    "s" '(:ignore t :which-key "scheduling")
    "ss" #'org-schedule
    "sd" #'org-deadline
    "s." #'org-time-stamp
    "g" #'org-set-tags-command
    "*" #'toggle-org-hide-stars
    "," #'org-insert-structure-template)
  ;; (mode-leader-define-key org-agenda-mode-map
  ;;   "c" #'org-agenda-columns
  ;;   "o" #'org-agenda-open-link
  ;;   "t" #'org-agenda-todo
  ;;   "s" '(:ignore t :which-key "scheduling")
  ;;   "ss" #'org-agenda-schedule
  ;;   "sd" #'org-agenda-deadline)

  (use-package evil-org
    :ensure t
    :init
    (require 'evil)
    :config
    (add-hook 'org-mode-hook 'evil-org-mode)
    (add-hook 'evil-org-mode-hook
              (lambda ()
                (evil-org-set-key-theme)))
    (require 'evil-org-agenda)
    ;; (evil-org-agenda-set-keys)
    ;; (evil-define-key 'motion org-agenda-mode-map
    ;;   "X" #'org-agenda-columns)
    (evil-define-key 'emacs org-agenda-mode-map
      "j" #'org-agenda-next-line
      "k" #'org-agenda-previous-line
      "J" #'org-agenda-next-item
      "K" #'org-agenda-previous-item
      "c" #'org-agenda-capture
      "x" #'org-agenda-execute
      "G" #'evil-goto-line
      "gg" #'evil-goto-first-line
      "gr" #'org-agenda-redo-all)
    (if (fboundp #'server-edit)
        (evil-define-key 'emacs org-agenda-mode-map
          "Q" #'delete-frame)))

  ;; adapted from https://emacs.stackexchange.com/a/14734/20375
  (defun org-agenda-skip-if-blocked ()
    (let ((next-headline (save-excursion (or (outline-next-heading) (point-max)))))
      (if (org-entry-blocked-p) next-headline)))
  (setq org-agenda-files '("~/Dropbox/org")
        ;; meaning:
        ;; TODO, NEXT, INPROGRESS, DONE are self-explanatory
        ;; WAITING is for tasks which are waiting for some issue to be resolved
        ;; HOLD is for tasks which are waiting indefinitely
        ;; CANCELLED is for tasks for which there is no intention of completion
        ;; UNANSWERED is for questions which haven’t been asked yet
        ;; RESOLVED is for questions which have been asked and answered
        ;; ASKED is for questions which have been asked but not answered
        org-todo-keywords
        '((sequence "TODO(t)" "NEXT(n)" "INPROGRESS(p)" "|" "DONE(d)")
          (sequence "WAITING(w@)" "HOLD(h@)" "|" "CANCELLED(x@)")
          (sequence "UNANSWERED(u)" "|" "RESOLVED(r@/@)")
          ;; (sequence "UNANSWERED(u)" "ASKNEXT(a)" "|" "RESOLVED(r@/@)")
          (sequence "QINACTIVE(v)" "|" "NOTASKING(k@)")
          (sequence "UNREAD(x)" "|" "READ(z)"))
        org-agenda-custom-commands
        '(("p" "Block agenda"
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
          ("u" "University"
           ((tags "REFILE"
                  ((org-agenda-overriding-header "To refile")))
            ;; (tags-todo "+university+assignment+SCHEDULED<=\"<now>\"/!TODO"
            ;;            ((org-agenda-overriding-header "Scheduled assignments")))
            (tags "+university/NEXT"
                  ((org-agenda-overriding-header "Next tasks")))
            (tags "+university+exam+TIMESTAMP<\"<+3w>\""
                  ((org-agenda-overriding-header "Exams")))
            (tags "+university+lecture/!"
                  ((org-agenda-overriding-header "Unwatched lectures")
                   ;; (org-agenda-skip-function '(org-agenda-skip-entry-if 'timestamp))
                   ;; (org-agenda-skip-function '(org-agenda-skip-if-blocked))
                   (org-agenda-dim-blocked-tasks 'invisible)))
            ;; (tags-todo "+university-lecture-assignment+DEADLINE<\"<+2w>\"/!"
            (tags-todo "+university-lecture+DEADLINE<\"<now>\"/!"
                       ((org-agenda-overriding-header "Overdue things")))
            (tags-todo "+university-lecture+DEADLINE<\"<+2w>\"/!"
                       ((org-agenda-overriding-header "Deadlines")
                        (org-agenda-sorting-strategy '(deadline-up))))
            (tags-todo "+university+assignment+SCHEDULED=\"\"+DEADLINE<\"<+3w>\"|+university+assignment+deadline=\"\"|+university+assignment+SCHEDULED<=\"<now>\"/!TODO|NEXT|INPROGRESS|WAITING"
                       ((org-agenda-overriding-header "Assignments")
                        (org-agenda-sorting-strategy '(deadline-up))))
            (agenda ""
                    ((org-agenda-span 14)
                     ;; (org-agenda-start-day "-1d")
                     ;; (org-agenda-start-on-weekday nil)
                     (org-deadline-warning-days 0)
                     (org-agenda-skip-function '(org-agenda-skip-entry-if 'todo '("HOLD")))
                     (org-agenda-before-sorting-filter-function
                      (lambda (line)
                        (unless (and (string-match "Sched\\." line)
                                     (equal "INPROGRESS" (get-text-property 0 'todo-state line)))
                          line)))))
            (tags-todo "+university/INPROGRESS"
                       ((org-agenda-overriding-header "In progress")))
            (tags "+university-lecture-assignment/!TODO"
                  ((org-agenda-overriding-header "TODO tasks")
                   (org-agenda-skip-function '(org-agenda-skip-entry-if 'timestamp))))
            (tags "+university/HOLD|WAITING"
                  ((org-agenda-overriding-header "On hold")
                   (org-agenda-sorting-strategy '(todo-state-up))))))
          ;; ((org-agenda-overriding-columns-format "%25ITEM %25DEADLINE"))
          ("q" "Questions"
           (;; (todo "ASKNEXT"
            ;;       ((org-agenda-overriding-header "Ask next opportunity")))
            (tags "+university/!UNANSWERED|WAITING"
                  ((org-agenda-overriding-header "Unanswered questions (University)")))
            (tags "-university/!UNANSWERED|WAITING"
                  ((org-agenda-overriding-header "Unanswered questions (Non-university)")))
            (tags "+university/RESOLVED"
                  ((org-agenda-overriding-header "Resolved questions (University)")))
            (tags "-university/RESOLVED"
                  ((org-agenda-overriding-header "Resolved questions (Non-university)")))
            (tags "+university/!ASKED"
                  ((org-agenda-overriding-header "Unresolved questions (University)")))
            (tags "-university/!ASKED"
                  ((org-agenda-overriding-header "Resolved questions (Non-university)")))
            (todo "QINACTIVE|NOTASKING"
                  ((org-agenda-overriding-header "Inactive questions (all)"))))))
        org-agenda-show-outline-path t
        org-agenda-breadcrumbs-separator "→"
        org-refile-targets
        '((nil :maxlevel . 9)
          (org-agenda-files :maxlevel . 9))
        org-refile-use-outline-path 'file
        org-outline-path-complete-in-steps nil ; for helm, see https://blog.aaronbieber.com/2017/03/19/organizing-notes-with-refile.html
        org-refile-allow-creating-parent-nodes 'confirm
        org-capture-templates
        '(("t" "todo" entry (file "~/Dropbox/org/refile.org")
           "* TODO %?")
          ("e" "event" entry (file "~/Dropbox/org/refile.org")
           "* %?\n  %^T")
          ("q" "question" entry (file "~/Dropbox/org/refile.org")
           "* UNANSWERED %?"))
        org-archive-mark-done nil
        org-archive-location "%s_archive::* Archived Tasks"
        org-enforce-todo-dependencies t
        org-track-ordered-property-with-tag nil
        org-agenda-dim-blocked-tasks t
        org-enforce-todo-checkbox-dependencies t
        org-log-into-drawer t
        org-startup-folded t)

  ;; (defun format-org-breadcrumbs (sep max-item-length max-output-length brds)
  ;;   (let* ((parts (split-string brds sep))
  ;;          (cdrs (s-join sep (mapcar (lambda (s) (s-truncate max-item-length s "..")) (cdr parts))))
  ;;          (cdrs-length (min (length cdrs) (- max-output-length (1+ (length (car parts)))))))
  ;;     (concat
  ;;      (s-truncate cdrs-length cdrs "..")
  ;;      (s-repeat (- max-output-length (+ (length (car parts)) cdrs-length)) " ")
  ;;      (car parts))))

  (defun format-org-breadcrumbs (sep max-item-length max-output-length brds)
    (let ((parts (split-string brds sep)))
      (s-truncate max-output-length
                  (s-join sep
                          (mapcar (lambda (s) (s-truncate max-item-length s "..")) parts))
                  "¬")))

  (defun formatted-breadcrumbs (m n)
    (format "%%-%d%S" n
            `(let ((breadcrumbs (org-with-point-at (org-get-at-bol 'org-marker) (org-display-outline-path nil nil org-agenda-breadcrumbs-separator t))))
               (if (or (equal breadcrumbs "") (equal breadcrumbs nil))
                   ""
                 (format-org-breadcrumbs org-agenda-breadcrumbs-separator ,m ,n
                                         (concat breadcrumbs ""
                                                 (get-text-property 0 'extra-space breadcrumbs)))))))
  (setq org-agenda-prefix-format
        `((todo . ,(concat " " (formatted-breadcrumbs 10 25) " %i %-12:c"))
          (tags . ,(concat " " (formatted-breadcrumbs 10 25) " %i %-12:c"))
          (agenda . ,(concat " " (formatted-breadcrumbs 10 25) " %i %-12:c%?-12t% s"))
          (search . " %i %-12:c")))

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

;; extempore
(use-package extempore-mode
  :defer
  :init
  (setq extempore-use-pretty-lambdas nil
        extempore-path "c:/Users/bradn/Documents/Music/Extempore/extempore-v0.8.6-lens-windows-latest/extempore/")
  :config
  (mode-leader-define-key extempore-mode-map
    "K" #'extempore-send-definition
    "r" #'extempore-send-region
    "x" #'extempore-send-last-sexp
    "z" #'switch-to-extempore
    "j" #'extempore-connect)
  (evil-define-key '(normal insert) extempore-mode-map (kbd "C-SPC") #'extempore-send-definition)
  (add-hook 'extempore-mode-hook (lambda () (lispy-mode 1))))

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
 '(org-agenda-date-today ((t (:inherit org-agenda-date :background "light gray" :slant italic :weight bold))))
 '(preview-reference-face ((t (:height 1.5)))))

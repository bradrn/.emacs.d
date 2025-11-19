;;; -*- lexical-binding: t -*-

;; from https://blog.d46.us/advanced-emacs-startup/
;; Use a hook so the message doesn't get clobbered by other messages.
(add-hook 'emacs-startup-hook
          (lambda ()
            (message "Emacs ready in %s with %d garbage collections."
                     (format "%.2f seconds"
                             (float-time
                              (time-subtract after-init-time before-init-time)))
                     gcs-done)))

;; basic config

(defvar last-file-name-handler-alist file-name-handler-alist)
(setq gc-cons-threshold 402653184
      gc-cons-percentage 0.6
      file-name-handler-alist nil)

(setq package-quickstart t)
(setq vc-make-backup-files t)
(setq backup-directory-alist `(("." . "~/.emacs.d/backups")))
(setq inhibit-startup-screen t)
(set-language-environment "UTF-8")
(setq-default inhibit-compacting-font-caches t)
(setq backup-by-copying t)
(setq read-process-output-max (* 1024 1024))
(setq use-short-answers t)
(setq initial-major-mode 'fundamental-mode
      initial-scratch-message nil)


;; appearance

(add-to-list 'default-frame-alist '(fullscreen . maximized))  ; https://emacs.stackexchange.com/a/3008
(setq-default indent-tabs-mode nil)
(setq tab-width 4)
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

;; see https://emacs.stackexchange.com/a/50136/20375
; (global-linum-mode -1)
(setq display-line-numbers-type 'visual)
(global-display-line-numbers-mode)

(column-number-mode)

(add-to-list 'default-frame-alist '(font . "Consolas 11"))

(setq variable-font-face-tuple '(:family "Scala Sans Pro" :height 113))

(custom-theme-set-faces
  'user
  `(variable-pitch ((t ,variable-font-face-tuple)))
  '(fixed-pitch ((t (:family "Consolas" :height 113))))
  '(line-number ((t (:inherit (shadow fixed-pitch))))))


;; UI

(electric-pair-mode 1)
(show-paren-mode 1)

(setq scroll-preserve-screen-position t
      scroll-margin 3
      scroll-conservatively 101)

(setopt switch-to-buffer-obey-display-actions t)

(let ((editing-hooks '(text-mode-hook prog-mode-hook)))
  (mapc (lambda (hook)
          (add-hook hook
                    #'(lambda () (setq show-trailing-whitespace t))))
        editing-hooks))

(setq recentf-auto-cleanup 'never
      recentf-max-menu-items 50)
(recentf-mode 1)

(setq ispell-dictionary "british")
(setq-default ispell-program-name
              (if (eq system-type 'windows-nt)
                  "c:/cygwin64/bin/aspell.exe"
                "/usr/bin/aspell"))

;; utilities

;; thanks http://emacsredux.com/blog/2013/05/18/instant-access-to-init-dot-el/
(defun find-user-init-file ()
  "Edit the `user-init-file', in another window."
  (interactive)
  (find-file user-init-file))

(defun find-insert-relative-path (filename)
  (interactive
   (list (read-file-name "Find file: " nil default-directory
                         (confirm-nonexistent-file-or-buffer))))
  (insert (file-relative-name filename)))

;; thanks https://emacs.stackexchange.com/a/18064/20375
(defun switch-to-previous-buffer ()
  (interactive)
  (switch-to-buffer (other-buffer)))

(defun set-selective-display-current-column ()
  (interactive)
  (set-selective-display
   (if selective-display nil (1+ (current-column)))))

;; server - from https://stackoverflow.com/a/5571983/7345298
(defun start-server-if-not-running ()
  "Start the Emacs server if it is not already running."
  (interactive)
  (load "server")
  (unless (server-running-p) (server-start)))
(start-server-if-not-running)

;; packaging

(setq package-archives
      '(("gnu"   . "http://elpa.gnu.org/packages/")
        ("melpa" . "http://melpa.org/packages/")))
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)
(setq use-package-always-ensure t)

(use-package try :defer t)


;; keybinds

(use-package evil
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
  ;; disambiguate C-i and TAB
  (define-key input-decode-map [?\C-i] [C-i])

  (evil-set-initial-state 'dashboard-mode 'emacs)
  (evil-set-initial-state 'sly-db-mode 'emacs)
  (evil-set-initial-state 'calc-mode 'emacs)
  (evil-set-initial-state 'comint-mode 'normal)
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
  :defer
  :commands (evil-escape-pre-command-hook)
  :init
  (setq-default evil-escape-key-sequence "jw"
                evil-escape-delay 0.2)
  (add-hook 'pre-command-hook 'evil-escape-pre-command-hook))
(use-package evil-exchange
  :defer
  :init
  (setq evil-exchange-key "gx"
        evil-exchange-cancel-key "gX")
  (define-key evil-normal-state-map evil-exchange-key 'evil-exchange)
  (define-key evil-visual-state-map evil-exchange-key 'evil-exchange)
  (define-key evil-normal-state-map evil-exchange-cancel-key 'evil-exchange-cancel)
  (define-key evil-visual-state-map evil-exchange-cancel-key 'evil-exchange-cancel))
;; (use-package evil-indent-plus :config (evil-indent-plus-default-bindings))
(use-package evil-lion
  :defer
  :init
  (setq evil-lion-left-align-key "gl"
        evil-lion-right-align-key "gL")
  (define-key evil-normal-state-map evil-lion-left-align-key 'evil-lion-left)
  (define-key evil-visual-state-map evil-lion-left-align-key 'evil-lion-left)
  (define-key evil-normal-state-map evil-lion-right-align-key 'evil-lion-right)
  (define-key evil-visual-state-map evil-lion-right-align-key 'evil-lion-right))
(use-package evil-matchit
  :defer
  :init
  (evil-set-command-property 'evilmi-jump-items :keep-visual t)
  (define-key evil-normal-state-map "%" 'evilmi-jump-items)
  (define-key evil-visual-state-map "%" 'evilmi-jump-items))
(use-package evil-nerd-commenter :commands (evilnc-comment-operator))
(use-package evil-numbers)
(use-package evil-surround :config (global-evil-surround-mode 1))
(use-package evil-snipe
  :defer
  :init
  (setq evil-snipe-scope 'buffer
        evil-snipe-smart-case t)
  (evil-snipe-mode 1))
(use-package evil-unimpaired
  :load-path "~/.emacs.d/evil-unimpaired"
  :config
  (evil-unimpaired-mode t)
  (use-package move-text
    :defer
    :init
    (evil-unimpaired-define-pair "e" '(move-text-up . move-text-down))))
(use-package evil-collection
  :config
  (delete 'lispy evil-collection-mode-list)  ; handled by lispyville
  (delete 'calc evil-collection-mode-list)   ; use default bindings
  (evil-collection-init))

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

(use-package vertico
  :init
  (setq enable-recursive-minibuffers t)

  (setq vertico-count 20
        vertico-preselect 'directory
        vertico-cycle t
        vertico-sort-function #'vertico-sort-alpha)

  (vertico-mode)
  (vertico-multiform-mode)

  ;; Sort directories before files
  ;; (from vertico README)
  (defun sort-directories-first (files)
    (setq files (vertico-sort-history-length-alpha files))
    (nconc (seq-filter (lambda (x) (string-suffix-p "/" x)) files)
           (seq-remove (lambda (x) (string-suffix-p "/" x)) files)))

  (setq vertico-multiform-categories
        '((file (vertico-sort-function . sort-directories-first))))

  ;; Add prompt indicator to `completing-read-multiple'.
  ;; We display [CRM<separator>], e.g., [CRM,] if the separator is a comma.
  (defun crm-indicator (args)
    (cons (format "[CRM%s] %s"
                  (replace-regexp-in-string
                   "\\`\\[.*?]\\*\\|\\[.*?]\\*\\'" ""
                   crm-separator)
                  (car args))
          (cdr args)))
  (advice-add #'completing-read-multiple :filter-args #'crm-indicator)

  :config
  (evil-define-key '(normal insert) vertico-map (kbd "C-j") #'vertico-next)
  (evil-define-key '(normal insert) vertico-map (kbd "C-k") #'vertico-previous))

(use-package vertico-directory
  :after vertico
  :ensure nil
  :bind (:map vertico-map
              ("RET" . vertico-directory-enter)
              ("C-l" . vertico-directory-enter)
              ("C-h" . vertico-directory-up))
  ;; Tidy shadowed file names
  :hook (rfn-eshadow-update-overlay . vertico-directory-tidy))

(use-package vertico-quick
  :after vertico
  :ensure nil
  :init
  (evil-define-key '(normal insert) vertico-map (kbd "M-w") #'vertico-quick-insert)
  (evil-define-key '(normal insert) vertico-map (kbd "C-w") #'vertico-quick-exit))

(use-package orderless
  :init
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))))

(use-package consult
  :init
  (setq consult-preview-key nil)
  :config
  ;; from consult README

  ;; Use `consult-completion-in-region' if Vertico is enabled.
  ;; Otherwise use the default `completion--in-region' function.
  (setq completion-in-region-function
        (lambda (&rest args)
          (apply (if vertico-mode
                     #'consult-completion-in-region
                   #'completion--in-region)
                 args)))

  (consult-customize
   consult-theme :preview-key '(:debounce 0.2 any)
   consult-ripgrep :preview-key '(:debounce 0.2 any)
   consult-line  :preview-key '(:debounce 0.2 any)))

(use-package marginalia
  :bind (:map minibuffer-local-map ("M-A" . marginalia-cycle))

  :init
  (setq marginalia-align 'right)

  (marginalia-mode))

(use-package which-key
  :config
  (setq which-key-allow-evil-operators t)
  (which-key-mode)
  (which-key-enable-god-mode-support))

(use-package cbm
  :defer)

;; from hydra-examples
(defun hydra-move-splitter-left (arg)
  "Move window splitter left."
  (interactive "p")
  (if (let ((windmove-wrap-around))
        (windmove-find-other-window 'right))
      (shrink-window-horizontally arg)
    (enlarge-window-horizontally arg)))

(defun hydra-move-splitter-right (arg)
  "Move window splitter right."
  (interactive "p")
  (if (let ((windmove-wrap-around))
        (windmove-find-other-window 'right))
      (enlarge-window-horizontally arg)
    (shrink-window-horizontally arg)))

(defun hydra-move-splitter-up (arg)
  "Move window splitter up."
  (interactive "p")
  (if (let ((windmove-wrap-around))
        (windmove-find-other-window 'up))
      (enlarge-window arg)
    (shrink-window arg)))

(defun hydra-move-splitter-down (arg)
  "Move window splitter down."
  (interactive "p")
  (if (let ((windmove-wrap-around))
        (windmove-find-other-window 'up))
      (shrink-window arg)
    (enlarge-window arg)))

(use-package hydra
  :config
  (defhydra hydra-window ()
    "
Movement^^        ^Split^         ^Switch^		^Resize^
----------------------------------------------------------------
_h_ ←       	_/_ vertical    _b_uffer		_H_ X←
_j_ ↓        	_-_ horizontal	 _f_ile		_J_ X↓
_k_ ↑        	_z_ undo      	_a_ce 1		_K_ X↑
_l_ →        	_Z_ reset      	_s_wap		_L_ X→
_F_ollow		_D_lt Other   	_S_ave		max_i_mize
_q_ cancel	_o_nly this   	_d_elete
"
    ("h" windmove-left )
    ("j" windmove-down )
    ("k" windmove-up )
    ("l" windmove-right )
    ("H" hydra-move-splitter-left)
    ("J" hydra-move-splitter-down)
    ("K" hydra-move-splitter-up)
    ("L" hydra-move-splitter-right)
    ("F" follow-mode)
    ("b" consult-buffer)
    ("f" find-file)
    ("a" (lambda ()
           (interactive)
           (ace-window 1)
           (add-hook 'ace-window-end-once-hook
                     'hydra-window/body))
     )
    ("/" (lambda ()
           (interactive)
           (split-window-right)
           (windmove-right))
     )
    ("-" (lambda ()
           (interactive)
           (split-window-below)
           (windmove-down))
     )
    ("s" (lambda ()
           (interactive)
           (ace-window 4)
           (add-hook 'ace-window-end-once-hook
                     'hydra-window/body)))
    ("S" save-buffer)
    ("d" delete-window)
    ("D" (lambda ()
           (interactive)
           (ace-window 16)
           (add-hook 'ace-window-end-once-hook
                     'hydra-window/body))
     )
    ("o" delete-other-windows)
    ("i" ace-maximize-window)
    ("z" (progn
           (winner-undo)
           (setq this-command 'winner-undo))
     )
    ("Z" winner-redo)
    ("q" nil)))

(use-package general
  :config
  (general-auto-unbind-keys)
  (general-create-definer spc-leader-define-key
    :states '(normal visual insert emacs)
    :keymaps 'override
    :prefix "SPC"
    :non-normal-prefix "M-SPC")

  (general-create-definer mode-leader-define-key
    :states '(normal visual insert emacs)
    :prefix "K"
    :non-normal-prefix "C-0")

  (spc-leader-define-key
    "SPC" #'execute-extended-command
    "TAB" #'switch-to-previous-buffer
    "`" #'cbm-cycle
    "RET" #'evil-execute-in-emacs-state
    "$"   #'set-selective-display-current-column
    "!"   #'shell-command
    "&"   #'async-shell-command
    "'"   #'shell

    "b"   '(:ignore t :which-key "buffers")
    "bb"  #'consult-buffer
    "bp"  #'consult-project-buffer
    "bd"  #'kill-current-buffer
    "bi"  #'ibuffer
    "bci" #'clone-indirect-buffer
    "bco" #'clone-indirect-buffer-other-window

    "c"   #'evil-ex-nohighlight

    "f"   '(:ignore t :which-key "files")
    "ff"  #'find-file
    "fi"  #'find-user-init-file
    "fd"  #'consult-find
    "fm"  #'rename-visited-file
    "fr"  #'consult-recent-file
    "fs"  #'save-buffer
    "fw"  #'write-file
    "fx"  #'delete-file
    "fy"  #'find-insert-relative-path

    "h"   #'help-command

    "i"   '(:ignore t :which-key "UI")
    "ic"  '(:ignore t :which-key "customise")
    "icf" #'customize-face
    "icg" #'customize-group
    "icv" #'customize-variable
    "id"  #'dired
    "if"  #'display-fill-column-indicator-mode
    "io"  #'column-number-mode
    "il"  '(:ignore t :which-key "calc")
    "ilc" #'calc
    "ilq" #'quick-calc
    "in"  #'set-frame-font
    "ip"  #'show-paren-mode
    "is" '(:ignore t :which-key "flyspell")
    "iss" #'flyspell-mode
    "isx" #'flyspell-buffer
    "it"  '(:ignore t :which-key "themes")
    "itml" (lambda () (interactive) (consult-theme 'modus-operandi))
    "itmt" (lambda () (interactive) (consult-theme 'modus-operandi-tinted))
    "itmd" (lambda () (interactive) (consult-theme 'modus-vivendi))
    "itdo" (lambda () (interactive) (consult-theme 'doom-one))
    "itdi" (lambda () (interactive) (consult-theme 'dichromacy))
    "itgs" (lambda () (interactive) (consult-theme 'gruvbox-light-soft))
    "itle" (lambda () (interactive) (consult-theme 'leuven))
    "itpd" (lambda () (interactive) (consult-theme 'spacemacs-dark))
    "itpl" (lambda () (interactive) (consult-theme 'spacemacs-light))
    "itsd" (lambda () (interactive) (consult-theme 'solarized-dark))
    "itsl" (lambda () (interactive) (consult-theme 'solarized-light))
    "itt" #'consult-theme
    "itx"  (lambda () (interactive) (mapc #'disable-theme custom-enabled-themes))
    "iu"  #'undo-tree-visualize
    "iw"  #'whitespace-mode
    "ix"  #'toggle-truncate-lines
    "iX"  #'toggle-word-wrap

    "j"   '(:ignore t :which-key "jump")
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

    "/"   '(:ignore t :which-key "find")
    "//"  #'consult-line
    "/b"  #'browse-url-at-point
    "/f"  #'ffap
    "/F"  #'(lambda () (interactive) (consult-find t))
    "/r"  #'consult-ripgrep
    "/m"  #'consult-imenu
    "/M"  #'consult-imenu-multi

    "w"   '(:ignore t :which-key "window")
    "wf"  #'make-frame
    "wh"  #'evil-window-left
    "wj"  #'evil-window-down
    "wk"  #'evil-window-up
    "wo"  #'delete-other-windows
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
    "ww"  #'hydra-window/body)

  (evil-define-key 'normal with-editor-mode-map
    (kbd "KK") 'with-editor-finish
    (kbd "Kk") 'with-editor-finish
    (kbd "KC") 'with-editor-cancel
    (kbd "Kc") 'with-editor-cancel))

(use-package avy
  :defer
  :init
  (defun avy-goto-char-forward-char (char &optional arg)
    "Run `avy-goto-char', then move forward one character.
CHAR and ARG are as in avy."
    (interactive (list (read-char "char: " t)
                       current-prefix-arg))
    (avy-goto-char char arg)
    (forward-char))
  (spc-leader-define-key
    "jj" #'evil-avy-goto-char
    "jJ" #'evil-avy-goto-char-2
    "jk" #'avy-goto-char-forward-char
    "jl" #'avy-goto-line))

(use-package winum
  :init
  (setq winum-auto-assign-0-to-minibuffer nil)
  (add-hook 'emacs-startup-hook (lambda () (winum-mode)))
  :config

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
  (set-face-attribute 'winum-face nil :weight 'bold :inverse-video t))


;; appearance - external

(use-package doom-themes
  :defer
  :config
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t)
  (doom-themes-org-config))
(use-package solarized-theme
  :defer
  :init
  (setq solarized-use-variable-pitch nil
        solarized-scale-org-headlines nil))
(use-package gruvbox-theme :defer)
(use-package spacemacs-theme :defer)

(use-package hl-todo :defer 3 :config (global-hl-todo-mode))

;; (use-package fill-column-indicator
;;   :defer
;;   :init
;;   (setq fci-always-use-textual-rule t
;;         fci-rule-character 9474)
;;   (spc-leader-define-key
;;     "if" #'fci-mode))

(use-package rainbow-delimiters
  :init
  (add-hook 'prog-mode-hook 'rainbow-delimiters-mode))

(use-package highlight-indent-guides
  :defer t
  :init
  (setq highlight-indent-guides-method 'character
        highlight-indent-guides-auto-character-face-perc 10
        highlight-indent-guides-responsive 'top)
  (spc-leader-define-key
   "ig" #'highlight-indent-guides-mode))

(use-package idle-highlight-mode
  :defer
  :init
  (spc-leader-define-key
    "ih" #'idle-highlight-mode))


;; UI

(use-package undo-tree
  :init
  (setq undo-tree-auto-save-history t
        undo-tree-history-directory-alist '(("." . "~/.emacs.d/undo")))
  :config
  (global-undo-tree-mode 1))

(use-package dired
  :ensure nil
  :init
  (setq dired-kill-when-opening-new-dired-buffer t)
  :config
  (put 'dired-find-alternate-file 'disabled nil))


(use-package magit
  :defer
  :init
  (spc-leader-define-key
    "g" #'magit-status
    "G" #'magit-blame)
  (setq magit-diff-refine-hunk 'all)
  :config
  (add-hook 'git-commit-setup-hook 'evil-insert-state))

(use-package yasnippet
  :defer t
  :init
  ;; don’t use yas-global-mode - it takes too much time at startup
  ;; instead only enable it in modes where I use it:
  (add-hook 'LaTeX-mode-hook #'yas-minor-mode)
  (add-hook 'haskell-mode-hook #'yas-minor-mode)
  (add-hook 'rustic-mode-hook #'yas-minor-mode)
  (add-hook 'yas-minor-mode-hook #'yas-reload-all))
(use-package yasnippet-snippets
  :after yasnippet)

(use-package company
  :defer t
  :init
  (setq company-idle-delay 0.3
        company-dabbrev-downcase nil)
  (add-hook 'prog-mode-hook #'(lambda () (company-mode 1)))
  (add-hook 'comint-mode-hook #'(lambda () (company-mode 1)))

  ;; based on counsel-company
  (defun move-company-to-minibuffer ()
    (interactive)
    (company-mode 1)
    (unless company-candidates
      (company-complete))
    (when company-candidates
      (company--continue)
      (let ((candidate (completing-read "Candidate: " company-candidates)))
        (company-finish candidate))))

  :config
  (define-key company-active-map (kbd "C-l") #'company-complete-selection)
  (define-key company-active-map (kbd "C-:") #'move-company-to-minibuffer)
  (define-key company-mode-map   (kbd "C-:") #'move-company-to-minibuffer)
  (spc-leader-define-key "im" #'company-mode))


(use-package flycheck
  :defer t
  :init
  (setq-default flycheck-display-errors-function #'flycheck-display-error-messages-unless-error-list)
  (setq flycheck-check-syntax-automatically '(save mode-enabled)
        flycheck-navigation-minimum-level 'error)
  (add-hook 'prog-mode-hook #'flycheck-mode)
  (spc-leader-define-key
    "ee" #'flycheck-list-errors
    "ec" #'flycheck-buffer))

;; (use-package flymake
;;   :config
;;   (spc-leader-define-key
;;     "ee" #'flymake-show-buffer-diagnostics
;;     "ep" #'flymake-show-project-diagnostics))

(use-package projectile
  :defer nil
  :init
  (if (eq system-type 'windows-nt)
      (setq projectile-indexing-method 'hybrid
            projectile-git-submodule-command nil
            projectile-generic-command "c:/msys64/usr/bin/find.exe . -type f -print0"))
  (setq projectile-compile-use-comint-mode t
        projectile-run-use-comint-mode t)

  :config
  (projectile-mode 1)
  (spc-leader-define-key
    "p" 'projectile-command-map)

  (defun my-projectile-compile-and-run (arg)
    (interactive "P")
    (let ((old-cff compilation-finish-functions))
      ;; can't use 'let' here because setting must persist till end of
      ;; async compilation
      (setq compilation-finish-functions
            (lambda (buf msg)
              (setq compilation-finish-functions old-cff)
              (if (string-match "finished" msg)
                  ;; TODO should use 'arg' here but binding gets in
                  ;; the way
                  (projectile-run-project nil))
              t))
      (projectile-compile-project arg)))
  (keymap-set projectile-command-map (kbd "r") #'my-projectile-compile-and-run)

  (defun my-projectile-cabal-project-p (&optional dir)
    (projectile-verify-file-wildcard "?*.cabal" dir))

  (advice-add 'projectile-cabal-project-p
              :override #'my-projectile-cabal-project-p)

  (projectile-update-project-type 'haskell-cabal :precedence 'high)
  (projectile-update-project-type 'haskell-stack :precedence 'low)
  (projectile-update-project-type 'nix :precedence 'low)
  (projectile-update-project-type 'nix-flake :precedence 'low)
  )

(use-package scratch
  :defer t
  :init
  (spc-leader-define-key
   "bs" #'scratch))

(use-package helpful
  :defer t
  :init
  (global-set-key (kbd "C-h f") #'helpful-callable)
  (global-set-key (kbd "C-h v") #'helpful-variable)
  (global-set-key (kbd "C-h k") #'helpful-key)
  (global-set-key (kbd "C-h c") #'helpful-command)
  :config
  (evil-define-key 'normal helpful-mode-map "q" 'quit-window))

;; (use-package casual-calc
;;   :bind (:map
;;          calc-mode-map
;;          ("C-o" . casual-calc-tmenu)
;;          :map
;;          calc-alg-map
;;          ("C-o" . casual-calc-tmenu))
;;   :after (calc))

(use-package editorconfig
  :config
  (editorconfig-mode 1))

;; LSP, Haskell

(setq-default c-default-style "k&r"
              c-basic-offset 4)

(use-package lsp-mode
  :hook
  ((haskell-mode . lsp)
   (c-mode . lsp)
   (c++-mode . lsp)
   (lsp-mode . lsp-enable-which-key-integration))
  :commands lsp
  :init
  (setq-default lsp-modeline-diagnostics-enable nil
                lsp-progress-function 'ignore
                lsp-lens-enable nil
                lsp-ui-doc-enable nil)
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
    "lh"  #'lsp-describe-thing-at-point
    ;; backend
    "lb" '(:ignore t :which-key "backend")
    "lbd" #'lsp-describe-session
    "lbr" #'lsp-workspace-restart
    "lbs" #'lsp-workspace-shutdown
    "lbv" #'lsp-version
    "lbx" #'lsp-workspace-folders-remove
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
    ;; text/code
    "lx" '(:ignore t :which-key "text/code")
    "lxh" #'lsp-document-highlight
    "lxl" #'lsp-lens-show
    "lxL" #'lsp-lens-hide))
(use-package lsp-ui
  :defer t
  :init
  (setq lsp-ui-sideline-actions-icon nil
        lsp-ui-sideline-show-diagnostics nil
        lsp-ui-peek-always-show t))

(use-package haskell-mode
  :mode (("\\.hs\\'" . haskell-mode))
  :init
  ;; fix indent
  (setq-default haskell-indentation-layout-offset 4
                haskell-indentation-left-offset 4
                haskell-indentation-starter-offset 4
                haskell-indentation-where-post-offset 4
                haskell-indentation-where-pre-offset 4)
  (defun haskell-hoogle-lookup-from-local-wrapper ()
    (interactive)
    (unless (haskell-hoogle-server-live-p) (haskell-hoogle-start-server))
    (haskell-hoogle-lookup-from-local))
  (mode-leader-define-key haskell-mode-map
    "hh" #'hoogle
    "hH" #'haskell-hoogle-lookup-from-local-wrapper)

  (setq auto-mode-alist (delete '("\\.hsc\\'" . haskell-mode) auto-mode-alist)))

(use-package lsp-haskell
  :defer
  :init
  ;; (setq-default lsp-haskell-server-path "/home/bradrn/.ghcup/bin/haskell-language-server-wrapper")
  (setq-default lsp-haskell-plugin-stan-global-on nil)
  )
(use-package company-cabal
  :after (company lsp-haskell)
  :config
  (add-to-list 'company-backends 'company-cabal))

;; (use-package hasky-extensions
;;   :defer t
;;   :init
;;   (mode-leader-define-key haskell-mode-map
;;    "xx" #'hasky-extensions
;;    "xd" #'hasky-extensions-browse-docs)
;;   :config
;;   (add-to-list 'hasky-extensions "OverloadedLabels" t))

(use-package shakespeare-mode
  :defer
  :init
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
  :config
  (mode-leader-define-key shakespeare-mode-map
    "cy" #'haskell-run-yesod-devel
    "x"  #'switch-between-hamlet-julius))


;; Rust

(use-package rustic
  :defer
  :init
  (setq rustic-format-on-save nil)
  :config
  (mode-leader-define-key rustic-mode-map
    "b" #'rustic-cargo-build
    "f" #'rustic-cargo-fmt
    "r" #'rustic-cargo-run))

;; Python

(use-package python
  :defer
  :init
  (defun python-start-or-switch-repl ()
    (interactive)
    (pop-to-buffer (process-buffer
                    (or (python-shell-get-process)
                        (run-python)))))

  (setq python-shell-interpreter "ipython"
        python-shell-interpreter-args "-i")

  (defun python-run-current-file ()
    (interactive)
    ;; set universal argument to put compilation into comint mode
    (let ((universal-argument t))
      (compile (concat "python "
                       (shell-quote-argument (file-name-nondirectory buffer-file-name)))
               t)))
  :config
  (mode-leader-define-key python-mode-map
    "'" #'python-start-or-switch-repl
    "r" #'python-run-current-file
    "va" #'pyvenv-activate
    "vd" #'pyvenv-deactivate
    "vw" #'pyvenv-workon))

(use-package pyvenv :defer)


;; HTML (Emmet)

(use-package emmet-mode
  :defer t
  :init
  (add-hook 'sgml-mode-hook 'emmet-mode) ;; Auto-start on any markup modes
  (add-hook 'css-mode-hook 'emmet-mode) ;; enable Emmet's css abbreviation.
  :config
  (define-key emmet-mode-keymap (kbd "TAB") #'emmet-expand-line))


;; LaTeX

(use-package pdf-tools
  :init
  (pdf-tools-install)
  (pdf-loader-install)

  (setq-default pdf-view-display-size 'fit-page)

  (defun my--pdf-sync-goto-current-page ()
    (interactive)
    (let ((size (pdf-view-image-size)))
      (pdf-sync-backward-search
       (/ (car size) 2)
       (/ (cdr size) 2))))
  (mode-leader-define-key pdf-sync-minor-mode-map
    "K" #'my--pdf-sync-goto-current-page)

  ;; (custom-theme-set-variables
  ;;  'deeper-blue
  ;;  '(pdf-view-midnight-colors (cons "gray80" "#181a26")))

  :config
  ;; swap keys
  (evil-define-key 'normal pdf-view-mode-map "f" #'pdf-links-action-perform)
  (evil-define-key 'normal pdf-view-mode-map "F" #'pdf-links-isearch-link)
  (add-hook 'pdf-view-mode-hook (lambda () (display-line-numbers-mode 0)))
  ;; (add-hook 'pdf-view-mode-hook (lambda () (setq-local cursor-type nil)))

  ;; annotations
  (evil-define-key 'normal pdf-view-mode-map "L" #'pdf-annot-list-annotations)
  (evil-define-key 'normal pdf-annot-list-mode-map "K" #'pdf-annot-list-display-annotation-from-id)
  (add-hook 'pdf-annot-list-mode-hook (lambda () (pdf-annot-list-follow-minor-mode t))))

(put 'latex-mode 'flyspell-mode-predicate 'my-tex-mode-flyspell-verify)
(defun my-tex-mode-flyspell-verify ()
  (and
   (not (save-excursion
          (re-search-backward "^[ \t]*%%%[ \t]+Local" nil t)))
   (not (save-excursion
          (let ((this (point))
                (eol (line-end-position))
                (has-match nil)
                (re "\\\\\\(\\(auto\\|text\\)?cite\\*?\\|label\\|ref\\|abbr\\|input\\)\\(\\[[^\\]*]\\]\\)*{[^}]*}\\|\\\\\\(\\(auto\\)?cites\\*?\\)\\({[^}]*}\\)+"))
            (beginning-of-line)
            (while (re-search-forward re eol t)
              (setq has-match
                    (or has-match
                        (and (>= this (match-beginning 0))
                             (<= this (match-end 0))))))
            has-match)))))

(use-package tex
  :ensure auctex
  :mode ("\\.tex\\'" . LaTeX-mode)

  :preface
  ;; from spacemacs
  (defun LaTeX-build ()
    (interactive)
    (progn
      (let ((TeX-save-query nil))
        (TeX-save-document (TeX-master-file)))
      (TeX-command "LaTeX" 'TeX-master-file -1)))

  (defun TeX-word-count ()
    (interactive)
    (let ((temp-buffer (concat "*wordcount: " (buffer-name) "*")))
      (with-output-to-temp-buffer temp-buffer
        (call-process
         "texcount"
         nil temp-buffer nil
         (buffer-file-name))
        (pop-to-buffer temp-buffer))))

  :init
  (setq TeX-command-default "LaTeX"
        TeX-auto-save t
        TeX-parse-self t
        TeX-syntactic-comment t
        ;; Synctex support
        TeX-source-correlate-start-server nil
        ;; Don't insert line-break at inline math
        LaTeX-fill-break-at-separators nil
        ;; preview-gs-command "c:/Program Files/gs/gs9.53.3/bin/gswin64c.exe"
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
    "K"   #'TeX-view
    "h"   #'TeX-doc
    "l"   #'TeX-recenter-output-buffer
    "e"   #'LaTeX-environment
    "c"   #'LaTeX-close-environment
    "i"   #'LaTeX-insert-item
    "s"   #'LaTeX-section
    "x"   #'TeX-kill-job
    "q"   #'TeX-next-error
    "\\"  #'TeX-electric-macro
    "w"   #'TeX-word-count

    "p"   '(:ignore t :which-key "preview")
    "pb"  #'preview-buffer
    "pp"  #'preview-at-point
    "pr"  #'preview-region
    "pc"  '(:ignore t :which-key "clearout")
    "pcb" #'preview-clearout-buffer
    "pcp" #'preview-clearout-at-point

    "r"   '(:ignore t :which-key "reftex")
    "ri"  #'reftex-toc
    "rc"  #'reftex-citation
    "rl"  #'reftex-label
    "rr"  #'reftex-reference
    "rv"  #'reftex-view-crossref
    "rx"  #'reftex-reset-mode)

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
  (evil-define-key '(insert visual) TeX-mode-map (kbd "C-j") #'latex/font-italic) ; not C-i as that = TAB and should perform indentation!
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

  (if (eq system-type 'windows-nt)
      (add-to-list 'TeX-tree-roots "c:/Users/bradn/AppData/Roaming/MiKTeX/2.9/"))

  ;; from https://tex.stackexchange.com/questions/286028/inverse-search-with-emacs-auctex-and-sumatrapdf-on-windows-10
  (setq TeX-PDF-mode t)
  (setq TeX-source-correlate-mode t)
  (setq TeX-source-correlate-method 'synctex)
  (setq TeX-view-program-list
        '(("Sumatra PDF" ("\"C:/Users/bradn/AppData/Local/SumatraPDF/SumatraPDF.exe\" -reuse-instance" (mode-io-correlate " -forward-search \"%b\" %n ") " %o"))
          ("Okular" "okular --unique %o#src:%n%b")
          ("Zathura"
           ("zathura "
            (mode-io-correlate " --synctex-forward %n:0:%b -x \"emacsclient +%{line} %{input}\" ")
            " %o")
           "zathura")
          ("PDF Tools" TeX-pdf-tools-sync-view)))
  ;; (add-to-list 'TeX-expand-list '("%(cntxcom)" (lambda () "c:/context/bin/context1.bat")))

  (assq-delete-all 'output-pdf TeX-view-program-selection)
  (if (eq system-type 'windows-nt)
      (add-to-list 'TeX-view-program-selection '(output-pdf "Sumatra PDF"))
    (require 'pdf-sync)  ; so that TeX-view works first time
    (add-to-list 'TeX-view-program-selection '(output-pdf "PDF Tools"))
    (add-hook 'TeX-after-compilation-finished-functions
              #'TeX-revert-document-buffer)))

;; org

(use-package org
  :defer
  :init
  (spc-leader-define-key
    "o" '(:ignore t :which-key "org-agenda")
    "oa" #'org-agenda
    "ob" #'org-switchb
    "oc" #'org-cycle-agenda-files
    "os" #'org-save-all-org-buffers)
  :config

  (defun toggle-org-hide-stars ()
    (interactive)
    (setq-local org-hide-leading-stars (not org-hide-leading-stars))
    ;; restart font-lock-mode
    (font-lock-mode nil)
    (font-lock-mode t))
  (mode-leader-define-key org-mode-map
    "ci" #'org-clock-in
    "co" #'org-clock-out
    "e" #'org-export-dispatch
    "i" #'org-insert-item
    "t" #'org-todo
    "w" #'org-table-toggle-column-width
    "W" #'org-refile
    "K" #'org-ctrl-c-ctrl-c
    "z" #'org-reveal
    "s" '(:ignore t :which-key "scheduling")
    "ss" #'org-schedule
    "sd" #'org-deadline
    "s." #'org-time-stamp
    "g" #'org-set-tags-command
    "*" #'toggle-org-hide-stars
    "," #'org-insert-structure-template)

  (use-package evil-org
    :init
    (require 'evil)
    :config
    (add-hook 'org-mode-hook 'evil-org-mode)
    (add-hook 'evil-org-mode-hook
              (lambda ()
                (evil-org-set-key-theme)))
    (require 'evil-org-agenda))

  (use-package zotxt
    :config
    (add-hook 'org-mode-hook 'org-zotxt-mode)
    (defun single-line--org-zotxt-insert-reference-links-to-items (items)
      "Insert links to Zotero ITEMS in buffer."
      (mapc (lambda (item)
              (org-zotxt-insert-reference-link-to-item item))
            ;; (insert "\n")
            ;; (forward-line 1))
            items))
    (advice-add 'org-zotxt-insert-reference-links-to-items
                :override #'single-line--org-zotxt-insert-reference-links-to-items)

    (setq zotxt-default-bibliography-style "mkbehr-short"
          org-zotxt-link-description-style :citekey
          zotxt-default-search-method :title-creator-year)
    (mode-leader-define-key org-mode-map
      "rt" #'org-zotxt-mode
      "rc" #'org-zotxt-insert-reference-link
      "ru" #'org-zotxt-update-reference-link-at-point
      "ro" #'org-zotxt-open-attachment))
  
  ;; adapted from https://emacs.stackexchange.com/a/14734/20375
  (defun org-agenda-skip-if-blocked ()
    (let ((next-headline (save-excursion (or (outline-next-heading) (point-max)))))
      (if (org-entry-blocked-p) next-headline)))
  (setq org-agenda-files '("~/org")
        ;; meaning:
        ;; TODO, NEXT, INPROGRESS, DONE are self-explanatory
        ;; WAITING is for tasks which are waiting for some issue to be resolved
        ;; HOLD is for tasks which are waiting indefinitely
        ;; CANCELLED is for tasks for which there is no intention of completion
        org-todo-keywords
        '((sequence "TODO(t)" "NEXT(n)" "INPROGRESS(p)" "|" "DONE(d)")
          (sequence "WAITING(w@)" "HOLD(h@)" "|" "CANCELLED(x@)"))
        org-agenda-custom-commands
        '(("p" "Block agenda"
           ((tags "-university/INPROGRESS"
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
           ((tags "+university/NEXT"
                  ((org-agenda-overriding-header "Next tasks")))
            (tags "+university+lecture/!"
                  ((org-agenda-overriding-header "Unwatched lectures")))
            (tags-todo "+university-lecture+DEADLINE<\"<+2w>\"/!"
                       ((org-agenda-overriding-header "Deadlines")
                        (org-agenda-sorting-strategy '(deadline-up))))
            (tags-todo "+university+assignment+SCHEDULED=\"\"+DEADLINE<\"<+3w>\"|+university+assignment+deadline=\"\"|+university+assignment+SCHEDULED<=\"<now>\"/!TODO|NEXT|INPROGRESS|WAITING"
                       ((org-agenda-overriding-header "Assignments")
                        (org-agenda-sorting-strategy '(deadline-up))))
            (agenda ""
                    ((org-agenda-span 14)
                     (org-deadline-warning-days 0)
                     (org-agenda-skip-function '(org-agenda-skip-entry-if 'todo '("HOLD")))
                     (org-agenda-before-sorting-filter-function
                      (lambda (line)
                        (unless (and (string-match "Sched\\." line)
                                     (equal "INPROGRESS" (get-text-property 0 'todo-state line)))
                          line)))))
            (tags-todo "+university/INPROGRESS"
                       ((org-agenda-overriding-header "In progress"))))))
        org-agenda-show-outline-path t
        org-agenda-breadcrumbs-separator "→"
        org-outline-path-complete-in-steps nil ; for helm, see https://blog.aaronbieber.com/2017/03/19/organizing-notes-with-refile.html
        org-archive-mark-done nil
        org-archive-location "%s_archive::* Archived Tasks"
        org-enforce-todo-dependencies t
        org-track-ordered-property-with-tag nil
        org-agenda-dim-blocked-tasks t
        org-enforce-todo-checkbox-dependencies t
        org-log-into-drawer t
        org-startup-folded t
        org-adapt-indentation t)

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


;; Markdown

(use-package markdown-mode
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init
  (setq markdown-gfm-use-electric-backquote nil)
  (add-hook 'markdown-mode-hook
            #'(lambda () (setq show-trailing-whitespace nil)))

  :config
  (evil-define-key '(insert visual) markdown-mode-map (kbd "C-b") #'markdown-insert-bold)
  (evil-define-key '(insert visual) markdown-mode-map (kbd "C-t") #'markdown-insert-code)
  (evil-define-key '(insert visual) markdown-mode-map (kbd "C-j") #'markdown-insert-italic) ; not C-i as that = TAB and should perform indentation!
  )

;; LISP

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

(use-package lispy
  :defer t
  :init
  (add-hook 'emacs-lisp-mode-hook (lambda () (lispy-mode 1)))
  (add-hook 'lisp-mode-hook       (lambda () (lispy-mode 1)))
  (add-hook 'lispy-mode-hook #'lispyville-mode)
  :config
  (setq lispy-close-quotes-at-end-p t))
(use-package lispyville
  :defer t
  :init
  (let ((pos (memq 'evil-mode-line-tag mode-line-format)))
    (setcdr pos (cons
                 '(:eval (when (featurep 'lispyville)
                           (lispyville-mode-line-string "l-special" "")))
                 (cdr pos))))
  :config
  (lispyville-set-key-theme
   '(operators
     c-w
     text-objects
     ;; atom-movement
     additional
     additional-movement
     commentary
     slurp/barf-cp
     wrap
     additional
     additional-insert
     escape)))

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
    "ha" #'sly-apropos
    "hd" #'hyperspec-lookup
    "hs" #'sly-describe-symbol))


;; Forth

(use-package gforth
  :mode (("\\.fs\\'" . forth-mode)
         ("\\.fb\\'" . forth-block-mode))
  :load-path "c:/Program Files (x86)/gforth")


;; Coq
;; based on https://github.com/tchajed/spacemacs-coq/blob/785bf0d5e702df2bd6b5c63935c1b8bf8d279b18/packages.el

(use-package proof-general
  :mode ("\\.v\\'" . coq-mode)
  :defer t
  :init
  (setq proof-splash-enable nil
        proof-electric-terminator-enable t
        proof-next-command-insert-space nil
        coq-one-command-per-line nil)
  (mode-leader-define-key coq-mode-map
    ;; Basic proof management
    "]" 'proof-assert-next-command-interactive
    "[" 'proof-undo-last-successful-command
    "." 'proof-goto-point
    "K" 'proof-goto-point
    ;; Layout
    "ll" 'proof-layout-windows
    "lc" 'pg-response-clear-displays
    "lp" 'proof-prf
    ;; Prover Interaction
    "px" 'proof-shell-exit
    "pc" 'proof-interrupt-process
    "pr" 'proof-retract-buffer
    "pb" 'proof-process-buffer
    ;; Prover queries ('ask prover')
    "af" 'proof-find-theorems
    "ap" 'coq-Print
    "ac" 'coq-Check
    "ab" 'coq-About
    "as" 'coq-Search
    "aip" 'coq-Print-with-implicits
    "aic" 'coq-Check-show-implicits
    "aib" 'coq-About-with-implicits
    "anp" 'coq-Print-with-all
    "anc" 'coq-Check-show-all
    "anb" 'coq-About-with-all
    ;; Moving the point (goto)
    "g." 'proof-goto-end-of-locked
    "ga" 'proof-goto-command-start
    "ge" 'proof-goto-command-end
    ;; Insertions
    "ie" 'coq-end-Section
    ;; Company-coq
    "il" 'company-coq-lemma-from-goal
    "im" 'company-coq-insert-match-construct))
(use-package company-coq
  :defer t
  :init
  (add-hook 'coq-mode-hook #'company-coq-mode)
  (setq company-coq-disabled-features
        '(spinner prettify-symbols))
  :config
  ;; redo syntactic font-lock to parse block comments properly
  (defun company-coq-syntactic-face-function/nospace (state)
    "Determine which face to use based on parsing state STATE."
    (let ((comment-opener-pos (nth 8 state)))
      (cl-flet ((looking-at-quoted
                 (pattern)
                 (looking-at-p (regexp-quote pattern))))
        (when comment-opener-pos
          (save-excursion
            (goto-char comment-opener-pos)
            (cond
             ((looking-at-quoted "(*!") 'company-coq-comment-h3-face)
             ((looking-at-quoted "(*+") 'company-coq-comment-h2-face)
             ((looking-at-quoted "(*** ") 'company-coq-comment-h1-face)
             ((looking-at-quoted "(**") 'font-lock-doc-face) ; diff!
             ))))))
  (company-coq-do-in-coq-buffers
    (remove-function (local 'font-lock-syntactic-face-function)
                     #'company-coq-syntactic-face-function)
    (add-function :before-until (local 'font-lock-syntactic-face-function)
                  #'company-coq-syntactic-face-function/nospace)))


;; reset garbage collection

(setq gc-cons-threshold 16777216
      gc-cons-percentage 0.1
      file-name-handler-alist last-file-name-handler-alist)

;; custom

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(LaTeX-math-abbrev-prefix "M-n")
 '(custom-safe-themes
   '("4c56af497ddf0e30f65a7232a8ee21b3d62a8c332c6b268c81e9ea99b11da0d3" "4eb6fa2ee436e943b168a0cd8eab11afc0752aebb5d974bba2b2ddc8910fca8f" "bffa9739ce0752a37d9b1eee78fc00ba159748f50dc328af4be661484848e476" "fee7287586b17efbfda432f05539b58e86e059e78006ce9237b8732fde991b4c" default))
 '(package-selected-packages
   '(jupyter pyvenv company-coq proof-general sly lispyville lispy zotxt evil-org auctex emmet-mode rustic shakespeare-mode yasnippet-snippets winum which-key use-package undo-tree try spacemacs-theme solarized-theme scratch rainbow-delimiters projectile origami move-text magit lsp-ui lsp-haskell ivy-yasnippet idle-highlight-mode hl-todo highlight-indent-guides helpful gruvbox-theme general flycheck fill-column-indicator evil-surround evil-snipe evil-numbers evil-nerd-commenter evil-matchit evil-lion evil-exchange evil-escape evil-collection editorconfig doom-themes counsel company-cabal avy))
 '(safe-local-variable-values
   '((TeX-engine . xelatex)
     (backup-inhibited . t)
     (backup-by-copying . t))))
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

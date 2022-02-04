(setq straight-repository-branch "develop")
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; Setup use-package

(straight-use-package 'use-package)

;; Performance tweaks
(setq gc-cons-threshold 100000000)
(setq read-process-output-max (* 1024 1024))
(global-so-long-mode 1)

;; Modal editing
(use-package boon :straight t
  :bind (("C-x f" . find-file)
         ("C-x s" . save-buffer)
         ("C-x ;" . comment-line)
         ("C-x C-s" . save-some-buffers)
         ("C-x e" . pp-eval-last-sexp)
         ("C-x C-e" . kmacro-end-and-call-macro)
         ("M-g s h p" . highlight-phrase)
         ("M-g s h r" . highlight-regexp)
         ("M-g s h l" . highlight-lines-mathcing-regexp)
         ("M-g s h u" . unhighlight-regexp)
         ("M-g M-s" . speedbar)
         ("M-g M-s" . speedbar)
         (:map boon-command-map
               ("^" . delete-indentation)
               ("p" . consult-line)
               ("&" . async-shell-command)
               ("%" . query-replace)))
  :init
  (require 'boon-colemak)
  (boon-mode 1)
  :config
  (let ((modes '(speedbar-mode sly-db-mode sly-inspector-mode)))
    (mapc #'(lambda (mode) (add-to-list 'boon-special-mode-list mode)) modes)))

(define-key global-map (kbd "C-z") nil)

(use-package multiple-cursors :straight t
  :config
  (bind-keys :map boon-command-map
             :prefix "b"
             :prefix-map my/mc-map
             ("n" . mc/mark-next-lines)
             ("i n" . mc/insert-numbers)
             ("i l" . mc/insert-letters)
             ("a" . mc/vertical-align)
             ("N" . mc/mark-previous-lines)
             ("b" . mc/mark-all-dwim)))

;; Swap to a bunch of more useful keybindings than the defaults
(use-package emacs
 :bind (("M-\\" . cycle-spacing)
        ("M-u" . upcase-dwim)
        ("M-l" . downcase-dwim)
        ("M-c" . capitalize-dwim)))
 
;; Platform specifics
(when (memq window-system '(mac ns x))
  (progn
    (use-package exec-path-from-shell :straight t
      :config
      (exec-path-from-shell-initialize))
    (use-package ns-auto-titlebar :straight t
      :config
      (ns-auto-titlebar-mode))
    (setq-default mac-option-modifier 'meta)
    (setq delete-by-moving-to-trash 'system-move-file-to-trash)
    (setq ns-right-option-modifier nil
	  mac-right-option-modifier nil
          mac-command-modifier 'super)))


;; some options that make Emacs a less intrusive OS citizen
(setq frame-resize-pixelwise t)
(use-package no-littering :straight t)

;; store all backup and autosave files in the tmp dir
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))
(setq create-lockfiles nil) ;; Allow other processes (such as Storybook) to access files at the same time as Emacs

(setq ring-bell-function 'ignore)
(defalias 'yes-or-no-p 'y-or-n-p)
(recentf-mode +1)
(delete-selection-mode +1)


;; Documentation enhancements
(use-package which-key :straight t
  :diminish which-key-mode
  :config (which-key-mode +1))

(use-package eldoc :ensure nil
  :diminish eldoc-mode
  :config
  (eldoc-mode +1))

(use-package helpful :straight t
  :bind (("C-h f" . helpful-callable))
   ("C-h v" . helpful-variable)
   ("C-h k" . helpful-key))


;; Navigation
(use-package rotate :straight t
 :bind ("C-x 4 r" . rotate-layout))

(use-package ace-window :straight t
  :custom
  (aw-scope 'frame)
  :bind (("M-o" . ace-window)
         ("C-x o" . ace-window)
         ("C-x 5 o" . my/ace-window-global))
  :config
  (defun my/ace-window-global (arg)
    (interactive "P")
    (setf aw-scope 'global)
    (ace-window arg)
    (setf aw-scope 'frame)))

(winner-mode +1)


;; Pulse current line when jumping somewhere
(defun my/pulse-line (&rest _)
 "Pulse the current line"
 (pulse-momentary-highlight-one-line (point)))

(dolist (command '(recenter-top-bottom other-window ace-window boon-switch-mark xref-pop-marker-stack))
 (advice-add command :after #'my/pulse-line))

;; File management
(use-package dired
  :hook (dired-mode . dired-hide-details-mode))

(use-package diredfl
  :straight t
  :config
  (diredfl-global-mode 1))

(use-package dired-git-info :straight t
 :bind (:map dired-mode-map
             (")" . dired-git-info-mode)))

(use-package rgrep
  :bind ("M-g s g" . rgrep))

(use-package view
 :ensure nil
 :config
 (setq view-read-only t))

(use-package speedbar
  :ensure nil
  :config
  (speedbar-add-supported-extension ".ts")
  (speedbar-add-supported-extension ".tsx"))

(use-package popper :straight t
  :bind (("C-`" . popper-toggle-latest)
         ("M-`" . popper-cycle)
         ("C-M-`" . popper-toggle-type))
  :init
  (setq popper-reference-buffers
        '("\\*Messages*\\*"
          "Output\\*$"
          "\\*Async Shell Command\\*"
          help-mode
          compilation-mode))
  (popper-mode +1)
  (popper-echo-mode +1))

;; Completion/selection

(use-package consult
  :straight t
  ;; Replace bindings. Lazily loaded due by `use-package'.
  :bind (;; C-c bindings (mode-specific-map)
         ("C-c h" . consult-history)
         ("C-c r" . consult-recent-file)
         ("C-c m" . consult-mode-command)
         ("C-c b" . consult-bookmark)
         ("C-c k" . consult-kmacro)
         ;; C-x bindings (ctl-x-map)
         ("C-x M-:" . consult-complex-command) ;; orig. repeat-complex-command
         ("C-x b" . consult-buffer) ;; orig. switch-to-buffer
         ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
         ("C-x 5 b" . consult-buffer-other-frame) ;; orig. switch-to-buffer-other-frame
         ;; Custom M-#' bindings for fast register access
         ("M-#" . consult-register-load)
         ("M-'" . consult-register-store) ;; orig. abbrev-prefix-mark (unrelated)
         ("C-M-#" . consult-register)
         ;; Other custom bindings
         ("M-y" . consult-yank-pop)     ;; orig. yank-pop
         ("<help> a" . consult-apropos) ;; orig. apropos-command
         ;; M-g bindings (goto-map)
         ("M-g e" . consult-compile-error)
         ("M-g o" . consult-outline)
         ("M-g m" . consult-mark)
         ("M-g k" . consult-global-mark)
         ("M-g i" . consult-imenu)
         ([remap imenu] . consult-imenu)
         ("M-g I" . consult-project-imenu)
         ;; M-s bindings (search-map)
         ("M-g s f" . consult-find)
         ("M-g s L" . consult-locate)
         ("M-g s G" . consult-git-grep)
         ("M-g s r" . consult-ripgrep)
         ("M-g s l" . consult-line)
         ("M-g s m" . consult-multi-occur)
         ("M-g s k" . consult-keep-lines)
         ("M-g s U" . consult-focus-lines)
           ;; Isearch integration
         ("M-s e" . consult-isearch)
         (:map isearch-mode-map
               ("M-e" . consult-isearch) ;; orig. isearch-edit-string
               ("M-s e" . consult-isearch) ;; orig. isearch-edit-string
               ("M-s l" . consult-line))) ;; required by consult-line to detect isearch
  :init
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)
  :config
  (setq consult-narrow-key "<") ;; (kbd "C-+")
  (setq consult-project-root-function
        (lambda ()
          (when-let (project (project-current))
            (car (project-roots project))))))

(use-package marginalia :straight t
  :init (marginalia-mode))

(use-package all-the-icons-completion :straight t
  :init (all-the-icons-completion-mode)
  :hook (marginalia-mode . all-the-icons-completion-marginalia-setup))

(use-package embark :straight t
  :demand t
  :init
  (defun embark-devdocs-lookup (ident)
    "Lookup identifier using devdocs."
    (interactive "DevDocs: ")
    (devdocs-lookup nil ident))
  (setq prefix-help-command #'embark-prefix-help-command)
  :bind (("C-." . embark-act)
         (:map embark-identifier-map
               ("D" . embark-devdocs-lookup))
         (:map embark-variable-map
               ("D" . embark-devdocs-lookup))))

(use-package embark-consult :straight t
 :after (embark consult)
 :demand t
 :hook
 (embark-collect-mode . consult-preview-at-point-mode))

(use-package savehist
  :init
  (savehist-mode))

(use-package vertico :straight (vertico :files (:defaults "extensions/*")
					:includes (vertico-buffer
						   vertico-directory
                                                   vertico-quick
                                                   vertico-unobtrusive))
  :init
  (vertico-mode)
  (vertico-mouse-mode)
  (vertico-multiform-mode)
  (setq vertico-multiform-commands
	'((consult-line unobtrusive)))
  (setq vertico-multiform-categories
	'((file grid)
          (consult-grep buffer)
          (imenu buffer))))

(use-package vertico-directory
  :after vertico
  :straight nil
  :ensure nil
  :bind (:map vertico-map
	      ("RET" . vertico-directory-enter)
              ("DEL" . vertico-directory-delete-char)
              ("M-DEL" . vertico-directory-delete-word))
  :hook (rfn-eshadow-update-overlay . vertico-directory-tidy))

(use-package vertico-quick
  :after vertico
  :straight nil
  :ensure nil
  :bind (:map vertico-map
	      ("C-'" . vertico-quick-exit)))

(use-package orderless :straight t
 :init
  (setq completion-styles '(orderless)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))))

(use-package emacs
 :init
 (setq minibuffer-prompt-properties
   '(read-only t cursor-intangible t face minibuffer-prompt))
 (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

  ;; Enable recursive minibuffers
 (setq enable-recursive-minibuffers t)
 (setq tab-always-indent 'complete))

(use-package corfu :straight t
  :custom
  (corfu-cycle t)
  (corfu-preselect-first nil)
  :bind
  (:map corfu-map
  	("TAB" . corfu-next)
        ("S-TAB" . corfu-previous)
        ([tab] . corfu-next)
        ([backtab] . corfu-previous))
    :init
    (corfu-global-mode))

(use-package kind-icon :straight t
  :custom
  (kind-icon-default-face 'corfu-default)
  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

(use-package cape :straight t
  :bind (("C-c p p" . completion-at-point)
	 ("C-c p t" . complete-tag)
	 ("C-c p d" . cape-dabbrev)
         ("C-c p f" . cape-file)
         ("C-c p k" . cape-keyword)
         ("C-c p s" . cape-symbol)
         ("C-c p a" . cape-abbrev)
         ("C-c p i" . cape-ispell)
         ("C-c p l" . cape-line)
         ("C-c p w" . cape-dict)
         ("C-c p \\" . cape-tex)
         ("C-c p _" . cape-tex)
         ("C-c p ^" . cape-tex)
         ("C-c p &" . cape-sgml)
         ("C-c p r" . cape-rfc1345))
  :init
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-tex)
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-keyword))

;; Jump to links/clickable items on screen
(use-package link-hint
  :straight t
  :init
  (cl-loop
   for (mode map) in '((minibuffer minibuffer-local-completion-map)
                       (embark embark-collect-mode-map)
                       (help help-mode-map)
                       (info Info-mode-map)
                       (apropos apropos-mode-map)
                       (vertico vertico-map)
                       (man Man-mode-map)
                       (woman woman-mode-map)
                       (package package-menu-mode-map)
                       (eww eww-mode-map)
                       (dired dired-mode-map))
   do (eval-after-load mode `(define-key ,map "'" #'link-hint-open-link))))


;; Delimiter editing/structured editing

(use-package paredit :straight t
  :hook ((emacs-lisp-mode lisp-mode) . paredit-mode))
(use-package electric-pair
  :hook ((js-mode python-mode text-mode typescript-mode typescript-tsx-mode) . electric-pair-local-mode))
(show-paren-mode 1)
(use-package rainbow-delimiters :straight t
  :hook (prog-mode . rainbow-delimiters-mode))

;; LSP support
(use-package eglot :straight t
  :bind ((:map eglot-mode-map
	       ("C-c r" . eglot-rename)
               ("C-c f" . eglot-format)
               ("C-c a" . eglot-code-actions)
               ("C-c h" . eldoc)))
  :hook ((js-mode javascript-jsx-mode typescript-mode python-mode typescript-tsx-mode c-mode zig-mode) . eglot-ensure)
  :config
  (let ((servers '((typescript-tsx-mode . ("typescript-language-server" "--stdio")))))
    (dolist (server servers)
      (add-to-list 'eglot-server-programs server))))

;; Other programming niceties
(use-package devdocs :straight t
  :hook (typescript-mode . (lambda ()
                             (setq-local devdocs-current-docs '("typescript"))))
        (js-mode . (lambda ()
                    (setq-local devdocs-current-docs '("javascript")))))


(use-package tempel :straight t
  :init
  (defun tempel-setup-capf ()
    (add-hook 'completion-at-point-functions #'tempel-expand -1 'local))
  (add-hook 'prog-mode-hook 'tempel-setup-capf)
  (add-hook 'text-mode-hook 'tempel-setup-capf))

(use-package project :demand nil
 :bind (:map project-prefix-map
        ("t" . project-vterm)
        ("m" . magit-status))
 :config
 (defun project-vterm ()
   "Start VTerm in the current project's root directory.
If a buffer already exists for running Vterm in the project's root,
switch to it.  Otherwise, create a new Vterm buffer.
With \\[universal-argument] prefix arg, create a new Vterm buffer even
if one already exists."
   (interactive)
   (defvar vterm-buffer-name)
   (let* ((default-directory (project-root (project-current t)))
          (vterm-buffer-name (project-prefixed-buffer-name "vterm"))
          (vterm-buffer (get-buffer vterm-buffer-name)))
     (if (and vterm-buffer (not current-prefix-arg))
         (pop-to-buffer-same-window vterm-buffer)
       (vterm t))))
 
 (nconc project-switch-commands '((magit-status "Magit") (project-vterm "Vterm"))))


;; Language specifics
(bind-key "C-c C-c" 'eval-defun)

(use-package rjsx-mode :straight t)
(use-package json-mode :straight t)

(use-package javascript-mode :ensure nil
  :bind ((:map js-mode-map
                  ("C-c d" . devdocs-lookup))))

(use-package typescript-mode :straight t
 :mode (rx ".ts" string-end)
 :bind ((:map typescript-mode-map
                 ("C-c d" . devdocs-lookup)))
 :custom
 (typescript-indent-level 4)
 :init (define-derived-mode typescript-tsx-mode typescript-mode "typescript-tsx")
       (add-to-list 'auto-mode-alist (cons (rx ".tsx" string-end) #'typescript-tsx-mode)))

(use-package tree-sitter-langs :straight t)
(use-package tree-sitter :straight t
 :hook (typescript-mode . tree-sitter-hl-mode)
 :config
 (setf (alist-get 'typescript-tsx-mode tree-sitter-major-mode-language-alist) 'tsx))

(use-package prettier :straight t
  :hook (after-init . global-prettier-mode))

(use-package pyvenv :straight t)

(use-package rustic :straight t
  :init
  (setq rustic-lsp-client 'eglot))

(use-package haskell-mode :straight t
  :bind ((:map haskell-mode-map
               ("<f8>" . haskell-navigate-imports)
               ("C-c C-," . haskell-mode-format-imports)
               ("C-c C-." . (lambda ()  (interactive) (progn
                                                        (haskell-sort-imports)
                                                        (haskell-align-imports))))
               ("C-c C-S-c" . haskell-compile)
               ("C-c C-l" . haskell-process-or-load)
               ("C-`" . haskell-interactive-bring)
               ("C-c C-t" . haskell-process-do-type)
               ("C-c C-i" . haskell-process-do-info)
               ("C-c C-c" . haskell-process-cabal-build)
               ("C-c C-k" . haskell-process-interactive-mode-clear)
               ("C-c c" . haskell-process-cabal)
               ("M-." . haskell-mode-jump-to-def-or-tag)
               ("C-c ?" . hoogle))
         (:map haskell-cabal-mode-map
               ("C-`" . haskell-interactive-bring)
               ("C-c C-c" . haskell-process-cabal-build)
               ("C-c C-k" . haskell-process-interactive-mode-clear)
               ("C-c c" . haskell-process-cabal)
               ("C-c C-c" . haskell-compile)))
  :hook ((haskell-mode . electric-pair-local-mode)
         (haskell-mode . interactive-haskell-mode)
         (haskell-mode . haskell-auto-insert-module-template)
         (haskell-mode . haskell-decl-scan-mode))
  :init
  (require 'haskell-interactive-mode)
  (require 'haskell-process)
  :config
  (setq haskell-process-suggest-remove-import-lines t)
  (setq haskell-process-auto-import-loaded-modules t)
  (setq haskell-process-log t)
  (cons "-fno-ghci-sandbox" haskell-process-args-cabal-repl))

(use-package zig-mode :straight t
  :config (setq zig-format-on-save nil)
  :hook ((zig-mode . electric-pair-local-mode)))

(use-package q-mode :straight t)

;; Terminal
(use-package vterm :straight t)

;; Git
(use-package magit :straight t
  :bind ("C-x g" . magit-status))

(use-package forge :straight t
  :after magit)

(use-package git-timemachine :straight t)

(use-package ibuffer-vc :straight t
  :bind ("C-x C-b" . ibuffer)
  :hook (ibuffer-mode . (lambda ()
                          (ibuffer-vc-set-filter-groups-by-vc-root)
                          (unless (eq ibuffer-sorting-mode 'alphabetic)
                            (ibuffer-do-sort-by-alphabetic)))))

;; Searching
(use-package anzu :straight t
  :diminish anzu-mode
  :bind ([remap query-replace] . anzu-query-replace)
  :config
  (global-anzu-mode +1)
  ;; Mood-line displays anzu output, so no need for anzu to display it as well
  (setq anzu-cons-mode-line-p nil))

(use-package expand-region :straight t
  :bind ("C-=" . er/expand-region))

(use-package undo-tree :straight t
  :config
  (global-undo-tree-mode +1))

(use-package avy
  :bind (("M-g M-g" . avy-goto-line)
         ("M-g g" . avy-goto-line)
         (:map isearch-mode-map
               ("C-'" . avy-isearch)))
  :config
  (defun my/avy-action-embark (pt)
   (unwind-protect
    (save-excursion
     (goto-char pt)
     (embark-act))
    (select-window
     (cdr (ring-ref avy-ring 0))))
   t)
  (setf (alist-get ?. avy-dispatch-alist) 'my/avy-action-embark))

(setq inferior-lisp-program "ros -Q run")
(use-package sly :straight t
 :commands (sly)
 :hook ((sly-mrepl . (lambda () (sly-symbol-completion-mode -1))))
 :bind ((:map lisp-mode-map
           ([remap pp-eval-last-sexp] . sly-eval-last-expression))))
 
(use-package sly-quicklisp :straight t)
(use-package sly-asdf :straight t)

(use-package cider :straight t)

;; Looks
(use-package nano-modeline :straight t
  :config
  (nano-modeline-mode))

(load "~/.emacs.d/themes/ceres-theme.el")
(load-theme 'ceres t)

(use-package solaire-mode :straight t
 :config
 (solaire-global-mode +1))

(set-face-attribute 'default nil :font "Fira Code" :height 120)
(set-face-attribute 'variable-pitch nil :font "Baskerville" :height 140)
(set-face-attribute 'fixed-pitch nil :font "Fira Code" :height 120)
(global-hl-line-mode +1)

(menu-bar-mode -1)
(tool-bar-mode -1)
(when (display-graphic-p)
  (scroll-bar-mode -1))
(setq inhibit-startup-message t)

(use-package all-the-icons-dired :straight t
  :hook (dired-mode . all-the-icons-dired-mode))

(use-package all-the-icons-ibuffer :straight t
 :hook (ibuffer-mode . all-the-icons-ibuffer-mode))

;; Jupyter
(use-package jupyter :straight t
  :demand t)

;; Markdown
(use-package markdown-mode :straight t
  :hook (markdown-mode . variable-pitch-mode))

;; Org mode
(straight-use-package '(org-contrib :includes org))
(setq org-babel-python-command "/usr/local/bin/python3")
(org-babel-do-load-languages
   'org-babel-load-languages
   '((sqlite . t)
     (python . t)
     (emacs-lisp . t)
     (jupyter . t)
     (shell . t)))
(setq org-babel-default-header-args:jupyter-python '((:async . "yes")
                                                     (:session . "ipy")
                                                     (:kernel . "python3")))
(setq org-src-fontify-natively t
      org-fontify-quote-and-verse-blocks t)
(put 'narrow-to-region 'disabled nil)

(add-hook 'org-mode-hook 'variable-pitch-mode)

(use-package org-bullets :straight t
 :custom
 (org-hide-leading-stars t)
 (org-bullets-bullet-list '("◉" "○"))
 :hook (org-mode . org-bullets-mode))

;; Custom packages
(use-package tracer
  :load-path "site-lisp/"
  :hook (prog-mode . tracer-mode))

(use-package observable-dataflow-mode
  :straight (:host github :repo "syvsto/observable-dataflow-mode"))

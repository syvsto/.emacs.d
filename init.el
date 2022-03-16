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

(add-hook 'prog-mode-hook #'display-line-numbers-mode)

(use-package objed :straight t
  :config
  (setq objed-modeline-hint nil)
  (objed-mode 1))
(use-package multiple-cursors :straight t)

;; Swap to a bunch of more useful keybindings than the defaults
(use-package emacs
 :bind (("M-\\" . cycle-spacing)
        ("M-u" . upcase-dwim)
        ("M-l" . downcase-dwim)
        ("M-c" . capitalize-dwim))
 :config
 (unless (version<= emacs-version "28.0")
   (repeat-mode 1)
   (defvar winner-repeat-map (make-sparse-keymap) "A map for repeating `winner-mode' keys.")
   (define-key winner-repeat-map (kbd "<left>") #'winner-undo)
   (define-key winner-repeat-map (kbd "<right>") #'winner-redo)
   (put 'winner-undo 'repeat-map 'winner-repeat-map)))
 
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
(setq view-read-only t)

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

(setq sentence-end-double-space nil)


;; Navigation
(use-package rotate :straight t
  :bind ("C-x 4 r" . rotate-layout)
  :config
  (defvar rotate-repeat-map (make-sparse-keymap) "A map for repeating `rotate-layout' keys.")
  (define-key rotate-repeat-map "r" #'rotate-layout)
  (put 'rotate-layout 'repeat-map 'rotate-repeat-map))

(use-package ace-window :straight t
  :custom
  (aw-scope 'frame)
  :bind (("C-x o" . ace-window)
	 ("M-o" . ace-window)))

(winner-mode +1)


;; Pulse current line when jumping somewhere
(defun my/pulse-line (&rest _)
 "Pulse the current line"
 (pulse-momentary-highlight-one-line (point)))

(dolist (command '(recenter-top-bottom other-window ace-window other-frame xref-pop-marker-stack))
 (advice-add command :after #'my/pulse-line))

;; File management
(use-package diredfl
  :straight t
  :config
  (diredfl-global-mode 1))

(use-package all-the-icons-dired :straight t
  :config
  (all-the-icons-dired-mode 1))

(use-package dirvish :straight t
  :bind (("C-x C-d" . dirvish)
	 (:map dired-mode-map
	       ("M-s" . dirvish-setup-menu)
	       ("SPC" . dirvish-show-history)
	       ("r" . dirvish-roam)
	       ("M-a" . dirvish-mark-actions-menu)
	       ("M-s" . dirvish-setup-menu)
	       ("M-f" . dirvish-toggle-fullscreen)))
  :custom
  (dirvish-attributes '(file-size all-the-icons))
  :init
  (dirvish-override-dired-mode))

(use-package rgrep
  :bind ("M-s g" . rgrep))

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
         ("M-g E" . consult-compile-error)
         ("M-g o" . consult-outline)
         ("M-g m" . consult-mark)
         ("M-g k" . consult-global-mark)
         ("M-g i" . consult-imenu)
	 ("M-g e" . consult-flymake)
         ([remap imenu] . consult-imenu)
         ("M-g I" . consult-project-imenu)
         ;; M-s bindings (search-map)
         ("M-s f" . consult-find)
         ("M-s L" . consult-locate)
         ("M-s G" . consult-git-grep)
         ("M-s r" . consult-ripgrep)
         ("M-s l" . consult-line)
         ("M-s m" . consult-multi-occur)
         ("M-s k" . consult-keep-lines)
         ("M-s U" . consult-focus-lines)
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
            (car (project-roots project)))))
  (setq completion-in-region-function
      (lambda (&rest args)
        (apply (if vertico-mode
                   #'consult-completion-in-region
                 #'completion--in-region)
               args))))

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
  :bind (("M-i" . embark-act)
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
	'((consult-flymake buffer)))
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
	      ("M-j" . vertico-quick-exit)))

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
 (setq enable-recursive-minibuffers t ))

(use-package company :straight t
  :custom
  (company-frontends '(company-preview-frontend))
  (company-minimum-prefix-length 4)
  :bind ((:map company-active-map
	       ("M-n" . consult-company)
	       ("C-n" . my/objed-next-line)
	       ("C-p" . my/objed-previous-line)
	       ("<tab>" . company-complete-selection)))
  :hook (prog-mode . company-mode)
  :config
  (defun my/objed-previous-line ()
    (interactive)
    (company-abort)
    (objed-activate)
    (objed-previous-line))
  (defun my/objed-next-line ()
    (interactive)
    (company-abort)
    (objed-activate)
    (objed-next-line)))

(use-package consult-company :straight t)

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

;; Delimiter editing/structured editing

(use-package electric-pair
  :hook (prog-mode . electric-pair-local-mode))
(show-paren-mode 1)
(use-package rainbow-delimiters :straight t
  :hook (prog-mode . rainbow-delimiters-mode))

;; LSP support
(use-package lsp-mode :straight t
  :hook (((python-mode zig-mode typescript-mode typescript-tsx-mode javascript-mode javascript-jsx-mode csharp-tree-sitter-mode c-mode) . lsp)
	 (lsp-mode . lsp-enable-which-key-integration))
  :custom
  (lsp-keymap-prefix "M-p")
  (lsp-enable-symbol-highlighting nil)
  (lsp-headerline-breadcrumb-enable nil))
(use-package lsp-ui :straight t
  :bind ((:map lsp-mode-map
	       ([remap xref-find-definitions] . lsp-ui-peek-find-definitions)
	       ([remap xref-find-references] . lsp-ui-peek-find-references))))
(use-package consult-lsp :straight t
  :bind ((:map lsp-mode-map
	       ([remap xref-find-apropos] . consult-lsp-symbols)))
  :config
  (consult-lsp-marginalia-mode 1))
  

(use-package flymake
  :ensure nil
  :bind  ((:map flymake-mode-map
		("M-g s" . flymake-goto-next-error)
		("M-g r" . flymake-goto-prev-error)))
  :config
  (defvar flymake-repeat-map (make-sparse-keymap) "A map for repeating `flymake' keys.")
  (define-key flymake-repeat-map "s" #'flymake-goto-next-error)
  (define-key flymake-repeat-map "r" #'flymake-goto-prev-error)
  (put 'flymake-goto-next-error 'repeat-map 'flymake-repeat-map)
  (put 'flymake-goto-prev-error 'repeat-map 'flymake-repeat-map))

(use-package devdocs :straight t
  :hook (typescript-mode . (lambda ()
                             (setq-local devdocs-current-docs '("typescript"))))
        (js-mode . (lambda ()
                    (setq-local devdocs-current-docs '("javascript")))))


(use-package yasnippet :straight t
  :config
  (yas-global-mode 1))

(use-package yasnippet-snippets :straight t)

(use-package project :demand nil
 :bind ((:map project-prefix-map
        ("t" . project-vterm)
        ("m" . magit-status)))
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

 (unless (version<= emacs-version "28.0")
   (nconc project-switch-commands '((magit-status "Magit") (project-vterm "Vterm")))))


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

(use-package csharp-mode :straight t
  :config
  (add-to-list 'auto-mode-alist '("\\.cs\\'" . csharp-tree-sitter-mode)))

(use-package tree-sitter-langs :straight t)
(use-package tree-sitter-indent :straight t)
(use-package tree-sitter :straight t
 :hook (typescript-mode . tree-sitter-hl-mode)
 :config
 (setf (alist-get 'typescript-tsx-mode tree-sitter-major-mode-language-alist) 'tsx))

(use-package prettier :straight t
  :hook (after-init . global-prettier-mode))

(use-package pyvenv :straight t)

(use-package rustic :straight t)

(use-package wgrep :straight t)

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

(use-package code-review :straight t
  :after forge
  :bind ((:map forge-topic-mode-map
	       ("C-c r" . code-review-forge-pr-at-point))
	 (:map code-review-mode-map
	       ("C-c C-n" . code-review-comment-jump-next)
	       ("C-c C-p" . code-review-comment-jump-previous))))

(use-package diff-hl :straight t
  :config
  (global-diff-hl-mode))

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
               ("M-j" . avy-isearch)))
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
  (nano-modeline-mode)
  )

(use-package tab-bar-echo-area :straight t
  :custom
  (tab-bar-show nil)
  :config
  (tab-bar-echo-area-mode 1))

;; (load "~/.emacs.d/themes/ceres-theme.el")
;; (load-theme 'ceres t)

(use-package modus-themes :straight t
  :config
  (modus-themes-load-operandi))

(use-package solaire-mode :straight t
 :config
 (solaire-global-mode +1))

(set-face-attribute 'default nil :font "Fira Code" :height 120)
(set-face-attribute 'variable-pitch nil :font "Baskerville" :height 160)
(set-face-attribute 'fixed-pitch nil :font "Fira Code" :height 120)
(global-hl-line-mode +1)

(menu-bar-mode -1)
(tool-bar-mode -1)
(when (display-graphic-p)
  (scroll-bar-mode -1))
(setq inhibit-startup-message t)

(use-package all-the-icons-ibuffer :straight t
  :hook (ibuffer-mode . all-the-icons-ibuffer-mode))

(use-package olivetti :straight t
  :custom
  (olivetti-body-width 140))

;; Jupyter
(use-package jupyter :straight t
  :demand t)

;; Markdown
(use-package markdown-mode :straight t
  :hook (markdown-mode . variable-pitch-mode))

;; Org mode

(define-key global-map (kbd "C-z") nil)

(straight-use-package '(org-contrib :includes org))
(setq org-use-speed-commands t)
(setq org-babel-python-command "/usr/local/bin/python3")
(setq org-load-done t)
(setq org-hide-emphasis-markers t)
(setq org-agenda-files (list "~/notes.org" "~/jobb/calendar.org" "~/jobb/time.org"))
(setq org-default-notes-file "~/notes.org")
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

(use-package org-modern :straight t
  :hook ((org-mode . org-modern-mode)
	 (org-agenda-finalize . org-modern-agenda))
  :config
  (setq
 ;; Edit settings
 org-auto-align-tags nil
 org-tags-column 0
 org-catch-invisible-edits 'show-and-error
 org-special-ctrl-a/e t
 org-insert-heading-respect-content t

 ;; Org styling, hide markup etc.
 org-hide-emphasis-markers t
 org-pretty-entities t
 org-ellipsis "…"

 ;; Agenda styling
 org-agenda-block-separator ?─
 org-agenda-time-grid
 '((daily today require-timed)
   (800 1000 1200 1400 1600 1800 2000)
   " ┄┄┄┄┄ " "┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄")
 org-agenda-current-time-string "⭠ now ─────────────────────────────────────────────────"))

(use-package org-roam :straight t
  ;; FIXME: Bind all org map bindings here, so I don't override previous binding. This can probably be split up, just need to read bind-keys docs.
  :bind ((:prefix "C-z" :prefix-map my/writing-map
		  ("o" . olivetti-mode)
		  ("a c" . org-capture)
		  ("a a" . org-agenda)
		  ("a l" . org-store-link)
		  ("r i" . org-roam-node-insert)
		  ("r f" . org-roam-node-find)
		  ("r c" . org-roam-capture)))
  :custom
  (org-roam-directory "~/org-roam")
  :config
  (org-roam-db-autosync-mode))

(use-package org-notifications :straight t
  :init
  (setq org-notifications-non-agenda-file '("~/jobb/calendar.org"))
  (setq org-notifications-which-agenda-files 'non-agenda-file)
  (setq org-notifications-notify-before-time 300)
  (org-notifications-start))

(use-package osm :straight t)

(defun my/ddg-search-webkit (keyword)
  (interactive "sSearch for: ")
  (let* ((keywords (s-split-words keyword))
	 (search-string (s-join "+" keywords)))
    (xwidget-webkit-browse-url (concat "https://duckduckgo.com/?q=" search-string))))

(bind-key "C-z s" 'my/ddg-search-webkit)

;; Custom packages
(use-package tracer
  :load-path "site-lisp/"
  :hook (prog-mode . tracer-mode))

(use-package observable-dataflow-mode
  :straight (:host github :repo "syvsto/observable-dataflow-mode"))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("118b4d40a523a61b4991c520fc078b9ca70eca6774fc1a8dc87e6a4fa778b637" "b45e8c816fec55f8aa03621b730a238e7610a7bf580ed9a6e4e120882e9bff2a" "72eefdab51d2b0c22327504a7c819aa19a6ed10b18889758249881b2954cdbe8" "d2457dde677c0049fbacfc7deb56c7ff5fdb1405dec78b7f14b3fedf3f53a37b" "a25c368c59f1e6d66fdf061dcc8c557763913f6b4d49576cc72173ac67298bf8" "10a57e03b8203adb7f66392e5179a52977f22cf618bb9ec823f2d9268eaa417b" "12317f37c057efed7f222ec0dfa2dceaa862707ddde769d8130e50c36103d9b6" "6842b68bbeb33c1912ddcb5cd0734d441b56d7d0e9882c49adc44899a3fa9976" "fdbf380d3b067f33fc023df2cbc7e591d92f6b33eddc26aaa59235f0ad2f54e9" "2371ba6224eaba0a6828f31f95393155bb865f6afde5a34b0172ce6a4c2ad07c" "a2d68805b09fc9678fb7132927aff5742c7e1dc55291e87eef98471b587e7014" "b14adf7023a50b56de758d1577662f736df77611515b62cb7af7b70e6a7dac40" "b5d7d25c3b79b28dbcb2596b57a537def847cc18221ed90030aa96d3a0d205a9" "38a5bc13e376a0bd3758eee380d094ffe6ba567f17ff980c6a9835f05ab24a1b" default))
 '(warning-suppress-log-types '((comp))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(nano-modeline-active-name ((t (:inherit (mode-line bold) :box nil))))
 '(nano-modeline-active-primary ((t (:inherit mode-line :foreground "#184034" :box nil))))
 '(nano-modeline-active-secondary ((t (:inherit mode-line :foreground "#184034" :box nil))))
 '(nano-modeline-active-status-** ((t (:inherit mode-line :background "#e4c340" :box nil))))
 '(nano-modeline-active-status-RO ((t (:inherit mode-line :background "#f2b0a2" :box nil))))
 '(nano-modeline-active-status-RW ((t (:inherit mode-line :background "#c0efff" :box nil))))
 '(nano-modeline-inactive ((t (:inherit mode-line-inactive :box nil))))
 '(nano-modeline-inactive-name ((t (:inherit mode-line-inactive :box nil))))
 '(nano-modeline-inactive-primary ((t (:inherit mode-line-inactive :foreground "#404148" :box nil))))
 '(nano-modeline-inactive-secondary ((t (:inherit mode-line-inactive :foreground "#404148" :box nil))))
 '(nano-modeline-inactive-status-** ((t (:inherit mode-line-inactive :foreground "#702f00" :box nil))))
 '(nano-modeline-inactive-status-RO ((t (:inherit mode-line-inactive :foreground "#8a0000" :box nil))))
 '(nano-modeline-inactive-status-RW ((t (:inherit mode-line-inactive :foreground "#003f8a" :box nil)))))

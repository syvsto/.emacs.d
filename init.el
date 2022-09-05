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
(global-auto-revert-mode 1)

;; Performance tweaks
(setq gc-cons-threshold 100000000)
(setq read-process-output-max (* 1024 1024))
(global-so-long-mode 1)
(global-subword-mode 1)

;; Swap to a bunch of more useful keybindings than the defaults
(use-package emacs
 :bind (("M-\\" . cycle-spacing)
        ("M-u" . upcase-dwim)
        ("M-l" . downcase-dwim)
        ("M-c" . capitalize-dwim))
 :config
 (setq completion-cycle-threshold 3))

 (setq tab-always-indent 'complete)
 (unless (version<= emacs-version "28.0")
   (repeat-mode 1)
   (defvar winner-repeat-map (make-sparse-keymap) "A map for repeating `winner-mode' keys.")
   (define-key winner-repeat-map (kbd "<left>") #'winner-undo)
   (define-key winner-repeat-map (kbd "<right>") #'winner-redo)
   (put 'winner-undo 'repeat-map 'winner-repeat-map)))
 
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

;; Documentation enhancements
(use-package which-key :straight t
  :config (which-key-mode +1))

(use-package eldoc :ensure nil
  :config
  (eldoc-mode +1))

(use-package helpful :straight t
  :bind (("C-h f" . helpful-callable))
   ("C-h v" . helpful-variable)
   ("C-h k" . helpful-key))

(use-package boon
  :straight t
  :init
  (require 'boon-colemak)
  (boon-mode 1)
  :bind (("C-x f" . find-file)
	     ("C-x s" . save-buffer)
	     ("C-x C-s" . save-some-buffers)
         ("C-x e" . eval-last-sexp)
 (:map boon-command-map
	   ("p" . consult-line)
       ("%" . anzu-query-replace-regexp)
       ("&" . async-shell-command))
       ("^" . delete-indentation)))
(use-package expand-region :straight t)

(setq sentence-end-double-space nil)

(add-hook 'doc-view-mode-hook #'(lambda () (internal-show-cursor nil nil)))

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
  :bind (("C-x C-o" . ace-window)
	 ("C-x o" . delete-blank-lines)
	 ("M-o" . ace-window)))

(winner-mode +1)

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

;; Pulse current line when jumping somewhere
(defun my/pulse-line (&rest _)
 "Pulse the current line"
 (pulse-momentary-highlight-one-line (point)))

(dolist (command '(recenter-top-bottom other-window ace-window other-frame xref-pop-marker-stack))
 (advice-add command :after #'my/pulse-line))

;; File management
(setq dired-dwim-target t)

(use-package dired :ensure nil
  :hook (dired-mode . dired-hide-details-mode))

(use-package all-the-icons :straight t)

(use-package all-the-icons-dired :straight t
  :hook (dired-mode . all-the-icons-dired-mode))

(use-package dired-hacks-utils :straight t
  :bind ((:map dired-mode-map
	       ([remap dired-next-line] . dired-hacks-next-file)
               ([remap dired-pervious-line] . dired-hacks-previous-file)))
  :hook (dired-mode . dired-utils-format-information-line-mode))

(use-package dired-filter :straight t)
(use-package dired-open :straight t)
(use-package dired-subtree :straight t
  :custom (dired-subtree-use-backgrounds nil)
  :bind (:map dired-mode-map
	      ("TAB" . dired-subtree-toggle)))
(use-package dired-collapse :straight t
  :hook (dired-mode . dired-collapse-mode))
              
(use-package rgrep
  :bind ("M-s g" . rgrep))

(use-package wgrep :straight t)

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

;; Searching
(use-package anzu :straight t
  :bind ([remap query-replace] . anzu-query-replace)
  :config
  (global-anzu-mode +1))

;; Completion/selection

(use-package all-the-icons-completion :straight t
  :config
  (all-the-icons-completion-mode)
  (add-hook 'marginalia-mode-hook #'all-the-icons-completion-marginalia-setup))

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
         ("M-g	I" . consult-project-imenu)
	 ("M-i" . consult-imenu)
         ;; M-s bindings (search-map)
         ("M-s f" . consult-find)
         ("M-s L" . consult-locate)
         ("M-s g" . consult-git-grep)
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
            (car (project-roots project))))))

(use-package deadgrep :straight t
  :bind ("M-s r" . deadgrep))

(bind-key "C-c d" 'flymake-show-buffer-diagnostics prog-mode-map)
(bind-key "C-c D" 'flymake-show-project-diagnostics prog-mode-map)

(use-package marginalia :straight t
  :init (marginalia-mode))

(use-package embark :straight t
  :demand t
  :bind ("C-." . embark-act)
  :init
  (setq prefix-help-command #'embark-prefix-help-command))

(use-package embark-consult :straight t
 :after (embark consult)
 :demand t
 :hook
 (embark-collect-mode . consult-preview-at-point-mode))

(use-package savehist
  :init
  (savehist-mode))

(use-package orderless :straight t
  :commands (orderless-filter))

(use-package fussy
  :straight t
  :config
  (push 'fussy completion-styles)
  (setq
   ;; For example, project-find-file uses 'project-files which uses
   ;; substring completion by default. Set to nil to make sure it's using
   ;; flx.
   completion-category-defaults nil
   completion-category-overrides nil
   fussy-filter-fn 'fussy-filter-orderless))

(use-package vertico :straight t
  :config
  (vertico-mode 1))

(use-package company :straight t
  :config
  (setq company-minimum-prefix-length 1)
  (setq company-idle-delay 0)
  (defun bb-company-capf (f &rest args)
  "Manage `completion-styles'."
  (if (length< company-prefix 2)
      (let ((completion-styles (remq 'fussy completion-styles)))
        (apply f args))
    (apply f args)))

(defun bb-company-transformers (f &rest args)
  "Manage `company-transformers'."
  (if (length< company-prefix 2)
      (apply f args)
    (let ((company-transformers '(fussy-company-sort-by-completion-score)))
      (apply f args))))

(advice-add 'company--transform-candidates :around 'bb-company-transformers)
(advice-add 'company-capf :around 'bb-company-capf)
(global-company-mode 1))

(use-package company-posframe :straight t
  :config
  (setq company-tooltip-minimum-width 40)
  (company-posframe-mode 1))

(use-package emacs
 :init
 (setq minibuffer-prompt-properties
   '(read-only t cursor-intangible t face minibuffer-prompt))
 (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)
 (add-hook 'minibuffer-setup-hook #'(lambda () (setq-local truncate-lines t)))

;; Enable recursive minibuffers
(setq enable-recursive-minibuffers t ))

(bind-key [remap dabbrev-expand] 'hippie-expand)

;; Delimiters
(electric-pair-mode 1)
(use-package rainbow-delimiters :straight t
  :hook (prog-mode . rainbow-delimiters-mode))

;; LSP support
(use-package lsp-mode :straight t
  :hook ((typescript-tsx-mode typescript-mode javascript-mode javascript-jsx-mode d-mode zig-mode python-mode) . lsp)
  :custom
  (lsp-headerline-breadcrumb-enable nil)
  (lsp-keymap-prefix "C-c l"))
  
(use-package yasnippet :straight t
  :config
  (yas-global-mode 1))

(use-package yasnippet-snippets :straight t
  :init
  (yasnippet-snippets-initialize))

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

(use-package json-mode :straight t)

(use-package typescript-mode :straight t
 :mode (rx ".ts" string-end)
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

(use-package rustic :straight t)

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

(setq inferior-lisp-program "ros -Q run")
(use-package sly :straight t
 :commands (sly)
 :hook ((sly-mrepl . (lambda () (sly-symbol-completion-mode -1))))
 :bind ((:map lisp-mode-map
           ([remap pp-eval-last-sexp] . sly-eval-last-expression))))
 
(use-package sly-quicklisp :straight t)
(use-package sly-asdf :straight t)

(use-package restclient :straight t)
(use-package ob-restclient :straight t)

;; Terminal
(use-package vterm :straight t
  :config
  (setq vterm-max-scrollback 100000))

;; Git
(use-package magit :straight t
  :bind ("C-x g" . magit-status))

(use-package forge :straight t
  :after magit
  :custom
  (forge-owned-accounts '(("syvsto"))))

(use-package diff-hl :straight t
  :config
  (global-diff-hl-mode))

(use-package ibuffer-vc :straight t
  :bind ("C-x C-b" . ibuffer)
  :hook (ibuffer-mode . (lambda ()
			  (ibuffer-vc-set-filter-groups-by-vc-root)
			  (unless (eq ibuffer-sorting-mode 'alphabetic)
			    (ibuffer-do-sort-by-alphabetic)))))

(use-package all-the-icons-ibuffer :straight t
  :hook (ibuffer-mode . all-the-icons-ibuffer-mode))

(use-package vundo :straight t
  :bind ("C-x u" . vundo)
  :custom (vundo-glyph-alist vundo-unicode-symbols))
(bind-key "C-?" 'undo-redo)

(global-hl-line-mode 1)

(menu-bar-mode -1)
(tool-bar-mode -1)
(when (display-graphic-p)
  (scroll-bar-mode -1))
(setq inhibit-startup-message t)

;; Markdown
(use-package markdown-mode :straight t
  :mode ("\\.mdx" . markdown-mdx-mode)
  :config
  (define-derived-mode markdown-mdx-mode markdown-mode "markdown-mdx")
  (add-hook 'markdown-mdx-mode-hook #'(lambda () (prettier-mode 0)))
  (defun my/format-mdx-on-save ()
    (interactive)
    (save-buffer)
    (shell-command (concat "prettier --write --parser mdx \"" buffer-file-name "\"")))
  (bind-key "C-c f" #'my/format-mdx-on-save markdown-mdx-mode-map))

;; Centering content
(use-package olivetti :straight t
  :custom (olivetti-body-width 160)
  :bind ("C-z" . olivetti-mode))

;; Org mode
(straight-use-package '(org-contrib :includes org))
(setq org-use-speed-commands t)
(setq org-babel-python-command "/usr/local/bin/python3")
(setq org-hide-emphasis-markers t)
(org-babel-do-load-languages
   'org-babel-load-languages
   '((sqlite . t)
     (python . t)
     (emacs-lisp . t)
     (restclient . t)
     (shell . t)))
(setq org-babel-default-header-args:jupyter-python '((:async . "yes")
                                                     (:session . "ipy")
                                                     (:kernel . "python3")))
(setq org-src-fontify-natively t
      org-fontify-quote-and-verse-blocks t)
(put 'narrow-to-region 'disabled nil)

(add-hook 'org-mode-hook #'writer-mode)
(add-hook 'writer-mode-hook #'org-num-mode)

(use-package async :straight t)

;; Looks
(use-package nano :straight (:type git :host github :repo "rougier/nano-emacs"))
(use-package mini-frame :straight t)
(use-package nano-faces :after nano)
(use-package nano-theme :after nano)
(use-package nano-base-colors :after nano)
(use-package nano-colors :after nano)
(use-package nano-modeline :after nano)
(use-package svg-tag-mode :straight t
  :config
  (setq svg-tag-tags
      '(("\\(:#[A-Za-z0-9]+\\)" . ((lambda (tag)
                                     (svg-tag-make tag :beg 2))))
        ("\\(:#[A-Za-z0-9]+:\\)$" . ((lambda (tag)
                                       (svg-tag-make tag :beg 2 :end -1))))))
  (global-svg-tag-mode 1))

;; Custom packages
(use-package tracer
  :load-path "site-lisp/"
  :hook (prog-mode . tracer-mode))

(use-package observable-dataflow-mode
  :straight (:host github :repo "syvsto/observable-dataflow-mode"))


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
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("02f30097c5840767499ed2f76d978545f6fb3a1c3ba59274576f7e23fc39d30b" "8f85f336a6d3ed5907435ea196addbbbdb172a8d67c6f7dbcdfa71cd2e8d811a" "b69d8a1a142cde4bbde2f940ac59d2148e987cd235d13d6c4f412934978da8ab" default))
 '(warning-suppress-log-types '((comp))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

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
   (put 'winner-undo 'repeat-map 'winner-repeat-map))
 
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
  :bind ("C-h ." . eldoc)
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
               ;; (";" . boon-toggle-mark)
               ;; ("'" . boon-end-of-line)
               ;; ("v" . boon-replace-by-character)
               ;; ("d" . boon-set-insert-like-state)
               ;; ("h" . boon-qsearch-previous-at-point)
               ;; ("m" . avy-goto-word-1)
       ("p" . consult-line) ;
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
(use-package all-the-icons :straight t)

(use-package dirvish :straight t
  :init
  (dirvish-override-dired-mode 1)
  :config
  (setq dired-dwim-target t)
  (setq dirvish-use-header-line nil)
  (setq dirvish-use-mode-line 'global)
  (setq dirvish-attributes '(all-the-icons file-time file-size collapse subtree-state vc-state)))
  
(use-package dirvish-extras
  :straight (:host github :repo "alexluigit/dirvish"
             :files ("extensions/dirvish-extras.el")))

(use-package dirvish-vc
  :straight (:host github :repo "alexluigit/dirvish"
             :files ("extensions/dirvish-vc.el")))
             
(use-package rgrep
  :bind ("M-s g" . rgrep))

(use-package grep
  :hook ((grep-mode . toggle-truncate-lines)))

(use-package wgrep :straight t
  :config
  (setq wgrep-auto-save-buffer t)
  (setq wgrep-change-readonly-file t)
  :bind (:map grep-mode-map
	      ("e" . wgrep-change-to-wgrep-mode)
              ("C-x C-q" . wgrep-change-to-wgrep-mode)))

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
         ("M-s r" . consult-ripgrep)
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
  :custom
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles . (partial-completion))))))

(use-package vertico :straight t
  :config
  (vertico-mode 1))

;; (use-package company :straight t
;;   :config
;;   (setq company-minimum-prefix-length 1)
;;   (setq company-idle-delay 0)
;;   (global-company-mode 1))

;; (use-package company-posframe :straight t
;;   :config
;;   (setq company-tooltip-minimum-width 40)
;;   (company-posframe-mode 1))

(use-package corfu :straight t
  :init (global-corfu-mode))

(use-package corfu-popupinfo
  :after corfu
  :config (corfu-popupinfo-mode 1)
  :straight (:host github :repo "emacs-straight/corfu"
             :files ("extensions/corfu-popupinfo.el"))
  :bind (:map corfu-map
         ([remap corfu-info-documentation] . corfu-popupinfo-toggle)))

(use-package corfu-quick
  :after corfu
  :straight (:host github :repo "emacs-straight/corfu"
             :files ("extensions/corfu-quick.el"))
  :bind (:map corfu-map
         ("C-'" . corfu-quick-complete)))

(use-package kind-icon
  :straight t
  :after corfu
  :custom
  (kind-icon-default-face 'corfu-default) ; to compute blended backgrounds correctly
  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

(use-package cape :straight t)

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

;; (use-package lsp-mode :straight t
;;   :disabled
;;   :hook ((typescript-tsx-mode typescript-mode javascript-mode javascript-jsx-mode d-mode zig-mode python-mode) . lsp)
;;   :custom
;;   (lsp-headerline-breadcrumb-enable nil)
;;   (lsp-keymap-prefix "C-c l"))

;; LSP support
(use-package flymake
  :straight t
  :bind (:map prog-mode-map
	      ("C-c d" . flymake-show-buffer-diagnostics)
	      ("C-c D" . flymake-show-project-diagnostics)))

(use-package flymake-diagnostic-at-point
  :straight t
  :after flymake
  :hook (flymake-mode . flymake-diagnostic-at-point-mode)
  :config (setq flymake-diagnostic-at-point-display-diagnostic-function
                'flymake-diagnostic-at-point-display-popup))

(use-package eglot
  :straight t
  :config
  (setq eglot-events-buffer-size 0)
  (setq eglot-extend-to-xref t)
  (setq eglot-put-doc-in-help-buffer nil)
   (eglot--code-action eglot-code-action-organize-imports-ts "source.organizeImports.ts")
   (eglot--code-action eglot-code-action-add-missing-imports-ts "source.addMissingImports.ts")

   (bind-key "C-c C-o C-i" #'eglot-code-action-organize-imports 'eglot-mode-map (not (or (eql major-mode 'typescript-mode) (eql major-mode 'typescript-tsx-mode))))
   (bind-key "C-c C-o C-i" #'eglot-code-action-organize-imports-ts 'eglot-mode-map (or (eql major-mode 'typescript-mode) (eql major-mode 'typescript-tsx-mode)))
   :bind (:map eglot-mode-map
	       ("C-c C-a" . eglot-code-actions)
               ("C-c C-r" . eglot-rename)))
  
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
 :bind (:map typescript-mode-map
             ("C-c C-o C-m" . eglot-code-action-add-missing-imports-ts))
 :init
 (define-derived-mode typescript-tsx-mode typescript-mode "typescript-tsx")
 (add-to-list 'auto-mode-alist (cons (rx ".tsx" string-end) #'typescript-tsx-mode)))

(use-package csharp-mode
  :ensure nil
  :config
  (add-to-list 'auto-mode-alist '("\\.cs\\'" . csharp-tree-sitter-mode)))

(use-package cider :straight t)

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

(use-package git-timemachine :straight t)

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


;; Other filetypes
(add-hook 'doc-view-mode #'(lambda () (setq-local visible-cursor nil)))

;; Looks
(use-package nano-modeline :straight t
  :config
  (nano-modeline-mode 1))

(use-package ceres-theme
  :load-path "themes/"
  :config
  (load-theme 'ceres t))

(set-face-attribute 'default nil :font "Berkeley Mono" :height 130)
(set-face-attribute 'variable-pitch nil :font "Baskerville" :height 160)
(set-face-attribute 'fixed-pitch nil :font "Berkeley Mono" :height 130)
(setq line-spacing 0.1)

(setq default-frame-alist
      (append (list
	       '(min-height . 1)  '(height     . 45)
	       '(min-width  . 1)  '(width      . 101)
               '(vertical-scroll-bars . nil)
               '(internal-border-width . 24) ;; frame padding around the text
               '(ns-transparent-titlebar . t)
               '(menu-bar-lines . 0)
               '(tool-bar-lines . 0))))


(use-package svg-tag-mode :straight t
  :hook ((prog-mode text-mode) . svg-tag-mode)
  :config
  (defun svg-progress-percent (value)
    (svg-image (svg-lib-concat
		(svg-lib-progress-bar (/ (string-to-number value) 100.0)
                                      nil :margin 0 :stroke 2 :radius 3 :padding 2 :width 11)
		(svg-lib-tag (concat value "%")
                             nil :stroke 0 :margin 0)) :ascent 'center))

  (defun svg-progress-count (value)
    (let* ((seq (mapcar #'string-to-number (split-string value "/")))
           (count (float (car seq)))
           (total (float (cadr seq))))
      (svg-image (svg-lib-concat
                  (svg-lib-progress-bar (/ count total) nil
					:margin 0 :stroke 2 :radius 3 :padding 2 :width 11)
                  (svg-lib-tag value nil
                               :stroke 0 :margin 0)) :ascent 'center)))

  (defconst date-re "[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}")
  (defconst time-re "[0-9]\\{2\\}:[0-9]\\{2\\}")
  (defconst day-re "[A-Za-z]\\{3\\}")
  (defconst day-time-re (format "\\(%s\\)? ?\\(%s\\)?" day-re time-re))

  (setq svg-tag-tags
	`(("\\(:#[A-Za-z0-9]+\\)" . ((lambda (tag)
                                       (svg-tag-make tag :beg 2))))
          ("TODO" . ((lambda (tag)
                       (svg-tag-make tag :face 'org-todo :inverse t :margin 0))))
          ("DONE" . ((lambda (tag)
                       (svg-tag-make tag :face 'org-done :margin 0))))
          ("\\(\\[[0-9]\\{1,3\\}%\\]\\)" . ((lambda (tag)
                                              (svg-progress-percent (substring tag 1 -2)))))
          ("\\(\\[[0-9]+/[0-9]+\\]\\)" . ((lambda (tag)
                                            (svg-progress-count (substring tag 1 -1)))))
          ("\\(:#[A-Za-z0-9]+:\\)$" . ((lambda (tag)
					 (svg-tag-make tag :beg 2 :end -1))))
          ("\\[#[A-Z]\\]" . ( (lambda (tag)
				(svg-tag-make tag :face 'org-priority 
                                              :beg 2 :end -1 :margin 0))))
          (,(format "\\(<%s>\\)" date-re) .
           ((lambda (tag)
              (svg-tag-make tag :beg 1 :end -1 :margin 0))))
          (,(format "\\(<%s \\)%s>" date-re day-time-re) .
           ((lambda (tag)
              (svg-tag-make tag :beg 1 :inverse nil :crop-right t :margin 0))))
          (,(format "<%s \\(%s>\\)" date-re day-time-re) .
           ((lambda (tag)
              (svg-tag-make tag :end -1 :inverse t :crop-left t :margin 0))))

          ;; Inactive date  (with or without day name, with or without time)
          (,(format "\\(\\[%s\\]\\)" date-re) .
           ((lambda (tag)
              (svg-tag-make tag :beg 1 :end -1 :margin 0 :face 'org-date))))
          (,(format "\\(\\[%s \\)%s\\]" date-re day-time-re) .
           ((lambda (tag)
              (svg-tag-make tag :beg 1 :inverse nil :crop-right t :margin 0 :face 'org-date))))
          (,(format "\\[%s \\(%s\\]\\)" date-re day-time-re) .
           ((lambda (tag)
              (svg-tag-make tag :end -1 :inverse t :crop-left t :margin 0 :face 'org-date)))))))

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
   '("7ea23ea0b792f1d5a4cd96f5698a70e5fdc4102ba81516327c0db00869b0b38f" "5a43eb67e709aef6279775b7256064b3a31699f1a6567abdf73b15379e2d6559" "198ba2f96082c9e770e4ae7bc9d89b5d2b58b8585171efdd6a90e86bdaea55da" "11f0d723bff6eb2fb450f99435848088675f50787dfb181cdca8e1c6dc07d974" "f581eca21b9fbd2898a171077a99a26cd70bef65f5aa9b4ba17fd293ae947086" "02f30097c5840767499ed2f76d978545f6fb3a1c3ba59274576f7e23fc39d30b" "8f85f336a6d3ed5907435ea196addbbbdb172a8d67c6f7dbcdfa71cd2e8d811a" "b69d8a1a142cde4bbde2f940ac59d2148e987cd235d13d6c4f412934978da8ab" default))
 '(warning-suppress-log-types '((comp))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

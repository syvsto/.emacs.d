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
(use-package diminish :straight t)


;; Platform specifics

(when (memq window-system '(mac ns x))
  (progn
    (use-package exec-path-from-shell :straight t
      :config
      (exec-path-from-shell-initialize))
    (setq-default mac-option-modifier 'meta)
    (setq ns-right-option-modifier nil)))


;; Some options that make Emacs less intrusive

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


;; Modal editing
(use-package boon :straight t
  :defer nil
  :bind (("C-x f" . find-file)
         ("C-x s" . save-buffer)
         ("C-x C-s" . save-some-buffers)
         (:map boon-command-map
          ("p" . pop-to-mark-command)
          ("%" . anzu-query-replace)))
  :config
  (require 'boon-colemak)
  (boon-mode))

(use-package which-key :straight t
  :diminish which-key-mode
  :config (which-key-mode +1))

(use-package eldoc :ensure nil
  :diminish eldoc-mode
  :config
  (eldoc-mode +1))

;; Navigation

(use-package ace-window :straight t
  :bind (("M-o" . ace-window)
         ("C-x o" . ace-window)))

(winner-mode +1)

(use-package dired
 :hook (dired-mode . dired-hide-details-mode)
 :config
 (use-package diredfl
  :straight t
  :config
  (diredfl-global-mode 1)))

(use-package dired-git-info :straight t
 :bind (:map dired-mode-map
        (")" . dired-git-info-mode)))
 
(use-package transpose-frame :straight t
   :bind (("C-x 4 t t" . transpose-frame)
          ("C-x 4 t f" . flip-frame)
          ("C-x 4 t F" . flop-frame)
          ("C-x 4 t r" . rotate-frame)
          ("C-x 4 t <" . rotate-frame-anticlockwise)
          ("C-x 4 t >" . rotate-frame-clockwise)))
          
;; Completion/selection

(use-package corfu :straight t
  :bind (:map corfu-map
              ("TAB" . corfu-next)
              ("<backtab>" . corfu-previous))
  :init
  (corfu-global-mode))
   
(use-package dabbrev
  :demand nil
  :bind (("M-/" . dabbrev-completion)
         ("C-M-/" . dabbrev-expand)))

(setq tab-always-indent 'complete)
(setq completion-cycle-threshold 3)
 
(use-package orderless :straight t
  :init
  (setq completion-styles '(orderless)
        completion-category-defaults nil
        completion-category-overrides '((file (styles . (partial-completion))))))

(use-package savehist
  :demand nil
  :init (savehist-mode))

(use-package consult
  :straight t
  ;; Replace bindings. Lazily loaded due by `use-package'.
  :bind (;; C-c bindings (mode-specific-map)
         ("C-c h" . consult-history)
         ("C-c m" . consult-mode-command)
         ("C-c b" . consult-bookmark)
         ("C-c k" . consult-kmacro)
         ;; C-x bindings (ctl-x-map)
         ("C-x M-:" . consult-complex-command)     ;; orig. repeat-complex-command
         ("C-x b" . consult-buffer)                ;; orig. switch-to-buffer
         ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
         ("C-x 5 b" . consult-buffer-other-frame)  ;; orig. switch-to-buffer-other-frame
         ;; Custom M-#' bindings for fast register access
         ("M-#" . consult-register-load)
         ("M-'" . consult-register-store)          ;; orig. abbrev-prefix-mark (unrelated)
         ("C-M-#" . consult-register)
         ;; Other custom bindings
         ("M-y" . consult-yank-pop)                ;; orig. yank-pop
         ("<help> a" . consult-apropos)            ;; orig. apropos-command
         ;; M-g bindings (goto-map)
         ("M-g e" . consult-compile-error)
         ("M-g o" . consult-outline)
         ("M-g m" . consult-mark)
         ("M-g k" . consult-global-mark)
         ("M-g i" . consult-imenu)
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
         :map isearch-mode-map
         ("M-e" . consult-isearch)                 ;; orig. isearch-edit-string
         ("M-s e" . consult-isearch)               ;; orig. isearch-edit-string
         ("M-s l" . consult-line))                 ;; required by consult-line to detect isearch
  :init
  (setq register-preview-delay 0
        register-preview-function #'consult-register-format)
  (advice-add #'register-preview :override #'consult-register-window)
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

(use-package minibuffer :demand nil
 :custom
 (completion-show-help nil)
 (enable-recursive-minibuffers t)
 (completion-ignore-case t)
 (minibuffer-eldef-shorten-default t)
 (resize-mini-windows t)
 :init
 (minibuffer-depth-indicate-mode)
 (minibuffer-electric-default-mode)
 :bind ((:map minibuffer-local-map
         ("M-RET" . exit-minibuffer))
        (:map minibuffer-local-completion-map
         ("SPC"))
        (:map completion-list-mode-map
         ("C-g" . abort-recursive-edit)
         ("n" . next-line)
         ("p" . previous-line)
         ("F" . consult-focus-lines)
         ("s" . isearch-forward))))

(use-package embark :straight t
 :bind (("C-." . embark-act)
        ("C-;" . embark-dwim)
        ("C-," . embark-collect-live)
        ("C-h B" . embark-bindings))        
 :config
 (add-to-list 'display-buffer-alist
              '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                nil
                (window-parameters (mode-line-format . none))))
 :init
 (setq completion-cycle-threshold 5)
 (setq prefix-help-command #'embark-prefix-help-command))

(use-package embark-consult :straight t
 :after (embark consult)
 :hook
 (embark-collect-mode . consult-preview-at-point-mode))

(use-package avy-embark-collect :straight t
 :bind ((:map embark-collect-mode-map
         ("C-'" . avy-embark-collect-choose))
        (:map minibuffer-mode-map
         ("C-'" . avy-embark-collect-choose))))

(defun my/slime-completion-in-region (_fn completions start end)
  (funcall completion-in-region-function start end completions nil))

(use-package slime :straight (:host github :repo "nuddyco/slime" :branch "clime")
  :defer nil
  :bind ((:map lisp-mode-map
               ("C-c s" . slime-selector)))
  :config
  (advice-add 'slime-display-or-scroll-completions :around #'my/slime-completion-in-region))
(setq inferior-lisp-program "sbcl")
;; (cl-pushnew 'slime-clime slime-contribs)
(cl-pushnew 'slime-quicklisp slime-contribs)
(slime-setup)

(use-package parinfer-rust-mode :straight t
  :hook (((emacs-lisp-mode lisp-mode) . parinfer-rust-mode))
  :config
  (setq-default indent-tabs-mode nil)
  (setq parinfer-rust-auto-download t))

(show-paren-mode +1)
(add-hook 'text-mode-hook #'electric-pair-local-mode)

;; LSP support
(use-package flycheck :straight t)
(use-package lsp-mode :straight t
  :init
  (setq lsp-headerline-breadcrumb-enable nil)
  (setq lsp-keymap-prefix "C-c l")
  :hook ((lsp-mode . lsp-enable-which-key-integration)
         (lsp-mode . electric-pair-local-mode)
         ((rjsx-mode . (lambda ()
                         (require 'lsp-sonarlint)
                         (require 'lsp-sonarlint-javascript)
                         (setq lsp-sonarlint-javascript-enabled t)
                         (lsp)))
          (python-mode . (lambda ()
                          (require 'lsp-sonarlint)
                          (require 'lsp-sonarlint-python)
                          (setq lsp-sonarlint-python-enabled t)
                          (lsp))))))
                          
(use-package lsp-ui :straight t
  :bind (:map lsp-ui-mode-map
         ([remap xref-find-definitions] . lsp-ui-peek-find-definitions)
         ([remap xref-find-references] . lsp-ui-peek-find-references))
  :config
  (setq lsp-ui-sideline-enable nil
        lsp-ui-doc-enable nil))
        
  

(use-package consult-lsp :straight t
  :bind (:map lsp-mode-map
              ([remap xref-find-apropos] . consult-lsp-symbols)))

(use-package lsp-sonarlint :straight t)

(use-package yasnippet :straight t
 :config
 (yas-global-mode 1))

(use-package yasnippet-snippets :straight t) 
;; Language specifics

(use-package rjsx-mode :straight t)

(use-package typescript-mode :straight t
 :mode (rx ".ts" string-end)
 :hook ((typescript-mode typescript-tsx-mode) . lsp)
 :init (define-derived-mode typescript-tsx-mode typescript-mode "typescript-tsx")
       (add-to-list 'auto-mode-alist (cons (rx ".tsx" string-end) #'typescript-tsx-mode)))
(use-package tree-sitter-langs :straight t)
 
(use-package tree-sitter :straight t
 :hook (typescript-mode . tree-sitter-hl-mode)
 :config
 (setf (alist-get 'typescript-tsx-mode tree-sitter-major-mode-language-alist) 'tsx))

(use-package prettier :straight t
  :hook (after-init . global-prettier-mode))

(use-package python-pytest :straight t
  :bind (:map python-mode-map
              ("C-c M-t" . python-pytest-dispatch)))

(use-package pyvenv :straight t)

(use-package rustic :straight t)

(use-package haskell-mode :straight t
  :hook (haskell-mode . electric-pair-local-mode))
(use-package lsp-haskell :straight t
  :hook ((haskell-mode haskell-literate-mode) . lsp))

(use-package zig-mode :straight t
  :config (setq zig-format-on-save nil)
  :hook ((zig-mode . electric-pair-local-mode)
         (zig-mode . lsp)))

;; Terminal

(use-package vterm :straight t)

;; Custom modes etc.

(use-package observable-dataflow-mode
  :straight (:host github :repo "syvsto/observable-dataflow-mode"))


;; Git

(use-package magit :straight t)
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
  :config
  (global-anzu-mode +1))

(use-package expand-region :straight t)

(use-package multiple-cursors :straight t)
  
(use-package undo-tree :straight t
  :config
  (global-undo-tree-mode +1))

(use-package avy
  :bind (("M-g M-g" . avy-goto-line)
         ("M-g g" . avy-goto-line)
         ("M-r" . avy-goto-line)
         (:map isearch-mode-map
               ("C-'" . avy-isearch))))

(use-package iy-go-to-char :straight t)

;; Looks

(use-package modus-themes :straight t
  :init
  (setq modus-themes-org-blocks 'gray-background
        modus-themes-completions 'opinionated)
  (modus-themes-load-themes)
  (modus-themes-load-operandi))

(set-face-attribute 'default nil :font "JetBrains Mono" :height 100)
(global-hl-line-mode +1)

(use-package feebleline :straight t
  :config
  (setq feebleline-msg-functions '((feebleline-line-number         :post "" :fmt "%5s")
                                   (feebleline-column-number       :pre ":" :fmt "%-2s")
                                   (feebleline-file-directory      :face feebleline-dir-face :post "")
                                   (feebleline-file-or-buffer-name :face font-lock-keyword-face :post "")
                                   (feebleline-file-modified-star  :face font-lock-warning-face :post "")
                                   (feebleline-project-name        :align right)))
  (feebleline-mode +1))

(menu-bar-mode -1)
(tool-bar-mode -1)
(when (display-graphic-p)
  (scroll-bar-mode -1))

(use-package all-the-icons-dired :straight t
 :hook (dired-mode . all-the-icons-dired-mode))

;; Jupyter
(use-package jupyter :straight t
  :demand t)

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

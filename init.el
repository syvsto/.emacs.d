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

;; Modal editing
(use-package boon :straight t
  :bind (("C-x f" . find-file)
         ("C-x s" . save-buffer)
         ("C-x ;" . comment-line)
         ("C-x C-s" . save-some-buffers)
         ("C-x e" . pp-eval-last-sexp)
         ("C-x C-e" . kmacro-end-and-call-macro)
         (:map boon-command-map
               ("p" . consult-line)
               ("&" . async-shell-command)
               ("%" . query-replace))
         (:map boon-forward-search-map
               ("r" . ctrlf-forward-alternate))
         (:map boon-backward-search-map
               ("r" . ctrlf-backward-alternate)))
  :init
  (require 'boon-colemak)
  (boon-mode 1))

;; Platform specifics
(when (memq window-system '(mac ns x))
  (progn
    (use-package exec-path-from-shell :straight t
      :config
      (exec-path-from-shell-initialize))
    (setq-default mac-option-modifier 'meta)
    (setq ns-right-option-modifier nil)))


;; Some options that make Emacs less intrusive

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

(use-package which-key :straight t
  :diminish which-key-mode
  :config (which-key-mode +1))

(use-package eldoc :ensure nil
  :diminish eldoc-mode
  :config
  (eldoc-mode +1))

(global-so-long-mode 1)

;; Navigation

(use-package rotate :straight t
 :bind ("C-x 4 r" . rotate-layout))

(use-package crux :straight t
  :bind (("C-x 4 t" . crux-transpose-windows)
         ("C-x M-e" . crux-eval-and-replace)
         ("C-^" . crux-top-join-line)
         ("C-a" . crux-move-beginning-of-line)))

(use-package ace-window :straight t
  :bind (("M-o" . ace-window)
         ("C-x o" . ace-window)))

(winner-mode +1)

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
  :bind ("M-s g" . rgrep))

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

(setq tab-always-indent 'complete)

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
         (:map minibuffer-mode-map
               ("C-l" . embark-collect-live))
         (:map embark-identifier-map
               ("D" . embark-devdocs-lookup)))
   :config
 (add-to-list 'display-buffer-alist
              '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                nil
                (window-parameters (mode-line-format . none))))
 (add-hook 'embark-collect-post-revert-hook
          (defun resize-embark-collect-window (&rest _)
            (when (memq embark-collect--kind '(:live :completions))
              (fit-window-to-buffer (get-buffer-window)
                                    (max (floor (frame-height) 4) 10) 1)))))

(use-package embark-consult :straight t
 :after (embark consult)
 :demand t
 :hook
 (embark-collect-mode . consult-preview-at-point-mode))

(use-package selectrum :straight t
 :config
 (selectrum-mode +1))
(use-package selectrum-prescient :straight t
 :config
 (selectrum-prescient-mode +1)
 (prescient-persist-mode +1))

(use-package savehist
  :init
  (savehist-mode))

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
                       (man Man-mode-map)
                       (woman woman-mode-map)
                       (package package-menu-mode-map)
                       (eww eww-mode-map)
                       (dired dired-mode-map))
   do (eval-after-load mode `(define-key ,map "'" #'link-hint-open-link))))


;; Delimiter editing/structured editing

(use-package parinfer-rust-mode :straight t
  :hook ((emacs-lisp-mode lisp-mode) . parinfer-rust-mode))
(use-package electric-pair
  :hook ((js-mode python-mode text-mode typescript-mode typescript-tsx-mode) . electric-pair-local-mode))
(show-paren-mode 1)

;; LSP support
(use-package flycheck :straight t
 :hook (flycheck-mode . (lambda ()
                          (if (display-graphic-p)
                           (flycheck-pos-tip-mode)
                           (flycheck-popup-tip-mode)))))

(use-package flycheck-pos-tip :straight t
 :commands flycheck-pos-tip-mode)
(use-package flycheck-popup-tip :straight t
 :commands flycheck-popup-tip-mode)

(use-package lsp-mode :straight t
  :init
  (setq lsp-headerline-breadcrumb-enable nil)
  (setq lsp-keymap-prefix "C-c l")
  :hook ((lsp-mode . lsp-enable-which-key-integration)
         (lsp-mode . electric-pair-local-mode)
         ((rjsx-mode . (lambda ()
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

(use-package iedit :straight t)

(use-package consult-lsp :straight t
  :after (consult lsp)
  :commands (consult-lsp-symbols consult-lsp-diagnostics)
  :init
  (define-key lsp-mode-map [remap xref-find-apropos] #'consult-lsp-symbols)
  (define-key lsp-mode-map (kbd "C-c e") #'consult-lsp-diagnostics))

(use-package lsp-sonarlint :straight t)

;; Other programming niceties

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

(use-package rjsx-mode :straight t)
(use-package json-mode :straight t)

(use-package javascript-mode :ensure nil
  :hook (js-mode . lsp)
  :bind ((:map js-mode-map
                  ("C-c d" . devdocs-lookup))))

(use-package typescript-mode :straight t
 :mode (rx ".ts" string-end)
 :bind ((:map typescript-mode-map
                 ("C-c d" . devdocs-lookup)))
 :hook ((typescript-mode typescript-tsx-mode) . lsp)
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

(use-package python-pytest :straight t
  :bind (:map python-mode-map
              ("C-c M-t" . python-pytest-dispatch)))

(use-package pyvenv :straight t)

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

(use-package lsp-haskell :straight t)

(use-package zig-mode :straight t
  :config (setq zig-format-on-save nil)
  :hook ((zig-mode . electric-pair-local-mode)
         (zig-mode . lsp)))

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

(use-package ctrlf :straight t
  :init (ctrlf-mode +1))

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
               ("C-'" . avy-isearch))))

(setq inferior-lisp-program "ros -Q run")
(use-package sly :straight t)
(use-package sly-quicklisp :straight t)

(use-package cider :straight t)

;; Looks

(use-package helpful :straight t
  :bind (("C-h f" . helpful-callable))
   ("C-h v" . helpful-variable)
   ("C-h k" . helpful-key))

(load "~/.emacs.d/themes/ceres-theme.el")
(load-theme 'ceres t)

(use-package solaire-mode :straight t
 :config
 (solaire-global-mode +1))

(set-face-attribute 'default nil :font "Fira Code" :height 110)
(global-hl-line-mode +1)

(use-package mood-line :straight t
  :config (mood-line-mode))

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
(use-package org-bullets :straight t
 :custom
 (org-bullets-bullet-list '("◉" "○"))
 :hook (org-mode . org-bullets-mode))

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
   '("1bf499628d54a42ae009b4f2f752c978f35f6a882e9025d25bea60893f0497e3" "982e45bb83eceebae9427d3595750923d30f574406fa3579872b1e6f541dbb03" "8acff5c51594488e06a284ebe2ee461e594f1f0b9cfe1eca691b682bccc17ab8" "f683ed3bd51b382129b5a37f2274b4a309497b057019cbe829821be36ae9059b" "0b0fe98ef2d1d9e65af498064a3dff485c09ab6f1f945b53afef1305b2b6130d" "ddf1a8fb709f9c974b764f12370f58e166cb11e8893366e675f4de5d5bb08017" "0ca407654295e80555a59ac173c789fb1f6fb06f94f9a53ed7d6517b021b38a6" "b900e7362d1049d4d2b4244f3a9a50eb59cc73dd65c83ea81c20659e3ab71595" "976f726421fb7b616376258cc2c5567392b113f885d480fe6f0753ab8e917996" "76db88acebfe8d7aecbac1ef2774cde04b86df5ac9f994b784b88baf501d0f67" "d746b3f894b54ffda5c683b309443e1f1a2d3da3e8bfffc8ff56721cd735d843" "692185aa53ed84c34bf0ef171e6a781c49c1b2680646e1045e72651c7179b6d2" "1eb55e21c75d5cd40afdb9ea8fd0b002716dc26714621869b8d429fd2aad0cc7" "6ccc8bb072a59ef522712719e2d69d18e22e5d11b41844c9703f53594c3d5397" "41d77463d60f52763b829b093e5f1bd07912994d434f8a4d851a9b2275d2fcd4" "28a359ced8026870d8c9c9ef77195ece0a6d7fcbc704d649c0738bb2786f44f6" "f07f34717602564cf0a0e839132003de82d0f59b9b842917fb98812b32c09422" "56b9cdcd58c83c6a25a6b4cf3c1387cfa40c4b2b3e28d11c6a8f786492e7d4db" "5ee8b67ec605e2743daf7edc3644b5a7b1a0145d2b078e7a1bfa47d63b5760fd" "c388c353c3c1052580a68ccbd73ecc4b5872157263c308e092fc9988d68f4288" "39b1e77823c24989cd59f092ee8cb2c0abd10e4efc2fff307071e89a85653553" "b6cabfbec1b53121ae62584b0ae2b748ec74b346afbb29ac1aaa788fa9e11eb5" "95f3086aef8d255ca8b21cc28d22052a98e3da75fede49084a7ba00e5a0b5ff0" "670dea67ad7d5715d30fbde5fda222d29c5580d7cd54ad981989c101d132c1b4" "ce93c03c4c5faa7413b66096349e18f9da2a1a9519a4ffdfc1c9550c8099e756" "01efe37b0d91845ebd5a801802de137c78ae1eb7bc1eecd189487bf818378483" "fea3edcf0423f12043140e2318f918b31d2c300cc02193f432dfd07e3b5d2952" "c9f38716edb9842eb26d80edf4baa39ea40cafc24f908292dcb9ba9f9922641a" "f01073c0f8539ab557e19e59e04076eb9c6d834bac8d4139d63778eed71b686a" default)))
 
(custom-set-faces)
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name
        "straight/repos/straight.el/bootstrap.el"
        (or (bound-and-true-p straight-base-dir)
            user-emacs-directory)))
      (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; Setup use-package
(use-package diminish :straight t)
(use-package bind-key :straight t)
(global-auto-revert-mode 1)

;; Performance tweaks
(setq gc-cons-threshold 100000000)
(setq read-process-output-max (* 1024 1024))
(global-so-long-mode 1)
(global-subword-mode 1)
(diminish 'subword-mode)

;; Swap to a bunch of more useful keybindings than the defaults
(bind-key "M-\\" 'cycle-spacing)
(bind-key "M-u" 'upcase-dwim)
(bind-key "M-l" 'downcase-dwim)
(bind-key "M-c" 'capitalize-dwim)
(setq completion-cycle-threshold 3)
(setq split-height-threshold nil)
 
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

(use-package boon
  :straight t
  :init
  (require 'boon-colemak)
  (boon-mode))

(use-package ef-themes
  :straight t
  :bind ("<f5>" . my/toggle-theme)
  :custom ((ef-themes-bold-constructs t)
	   (ef-themes-mixed-constructs t)
	   (ef-themes-mixed-fonts t)
	   (ef-themes-variable-pitch-ui t)
	   (ef-themes-mode-line-accented t)
	   (ef-themes-links '(neutral-underline))
	   (ef-themes-prompts '(bold italic)))
  :init
  (defvar my/current-theme-mode 'light)
  (defun my/toggle-theme ()
    (interactive)
    (let ((dark-theme 'ef-maris-dark)
	  (light-theme 'ef-duo-light))
      (if (eq my/current-theme-mode 'light)
	  (progn
	    (disable-theme light-theme)
	    (load-theme dark-theme :no-confirm)
	    (setq my/current-theme-mode 'dark))
	(progn
	  (disable-theme dark-theme)
	  (load-theme light-theme :no-confirm)
	  (setq my/current-theme-mode 'light)))))
  (load-theme 'ef-duo-light :no-confirm))

(global-hl-line-mode 1)

(menu-bar-mode -1)
(tool-bar-mode -1)
(when (display-graphic-p)
  (scroll-bar-mode -1))
(setq inhibit-startup-message t)

(set-face-attribute 'default nil :font "Berkeley Mono" :height 140)
(set-face-attribute 'variable-pitch nil :font "Roboto" :height 140)
(setq-default line-spacing 8)

(bind-key "C-x f" #'find-file)
(bind-key "C-x C-b" #'ibuffer)
(bind-key "C-x s" #'save-buffer)
(bind-key "C-x C-s" #'save-some-buffers)
(bind-key "C-x e" #'eval-last-sexp)
(bind-key "C-x ;" #'comment-line)

(use-package ibuffer-vc
  :straight t
  :hook (ibuffer . (lambda ()
		     (ibuffer-vc-set-filter-groups-by-vc-root)
		     (unless (eq ibuffer-sorting-mode 'alphabetic)
		       (ibuffer-do-sort-by-alphabetic)))))

;; Documentation enhancements
(use-package which-key
  :straight t
  :diminish which-key-mode
  :config
  (which-key-mode +1))

(bind-key "C-h ." #'eldoc)
(diminish 'eldoc-mode)
(eldoc-mode +1)

(setq sentence-end-double-space nil)

(add-hook 'doc-view-mode-hook #'(lambda () (internal-show-cursor nil nil)))

(use-package ace-window
  :straight t
  :config
  (setq aw-scope 'frame)
  (bind-key "C-x o" 'ace-window)
  (bind-key "M-o" 'ace-window))

(winner-mode +1)

;; Pulse current line when jumping somewhere
(defun my/pulse-line (&rest _)
 "Pulse the current line"
 (pulse-momentary-highlight-one-line (point)))
(dolist (command '(recenter-top-bottom other-window ace-window other-frame xref-pop-marker-stack))
  (advice-add command :after #'my/pulse-line))

(add-hook 'grep-mode-hook #'toggle-truncate-lines)

(use-package wgrep
  :straight t
  :custom ((wgrep-auto-save-buffer t)
	   (wgrep-change-readonly-file t)))

(setq view-read-only t)

;; Searching
(use-package anzu
  :straight t
  :bind (
	 ([remap query-repalace] . anzu-query-replace)
	 ([remap query-replace-regaexp] . anzu-query-replace-regexp)
	 (:map boon-command-map
	       ("%" . query-replace)))
  :config
  (global-anzu-mode +1))

;; ;; Completion/selection

(use-package marginalia
  :straight t
  :config
  (marginalia-mode))

(savehist-mode)

(use-package orderless
  :straight t
  :custom
  (completion-styles '(orderless basic))
  (completion-cycle-threshold 1)
  (tab-always-indent 'complete)
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles . (partial-completion))))))

(use-package vertico
  :straight (:files (:defaults "extensions/*"))
  :config
  (vertico-mode 1))

(use-package vertico-directory
  :after vertico
  :straight nil
  :bind (:map vertico-map
	      ("RET" . vertico-directory-enter)
	      ("DEL" . vertico-directory-delete-char)
	      ("M-DEL" . vertico-directory-delete-word))
  :hook (rfn-eshadow-update-overlay . vertico-directory-tidy))

(use-package consult
  :straight t
  :bind (([remap goto-line] . consult-goto-line)
	 ("M-i" . consult-imenu)
	 ([remap yank-pop] . consult-yank-pop)
	 ("M-s r" . consult-ripgrep)
	 ("C-x b" . 'consult-buffer)
	 (:map boon-command-map
	       ("p" . consult-line)))
  :hook (completion-list-mode . consult-preview-at-point-mode))

(use-package corfu
  :straight (:files (:defaults "extensions/*"))
  :init
  (global-corfu-mode))

(use-package corfu-popupinfo
  :after corfu
  :hook (corfu-mode . corfu-popupinfo-mode)
  :custom
  (corfu-popupinfo-delay '(0.25 . 0.1))
  (corfu-popupinfo-hide nil)
  :config
  (corfu-popupinfo-mode))

(use-package kind-icon
  :if (display-graphic-p)
  :straight t
  :after corfu
  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

(use-package embark
  :straight t
  :bind ("C-." . embark-act))

(use-package embark-consult
  :after embark
  :straight t
  :hook (embark-collect-mode . consult-preview-at-point-mode))

(setq minibuffer-prompt-properties
      '(read-only t cursor-intangible t face minibuffer-prompt))
(add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)
(add-hook 'minibuffer-setup-hook #'(lambda () (setq-local truncate-lines t)))

(setq enable-recursive-minibuffers t )

;; Dired
(put 'dired-find-alternate-file 'disabled nil)
(add-hook 'dired-mode-hook #'dired-hide-details-mode)

;; ;; Delimiters
(electric-pair-mode 1)

(use-package flymake-diagnostic-at-point
  :after flymake
  :straight t
  :custom (flymake-diagnostic-at-point-display-diagnostic-function
	   'flymake-diagnostic-at-point-display-popup)
  :config 
    (require 'flymake-diagnostic-at-point)
    (add-hook 'flymake-mode-hook #'flymake-diagnostic-at-point-mode))

(setq treesit-language-source-alist
      '((bash "https://github.com/tree-sitter/tree-sitter-bash")
	(cmake "https://github.com/uyha/tree-sitter-cmake")
	(css "https://github.com/tree-sitter/tree-sitter-css")
	(elisp "https://github.com/Wilfred/tree-sitter-elisp")
	(go "https://github.com/tree-sitter/tree-sitter-go")
	(html "https://github.com/tree-sitter/tree-sitter-html")
	(javascript "https://github.com/tree-sitter/tree-sitter-javascript" "master" "src")
	(json "https://github.com/tree-sitter/tree-sitter-json")
	(make "https://github.com/alemuller/tree-sitter-make")
	(markdown "https://github.com/ikatyang/tree-sitter-markdown")
	(python "https://github.com/tree-sitter/tree-sitter-python")
	(rust "https://github.com/tree-sitter/tree-sitter-rust")
	(toml "https://github.com/tree-sitter/tree-sitter-toml")
	(tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
	(vue "https://github.com/xiaoxin-sky/tree-sitter-vue" "master" "src")
	(typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
	(yaml "https://github.com/ikatyang/tree-sitter-yaml")))
;; Use this to install tree sitter grammars
(mapc #'treesit-install-language-grammar (mapcar #'car treesit-language-source-alist))

(use-package web-mode
  :straight t)
(define-derived-mode vue-mode web-mode "Vue")
(add-to-list 'auto-mode-alist '("\\.vue\\'" . vue-mode))

(defun my/vue-eglot-init-options ()
  (let ((tsdk-path (expand-file-name
		    "lib"
		    (string-trim-right (shell-command-to-string "npm list --global --parseable typescript | head -n1")))))
    `(:typescript (:tsdk ,tsdk-path
			 :languageFeatures (:completion
					    (:defaultTagNameCase "both"
					     :defaultAttrNameCase "kebabCase"
					     :getDocumentNameCasesRequest nil
					     :getDocumentSelectionRequest nil
					     :diagnostics
					     (:getDocumentVersionRequest nil))
			 :documentFeatures (:documentFormatting
				            (:defaultPrintWidth 100
				             :getDocumentPrintWidthRequest nil)
					    :documentSymbol t
					    :documentColor t))))))
(defun my/eglot-setup ()
  (add-to-list 'eglot-server-programs
	       '(tsx-ts-mode . ("typescript-language-server" "--stdio")))
  (add-to-list 'eglot-server-programs
	       '(typescript-ts-mode . ("typescript-language-server" "--stdio")))
(add-to-list 'eglot-server-programs
	     `(vue-mode . ("vue-language-server" "--stdio" :initializationOptions ,(my/vue-eglot-init-options))))  
  (setq eglot-events-buffer-size 0)
  (fset #'jsonrpc--log-event #'ignore)
  (setq eglot-extend-to-xref t)
  (setq eglot-put-doc-in-help-buffer nil)
  (setq eglot-send-changes-idle-item 0.25)
  (setq eglot-sync-connect nil)
  (eglot--code-action eglot-code-action-organize-imports-ts "source.organizeImports.ts")
  (eglot--code-action eglot-code-action-add-missing-imports-ts "source.addMissingImports.ts")
  (bind-key "C-c C-a" 'eglot-code-actions 'eglot-mode-map)
  (bind-key "C-c C-r" 'eglot-rename 'eglot-mode-map)
  (bind-key "C-c C-f" 'eglot-format-buffer 'eglot-mode-map)
  (bind-key "C-c C-l C-m" 'eglot-code-action-add-missing-imports-ts 'tsx-ts-mode-map)
  (bind-key "C-c C-l C-i" 'eglot-code-action-organize-imports-ts 'tsx-ts-mode-map)
  (bind-key "C-c C-l C-m" 'eglot-code-action-add-missing-imports-ts 'typescript-ts-mode-map)
  (bind-key "C-c C-l C-i" 'eglot-code-action-organize-imports-ts 'typescript-ts-mode-map))
(add-hook 'eglot-managed-mode-hook 'my/eglot-setup)

(use-package sly
  :straight t)

(use-package markdown-mode
  :straight t
  :mode ("README\\.md\\'" . gfm-mode))

(use-package vterm
  :straight t)

(use-package apheleia
  :straight t
  :diminish apheleia-mode
  :config
  (with-eval-after-load 'apheleia
    (setf (alist-get 'prettier apheleia-formatters) '(npx "prettier" "--stdin-filepath" filepath))
    (setf (alist-get 'prettier-css apheleia-formatters)'(npx "prettier" "--stdin-filepath" filepath "--parser=css"))
    (setf (alist-get 'prettier-html apheleia-formatters)'(npx "prettier" "--stdin-filepath" filepath "--parser=html"))
    (setf (alist-get 'prettier-graphql apheleia-formatters)'(npx "prettier" "--stdin-filepath" filepath "--parser=graphql"))
    (setf (alist-get 'prettier-javascript apheleia-formatters)'(npx "prettier" "--stdin-filepath" filepath "--parser=babel-flow"))
    (setf (alist-get 'prettier-json apheleia-formatters)'(npx "prettier" "--stdin-filepath" filepath "--parser=json"))
    (setf (alist-get 'prettier-markdown apheleia-formatters)'(npx "prettier" "--stdin-filepath" filepath "--parser=markdown"))
    (setf (alist-get 'prettier-ruby apheleia-formatters)'(npx "prettier" "--stdin-filepath" filepath "--parser=ruby"))
    (setf (alist-get 'prettier-scss apheleia-formatters)'(npx "prettier" "--stdin-filepath" filepath "--parser=scss"))
    (setf (alist-get 'prettier-svelte apheleia-formatters)'(npx "prettier" "--stdin-filepath" filepath "--parser=svelte"))
    (setf (alist-get 'prettier-vue apheleia-formatters)'(npx "prettier" "--stdin-filepath" filepath "--parser=vue"))
    (setf (alist-get 'prettier-typescript apheleia-formatters)'(npx "prettier" "--stdin-filepath" filepath "--parser=typescript"))
    (setf (alist-get 'prettier-yaml apheleia-formatters)'(npx "prettier" "--stdin-filepath" filepath "--parser="yaml)))
  (apheleia-global-mode +1))    

(add-to-list 'auto-mode-alist (cons (rx ".tsx" string-end) #'tsx-ts-mode))
(add-to-list 'auto-mode-alist (cons (rx ".ts" string-end) #'typescript-ts-mode))

(use-package copilot
  :straight (:host github :repo "copilot-emacs/copilot.el" :files ("dist" "*.el"))
  :ensure t
  :hook (prog-mode . copilot-mode)
  :bind (("C-c M-f" . copilot-complete)
	 (:map copilot-completion-map
	       ("C-g" . copilot-clear-overlay)
               ("<right>" . copilot-accept-completion)
               ("C-f" . copilot-accept-completion)
               ("M-<right>" . copilot-accept-completion-by-word)
               ("M-f" . copilot-accept-completion-by-word)
               ("C-e" . copilot-accept-completion-by-line)
               ("<end>" . copilot-accept-completion-by-line)
               ("M-n" . copilot-next-completion)
               ("M-p" . copilot-previous-completion))))

;; Git
(use-package magit
  :straight t
  :bind ("C-x g" . magit-status))

(use-package git-timemachine :straight t)

(use-package undo-tree
  :straight t
  :diminish undo-tree-mode
  :bind ("C-x u" . undo-tree-visualize)
  :custom (undo-tree-auto-save-history nil)
  :init
  (global-undo-tree-mode))

;; Org
(use-package org-modern
  :straight t
  :hook (((org-mode . org-modern-mode)
	  (org-agenda-finalize . org-modern-agenda))))


(use-package olivetti
  :straight t)

(use-package mermaid-mode
  :straight t)

;; Platform specifics
(use-package exec-path-from-shell
  :if (memq window-system '(mac ns x))
  :straight t
  :config
  (exec-path-from-shell-initialize)
  (setq-default mac-option-modifier 'meta)
  (setq delete-by-moving-to-trash 'system-move-file-to-trash)
  (setq ns-right-option-modifier nil
	mac-right-option-modifier nil
        mac-command-modifier 'super))

(with-eval-after-load 'eshell
  (bind-key "C-r" #'consult-history 'eshell-mode-map))

(use-package eat
  :straight t
  :config
  (add-hook 'eshell-load-hook #'eat-eshell-mode)
  (add-hook 'eshell-load-hook #'eat-eshell-visual-command-mode))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("bebec7cd48f56fbca1c878d7f43ece10d5390ab95790883d95ae4c0f6045600a" "14ba61945401e42d91bb8eef15ab6a03a96ff323dd150694ab8eb3bb86c0c580" "5014b68d3880d21a5539af6ef40c1e257e9045d224efb3b65bf0ae7ff2a5e17a" "4d4475c85408bbc9d71e692dd05d55c6b753d64847f5e364d1ebec78d43e2aef" "90eb3b47cd9bf5eec0f5820682fcb40b4a37fc128b12eceb39e09f64758a8ebe" "6c655326d9bb38d4be02d364d344bfa61b3c8fdabd1cf4b97dddc8c0b3047b47" "3ca84532551daa1b492545bbfa47fd1b726ca951d8be29c60a3214ced30b86f5" "86a9bfbda652a2dd887077a4ad91afbec2fde569e26147ceb8a605976c99d8d2" "8081bc8961e8428dc7897544d6deaa9099b0807e57fc4281187c1bda4ff0c386" default))
 '(package-selected-packages
   '(mermaid-mode eglot olivetti org-modern eat gcmh vterm markdown markdown-mode web-mode ibuffer-vc ibuffer-vs ef-themes sly slime embark-consult embark exec-path-from-shell git-timemachine apheleia flymake-diagnostic-at-point corfu orderless ace-window which-key boon no-littering wgrep vertico undo-tree modus-themes marginalia magit kind-icon diminish anzu)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

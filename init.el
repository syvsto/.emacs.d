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
(straight-use-package 'diminish)
(straight-use-package 'bind-key)
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
(straight-use-package 'no-littering)

;; store all backup and autosave files in the tmp dir
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))
(setq create-lockfiles nil) ;; Allow other processes (such as Storybook) to access files at the same time as Emacs

(setq ring-bell-function 'ignore)
(defalias 'yes-or-no-p 'y-or-n-p)
(recentf-mode +1)

(define-key input-decode-map [?\C-m] [C-m])

(straight-use-package 'boon)
(require 'boon-colemak)
(boon-mode)

(straight-use-package 'standard-themes)
(setq standard-themes-bold-constructs t
      standard-themes-mixed-constructs t
      standard-themes-mixed-fonts t
      standard-themes-variable-pitch-ui t
      standard-themes-mode-line-accented t
      standard-themes-fringes '(subtle)
      standard-themes-links '(neutral-underline)
      standard-themes-prompts '(bold italic))
(mapc #'disable-theme custom-enabled-themes)
(bind-key "<f5>" #'standard-themes-toggle)
(load-theme 'standard-light :no-confirm)

(global-hl-line-mode 1)

(menu-bar-mode -1)
(tool-bar-mode -1)
(when (display-graphic-p)
  (scroll-bar-mode -1))
(setq inhibit-startup-message t)

(set-face-attribute 'default nil :font "Berkeley Mono" :height 130)

(bind-key "C-x f" #'find-file)
(bind-key "C-x s" #'save-buffer)
(bind-key "C-x C-s" #'save-some-buffers)
(bind-key "C-x e" #'eval-last-sexp)
(bind-key "C-x ;" #'comment-line)

;; Documentation enhancements
(straight-use-package 'which-key)
(which-key-mode +1)

(bind-key "C-h ." #'eldoc)
(eldoc-mode +1)

(setq sentence-end-double-space nil)

(add-hook 'doc-view-mode-hook #'(lambda () (internal-show-cursor nil nil)))

(straight-use-package 'ace-window)
(setq aw-scope 'frame)
(bind-key "C-x o" 'ace-window)
(bind-key "M-o" 'ace-window)

(winner-mode +1)

;; Pulse current line when jumping somewhere
(defun my/pulse-line (&rest _)
 "Pulse the current line"
 (pulse-momentary-highlight-one-line (point)))
(dolist (command '(recenter-top-bottom other-window ace-window other-frame xref-pop-marker-stack))
  (advice-add command :after #'my/pulse-line))

(add-hook 'grep-mode-hook #'toggle-truncate-lines)

(straight-use-package 'wgrep)
(setq wgrep-auto-save-buffer t)
(setq wgrep-change-readonly-file t)

(setq view-read-only t)

;; Searching
(straight-use-package 'anzu)
(bind-key [remap query-repalace] 'anzu-query-replace)
(bind-key [remap query-replace-regaexp] 'anzu-query-replace-regexp)
(global-anzu-mode +1)

;; ;; Completion/selection

(straight-use-package 'marginalia)
(marginalia-mode)

(savehist-mode)

(straight-use-package 'orderless)
(setq completion-styles '(orderless basic))
(setq completion-cycle-threshold 3)
(setq tab-always-indent 'complete)
(setq completion-category-defaults nil)
(setq completion-category-overrides '((file (styles . (partial-completion)))))

(straight-use-package 'vertico)
(vertico-mode 1)

(straight-use-package 'consult)
(bind-key [remap goto-line] 'consult-goto-line)
(bind-key "p" 'consult-line 'boon-command-map)
(bind-key "M-i" 'consult-imenu)
(bind-key [remap yank-pop] 'consult-yank-pop)
(bind-key "M-s r" 'consult-ripgrep)

(straight-use-package 'corfu)
(global-corfu-mode)

(setq minibuffer-prompt-properties
      '(read-only t cursor-intangible t face minibuffer-prompt))
(add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)
(add-hook 'minibuffer-setup-hook #'(lambda () (setq-local truncate-lines t)))

(setq enable-recursive-minibuffers t )

;; ;; Delimiters
(electric-pair-mode 1)

(straight-use-package 'flymake-diagnostic-at-point)
(setq flymake-diagnostic-at-point-display-diagnostic-function
      'flymake-diagnostic-at-point-display-popup)
(eval-after-load 'flymake
  (progn
    (require 'flymake-diagnostic-at-point)
    (add-hook 'flymake-mode-hook #'flymake-diagnostic-at-point-mode)))

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
	(toml "https://github.com/tree-sitter/tree-sitter-toml")
	(tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
	(typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
	(yaml "https://github.com/ikatyang/tree-sitter-yaml")))

(defun my/eglot-setup ()
  (add-to-list 'eglot-server-programs
	       '(tsx-ts-mode . ("typescript-language-server" "--stdio")))
  (add-to-list 'eglot-server-programs
	       '(typescript-ts-mode . ("typescript-language-server" "--stdio")))
  (setq eglot-events-buffer-size 4096)
  (setq eglot-extend-to-xref t)
  (setq eglot-put-doc-in-help-buffer nil)
  (eglot--code-action eglot-code-action-organize-imports-ts "source.organizeImports.ts")
  (eglot--code-action eglot-code-action-add-missing-imports-ts "source.addMissingImports.ts")
  (bind-key "C-c a" 'eglot-code-actions 'eglot-mode-map)
  (bind-key "C-c r" 'eglot-rename 'eglot-mode-map)
  (bind-key "C-c l m" 'eglot-code-action-add-missing-imports-ts 'tsx-ts-mode-map)
  (bind-key "C-c l i" 'eglot-code-action-organize-imports-ts 'tsx-ts-mode-map)
  (bind-key "C-c l m" 'eglot-code-action-add-missing-imports-ts 'typescript-ts-mode-map)
  (bind-key "C-c l i" 'eglot-code-action-organize-imports-ts 'typescript-ts-mode-map))
(add-hook 'eglot-managed-mode-hook 'my/eglot-setup)

(straight-use-package 'apheleia)
(apheleia-global-mode +1)

(add-to-list 'auto-mode-alist (cons (rx ".tsx" string-end) #'tsx-ts-mode))
(add-to-list 'auto-mode-alist (cons (rx ".ts" string-end) #'typescript-ts-mode))

;; Git
(straight-use-package 'magit)
(bind-key "C-x g" 'magit-status)

(straight-use-package 'git-timemachine)

(straight-use-package 'vundo)
(bind-key "C-x u" 'vundo)
(setq vundo-glyph-alist 'vundo-unicode-symbols)

;; Platform specifics
(when (memq window-system '(mac ns x))
  (progn
    (straight-use-package 'exec-path-from-shell)
    (exec-path-from-shell-initialize))
  (setq-default mac-option-modifier 'meta)
  (setq delete-by-moving-to-trash 'system-move-file-to-trash)
  (setq ns-right-option-modifier nil
	mac-right-option-modifier nil
        mac-command-modifier 'super))

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
    (setq-default mac-option-modifier 'meta)))


;; Some options that make Emacs less intrusive

(use-package no-littering :straight t)
(setq ring-bell-function 'ignore)
(recentf-mode +1)


;; Objed - modal editing

(use-package objed :straight t
  :config (objed-mode 1))
(use-package avy :straight t)
(use-package multiple-cursors :straight t)
(use-package which-key :straight t
  :diminish which-key-mode
  :config (which-key-mode +1))


;; Diminish some commonly used modes where the lighter isn't very useful

(use-package eldoc :ensure nil
  :diminish eldoc-mode
  :config
  (eldoc-mode +1))

;; Looks

(tool-bar-mode -1)
(menu-bar-mode -1)
(when (display-graphic-p)
  (scroll-bar-mode -1))

(use-package svg-tag-mode :straight (:host github :repo "rougier/svg-tag-mode"))
(use-package mini-frame :straight t)
(add-to-list 'load-path (expand-file-name "~/.emacs.d/nano-emacs"))
(require 'nano-layout)
(require 'nano-base-colors)
(require 'nano-faces)
(nano-faces)
(require 'nano-theme)
(nano-theme)
(require 'nano-defaults)
(require 'nano-session)
(require 'nano-modeline)
(require 'nano-help)
(require 'nano-splash)


;; Completion/selection

(use-package selectrum :straight t)
(use-package selectrum-prescient :straight t)

(setq tab-always-indent 'complete)

(selectrum-mode +1)
(selectrum-prescient-mode +1)
(prescient-persist-mode +1)

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
         ;; Custom M-"/' bindings for fast register access
         ("M-\"" . consult-register-load)
         ("M-'" . consult-register-store)          ;; orig. abbrev-prefix-mark (unrelated)
         ("C-M-\"" . consult-register)
         ;; Other custom bindings
         ("M-y" . consult-yank-pop)                ;; orig. yank-pop
         ("<help> a" . consult-apropos)            ;; orig. apropos-command
         ;; M-g bindings (goto-map)
         ("M-g e" . consult-compile-error)
         ("M-g f" . consult-flymake)               ;; Alternative: consult-flycheck
         ("M-g g" . consult-goto-line)             ;; orig. goto-line
         ("M-g M-g" . consult-goto-line)           ;; orig. goto-line
         ("M-g o" . consult-outline)
         ("M-g m" . consult-mark)
         ("M-g k" . consult-global-mark)
         ("M-g i" . consult-imenu)
         ("M-g I" . consult-project-imenu)
         ;; M-s bindings (search-map)
         ("M-s f" . consult-find)
         ("M-s L" . consult-locate)
         ("M-s g" . consult-grep)
         ("M-s G" . consult-git-grep)
         ("M-s r" . consult-ripgrep)
         ("M-s l" . consult-line)
         ("M-s m" . consult-multi-occur)
         ("M-s k" . consult-keep-lines)
         ("M-s u" . consult-focus-lines)
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


;; Interactive evaluation for languages with REPL (integrates with Objed)

(use-package eval-in-repl :straight t)

(defun my/slime-completion-in-region (_fn completions start end)
  (funcall completion-in-region-function start end completions nil))

(use-package slime :straight (:host github :repo "nuddyco/slime" :branch "clime")
  :defer nil
  :config
  (advice-add 'slime-display-or-scroll-completions :around #'my/slime-completion-in-region))
(setq inferior-lisp-program "sbcl")
;; (cl-pushnew 'slime-clime slime-contribs)
(cl-pushnew 'slime-quicklisp slime-contribs)
(slime-setup)

(electric-pair-mode +1)
(show-paren-mode +1)
(use-package paredit :straight t
  :hook ((lisp-mode emacs-lisp-mode) . paredit-mode))


;; LSP support 

(use-package lsp-mode :straight t
  :init
  (setq lsp-keymap-prefix "C-c l")
  :hook ((lsp-mode . lsp-enable-which-key-integration)
	 ((js-mode python-mode) . lsp)))

(use-package consult-lsp :straight t
  :bind (:map lsp-mode-map
	      ([remap xref-find-apropos] . consult-lsp-symbols)))


;; Language specifics

(use-package python-pytest :straight t
  :bind (:map python-mode-map
	      ("C-c M-t" . python-pytest-dispatch)))

(use-package pyvenv :straight t)


;; Project handling

(use-package projectile :straight t
  :config
  (projectile-mode +1)
  (defun my/objed-projectile (_a _b)
    (objed--reset--objed-buffer)
    (projectile-commander)
    (objed--init (or objed--object 'char)))
  (defun my/objed-magit (_a _b)
    (objed--reset--objed-buffer)
    (magit-status)
    (objed--exit-objed))
  (objed-define-op "x p" my/objed-projectile)
  (objed-define-op "x g" my/objed-magit)
  (define-key projectile-mode-map (kbd "C-x p") 'projectile-command-map))

;; Custom modes etc.
(use-package observable-dataflow-mode
  :straight (:host github :repo "syvsto/observable-dataflow-mode"))

;; Git
(use-package magit :straight t)

(use-package ibuffer-vc :straight t
  :bind ("C-x C-b" . ibuffer)
  :hook (ibuffer-mode . (lambda ()
			  (ibuffer-vc-set-filter-groups-by-vc-root)
			  (unless (eq ibuffer-sorting-mode 'alphabetic)
			    (ibuffer-do-sort-by-alphabetic)))))

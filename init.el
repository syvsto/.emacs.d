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


;; Modal editing

(use-package smart-god-mode
  :straight (:host github :repo "syvsto/smart-god-mode")
  :defer nil
  :commands (smart-god-local-mode smart-god-mode-all smart-god-mode-set-exit-and-do-keys)
  :bind (("C-x C-1" . delete-other-windows)
         ("C-x C-2" . split-window-below)
         ("C-x C-3" . split-window-right)
         ("C-x C-0" . delete-window)
         (:map smart-god-local-mode-map
               ("q" . smart-god-local-mode)))
  :config
  (setq smart-god-mod-alist '((nil . "C-")
                              ("m" . "M-")
                              ("u" . "C-M-")))
  (smart-god-mode-set-exit-and-do-keys
   '("'" "," ":" "/" "-" "SPC" "*" "@" "_" "+" "=" "!" "#" "$"
     "%" "^" "&" "." "`" "~" "<left>"))
  (setq smart-god-mode-do-and-enter-keys '("<up>" "<down>" "<right>" ")" "]" "}")
        smart-god-mode-auto-enter-on-ctrl-keys t
        smart-god-mode-auto-enter-on-ctrl-exempt-keys '("C-g" "C-o"))
  
  (defun my/update-cursor ()
    (setq cursor-type (cond (smart-god-local-mode 'box)
                            (buffer-read-only 'hbar)
                            (t 'bar))))
  (add-hook 'post-command-hook #'my/update-cursor)
  (add-hook 'smart-god-mode-disabled-hook #'my/update-cursor)
  (add-hook 'smart-god-mode-enabled-hook #'my/update-cursor)
  
  (smart-god-mode-all))

(use-package which-key :straight t
  :diminish which-key-mode
  :config (which-key-mode +1))

(use-package eldoc :ensure nil
  :diminish eldoc-mode
  :config
  (eldoc-mode +1))


;; Navigation

(use-package ace-window :straight t
  :bind ("M-o" . ace-window))

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
         ;; Custom M-#' bindings for fast register access
         ("M-#" . consult-register-load)
         ("M-'" . consult-register-store)          ;; orig. abbrev-prefix-mark (unrelated)
         ("C-M-#" . consult-register)
         ;; Other custom bindings
         ("M-y" . consult-yank-pop)                ;; orig. yank-pop
         ("<help> a" . consult-apropos)            ;; orig. apropos-command
         ;; M-g bindings (goto-map)
         ("M-g C-e" . consult-compile-error)
         ("M-g C-f" . consult-flymake)               ;; Alternative: consult-flycheck
         ("M-g C-g" . consult-goto-line)             ;; orig. goto-line
         ("M-g M-g" . consult-goto-line)           ;; orig. goto-line
         ("M-g C-o" . consult-outline)
         ("M-g C-m" . consult-mark)
         ("M-g C-k" . consult-global-mark)
         ("M-g C-i" . consult-imenu)
         ("M-g C-I" . consult-project-imenu)
         ;; M-s bindings (search-map)
         ("M-s C-f" . consult-find)
         ("M-s C-L" . consult-locate)
         ("M-s C-g" . consult-grep)
         ("M-s C-G" . consult-git-grep)
         ("M-s C-r" . consult-ripgrep)
         ("M-s C-l" . consult-line)
         ("M-s C-m" . consult-multi-occur)
         ("M-s C-k" . consult-keep-lines)
         ("M-s C-u" . consult-focus-lines)
         ;; Isearch integration
         ("M-s e" . consult-isearch)
         :map isearch-mode-map
         ("M-e" . consult-isearch)                 ;; orig. isearch-edit-string
         ("M-s C-e" . consult-isearch)               ;; orig. isearch-edit-string
         ("M-s C-l" . consult-line))                 ;; required by consult-line to detect isearch
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
  :bind ((:map lisp-mode-map
               ("C-c s" . slime-selector)))
  :config
  (advice-add 'slime-display-or-scroll-completions :around #'my/slime-completion-in-region))
(setq inferior-lisp-program "sbcl")
;; (cl-pushnew 'slime-clime slime-contribs)
(cl-pushnew 'slime-quicklisp slime-contribs)
(slime-setup)

(use-package parinfer-rust-mode :straight t
  :hook ((emacs-lisp-mode lisp-mode) . parinfer-rust-mode)
  :config
  (setq-default indent-tabs-mode nil)
  (setq parinfer-rust-auto-download t))
(show-paren-mode +1)

;; LSP support 

(use-package lsp-mode :straight t
  :init
  (setq lsp-headerline-breadcrumb-enable nil)
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

;; Searching

(use-package anzu :straight t
  :diminish anzu-mode
  :config
  (global-anzu-mode +1))


;; Looks

(use-package svg-tag-mode :straight (:host github :repo "rougier/svg-tag-mode"))
(use-package mini-frame :straight t)
(add-to-list 'load-path (expand-file-name "~/.emacs.d/nano-emacs"))
(require 'nano-layout)
(require 'nano-theme-light)

(require 'nano-faces)
(nano-faces)

(require 'nano-theme)
(nano-theme)

(require 'nano-session)
(require 'nano-modeline)
(require 'nano-help)
(require 'nano-splash)

(straight-use-package 'org-plus-contrib :includes 'org)
(require 'nano-writer)

(menu-bar-mode -1)
(tool-bar-mode -1)
(when (display-graphic-p)
  (scroll-bar-mode -1))




;;; ceres-theme.el --- Theme 

;; Copyright (C) 2022, Syver Storm-Furru

;; Author: Syver Storm-Furru
;; Version: 0.1
;; Package-Requires: ((emacs "24"))
;; Created with ThemeCreator, https://github.com/mswift42/themecreator.


;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>.

;; This file is not part of Emacs.

;;; Commentary:

;;; Code:

 (deftheme ceres)
(let ((class '((class color) (min-colors 89)))
      (bg-lighter "#fbfbf8")
      (bg-light "#f0ede4")
      (bg "#f5f3ed")
      (fg "#101010")
      (fg2 "#888888")
      (blue "#009bc2")
      (blue-light "#82e6ff")
      (warning "#e61c1c")
      (warning2 "#fcba03"))
  (custom-theme-set-faces
   'ceres
   `(default ((,class (:background ,bg :foreground ,fg))))
   `(solaire-default-face ((,class (:background ,bg-light))))
   `(font-lock-comment-face ((,class (:foreground ,fg2))))
   `(font-lock-negation-char-face ((,class (:foreground ,fg))))
   `(font-lock-reference-face ((,class (:foreground ,fg))))
   `(font-lock-constant-face ((,class (:foreground ,fg))))
   `(font-lock-builtin-face ((,class (:foreground ,fg))))
   `(font-lock-doc-face ((,class (:foreground ,fg2))))
   `(font-lock-function-name-face ((,class (:foreground ,fg :bold ,class))))
   `(font-lock-keyword-face ((,class (:bold ,class :foreground ,fg))))
   `(font-lock-string-face ((,class (:foreground ,blue))))
   `(font-lock-type-face ((,class (:bold ,class))))
   `(font-lock-variable-name-face ((,class (:foreground ,fg))))
   `(font-lock-warning-face ((,class (:foreground ,warning))))
   `(term-color-black ((,class (:foreground ,fg :background nil))))
   `(region ((,class (:background ,fg :foreground ,bg))))
   `(highlight ((,class (:foreground ,bg :background ,fg))))
   `(hl-line ((,class (:background  ,bg-light))))
   `(solaire-hl-line-face ((,class (:background ,bg))))
   `(fringe ((,class (:background ,bg))))
   `(cursor ((,class (:background ,fg :foreground ,bg))))
   `(show-paren-match ((,class (:inherit underline))))
   `(sp-pair-overlay-face ((,class :inherit underline)))
   `(isearch ((,class (:bold t :foreground ,fg :background ,blue-light :bold t))))
   `(isearch-group-1 ((,class (:bold t :foreground ,fg :background ,warning :bold t))))
   `(isearch-group-2 ((,class (:bold t :foreground ,fg :background ,warning :bold t))))
   `(isearch-fail ((,class :inherit warning)))
   `(lazy-highlight ((,class :bold nil :background ,warning2 :foreground ,fg)))
   `(match ((,class :bold nil :background ,warning :foreground ,fg)))
   `(header-line ((,class :foreground ,bg :background ,fg)))
   `(mode-line ((,class (:overline ,fg :underline ,fg :bold t :foreground ,bg :background ,fg))))
   `(mode-line-inactive ((,class (:overline ,fg :foreground ,fg :background ,bg :weight normal))))
   `(vertical-border ((,class (:foreground ,fg))))
   `(minibuffer-prompt ((,class (:bold t :foreground ,fg))))
   `(default-italic ((,class (:italic t))))
   `(link ((,class (:foreground ,fg :underline t))))

   ;; Org-mode
   `(org-code ((,class (:inherit fixed-pitch))))
   `(org-block ((,class (:inherit fixed-pitch))))
   `(org-hide ((,class (:foreground ,fg2))))
   `(org-document-title ((,class (:inherit fixed-pitch :bold t :height 1.6))))
   `(org-level-1 ((,class (:inherit fixed-pitch :bold t :height 1.2))))
   `(org-level-2 ((,class (:inherit fixed-pitch :bold nil :height 1.1))))
   `(org-level-3 ((,class (:inherit fixed-pitch :bold t))))
   `(org-level-4 ((,class (:inherit fixed-pitch :bold nil))))
   `(org-headline-done ((,class (:foreground ,fg2 :strike-through t))))
   `(org-checkbox ((,class (:inherit fixed-pitch :bold t))))
   `(org-date ((,class (:underline t))))
   `(org-footnote  ((,class (:underline t))))
   `(org-link ((,class (:underline t))))
   `(org-special-keyword ((,class (:foreground ,fg))))

   `(org-agenda-structure ((,class (:foreground ,fg2))))

   `(markdown-header-face-1 ((,class (:bold t :height 1.6))))
   `(markdown-header-face-2 ((,class (:bold t :height 1.2))))
   `(markdown-header-face-3 ((,class (:bold nil :height 1.1))))
   `(markdown-header-face-4 ((,class (:bold t))))

   `(anzu-mode-line ((,class :inherit bold :background ,blue-light)))
   `(anzu-mode-line-no-match ((,class :inherit bold :background ,warning)))
   `(anzu-replace-highlight ((,class :inherit isearch :underline t)))
   `(anzu-replace-to ((,class :background ,warning2 :foreground ,fg :underline t)))

   ;; Objed
   `(objed-hl ((,class :background ,bg-lighter)))
   `(objed-mode-line ((,class :background ,blue-light)))

   ;; rainbow delimiters
   `(rainbow-delimiters-base-face-error ((,class :foreground ,warning)))
   `(rainbow-delimiters-base-face ((,class :foreground ,fg2)))
   `(rainbow-delimiters-depth-1-face ((,class :inherit rainbow-delimiters-base-face)))
   `(rainbow-delimiters-depth-2-face ((,class :inherit rainbow-delimiters-base-face)))
   `(rainbow-delimiters-depth-3-face ((,class :inherit rainbow-delimiters-base-face)))
   `(rainbow-delimiters-depth-4-face ((,class :inherit rainbow-delimiters-base-face)))
   `(rainbow-delimiters-depth-5-face ((,class :inherit rainbow-delimiters-base-face)))
   `(rainbow-delimiters-depth-6-face ((,class :inherit rainbow-delimiters-base-face)))
   `(rainbow-delimiters-depth-7-face ((,class :inherit rainbow-delimiters-base-face)))
   `(rainbow-delimiters-depth-8-face ((,class :inherit rainbow-delimiters-base-face)))
   `(rainbow-delimiters-depth-9-face ((,class :inherit rainbow-delimiters-base-face)))
   `(rainbow-delimiters-mismatched-face ((,class :inherit bold :foreground ,warning)))
   `(rainbow-delimiters-unmatched-face ((,class :inherit bold :foreground ,warning2)))

   ;; magit
   `(magit-header-line ((t (:underline nil))))
   `(magit-branch-local ((,class :inherit italic :foreground ,fg :underline t)))
   `(magit-section-heading ((,class :bold t :foreground ,fg :height 1.1 :underline t)))
   `(magit-branch-remote ((,class :inherit italic :foreground ,fg :underline t)))
        
   `(transient-argument ((,class :inherit bold :foreground ,fg)))
   `(transient-value ((,class :inherit bold :foreground ,fg)))

   ;; git gutter
   `(git-gutter-fr+-added ((,class :foreground ,fg2)))
   `(git-gutter-fr+-deleted ((,class :foreground ,fg2)))
   `(git-gutter-fr+-modified ((,class :foreground ,fg2)))
   `(git-gutter+-added ((,class :foreground ,fg2)))
   `(git-gutter+-deleted ((,class :foreground ,fg2)))
   `(git-gutter+-modified ((,class :foreground ,fg2)))

   ;; diff-hl
   `(diff-hl-change ((,class :foreground ,blue :background ,bg)))
   `(diff-hl-insert ((,class :foreground "PaleGreen" :background ,bg)))
   `(diff-hl-delete ((,class :foreground ,warning :background ,bg)))

   ;; avy
   `(avy-lead-face ((,class :background ,blue :foreground ,bg-lighter :bold t)))
   `(avy-lead-face-0 ((,class :background ,warning :foreground ,bg-lighter :bold t)))
   `(avy-lead-face-1 ((,class :background ,warning2 :foreground ,bg-lighter :bold t)))
   `(avy-lead-face-2 ((,class :background ,blue-light :foreground ,bg-lighter :bold t)))

   ;; ace-window
   `(aw-leading-char-face ((,class :inherit bold :height 3.0 :background ,bg :foreground ,warning)))
        
   ;; completions
   `(completions-common-part ((,class :background ,blue-light)))
   `(orderless-match-face-0 ((,class :background ,blue-light)))
   `(orderless-match-face-1 ((,class :background ,warning)))
   `(orderless-match-face-2 ((,class :background ,warning2)))
   `(orderless-match-face-3 ((,class :background ,bg-light)))
        
   ;; js2 and derived modes
   `(js2-function-param ((,class :foreground ,fg :italic t)))

   ;; eshell
   `(eshell-prompt ((,class :bold t :foreground ,fg :background ,blue-light)))

   ;; dired
   `(all-the-icons-dired-dir-face ((,class :foreground ,fg)))
   `(dired-directory ((,class :foreground ,fg)))
   `(dired-flagged ((,class :background ,blue-light :foreground ,fg)))
   `(dired-header ((,class :foreground ,fg :bold t :underline t)))
   `(diredfl-file-name ((,class :foreground ,fg)))
   `(diredfl-file-suffix ((,class :foreground ,fg2)))
   `(diredfl-dir-name ((,class :foreground ,fg :italic t)))
   `(diredfl-dir-heading ((,class :foreground ,fg :bold t :underline t)))

   ;; speedbar
   `(speedbar-selected-face ((,class :bold t :foreground ,fg)))
   `(speedbar-highlight-face ((,class :bold t :foreground ,fg :background ,bg :underline t)))
   `(speedbar-file-face ((,class :foreground ,fg)))
   `(speedbar-directory-face ((,class :foreground ,fg)))
   `(speedbar-tag-face ((,class :foreground ,fg2)))

   ;; ibuffer
   `(ibuffer-deletion-face ((,class :foreground ,warning)))
   `(ibuffer-filter-group-name-face ((,class :bold t)))
   `(ibuffer-marked-face ((,class :bold t :foreground ,bg :background ,fg)))
   `(ibuffer-title-face ((,class :bold t :height 1.1)))

   ;; Helm
   `(helm-action ((,class :underline t)))
   `(helm-selection ((,class :foreground ,fg :background ,blue-light)))
   `(helm-selection-line ((,class :bold t :underline t)))
   `(helm-source-header ((,class :bold t :underline t :height 1.3)))
   `(helm-ff-directory ((,class :bold t)))
   `(helm-ff-dotted-directory ((,class :italic t :bold t)))
   `(helm-match ((,class :foreground ,blue :background ,bg)))
   `(helm-match-item ((,class :background ,blue-light :foreground ,fg)))
   `(helm-ff-file-extension ((,class :foreground ,fg2)))

   ;; Selectrum
   `(selectrum-current-candidate ((,class :underline t :bold t)))
   `(selectrum-primary-highlight ((,class :bold t)))
   `(selectrum-secondary-highlight ((,class :foreground ,blue)))

   ;; Vertico
   `(vertico-current ((,class :bold t :underline t)))

   ;; Consult
   `(consult-annotation ((,class :foreground ,bg-light)))
   `(consult-file ((,class :foreground ,fg2)))

   ;; Company-tooltip
   `(company-tooltip ((,class :background ,bg-lighter)))
   `(company-tooltip-annotation ((,class :foreground ,blue)))
   `(company-tooltip-selection ((,class :foreground ,bg-lighter :background ,fg :bold t)))
   `(company-tooltip-common ((,class :italic t)))
   `(company-tooltip-common-selection ((,class :foreground ,bg-lighter :background ,fg :italic t)))

   ;; Nano modeline
   `(nano-modeline-active ((,class (:bold t :foreground ,bg :background ,fg))))
   `(nano-modeline-active-name ((,class (:bold t :foreground ,bg :background ,fg))))
   `(nano-modeline-active-primary ((,class (:bold t :foreground ,bg :background ,fg))))
   `(nano-modeline-active-secondary ((,class (:bold t :foreground ,bg :background ,fg))))
   `(nano-modeline-active-status-RO ((,class (:bold t :foreground ,bg :background ,warning))))
   `(nano-modeline-active-status-RW ((,class (:bold t :foreground ,bg :background ,blue))))
   `(nano-modeline-active-status-** ((,class (:bold t :foreground ,bg :background ,warning2))))
   `(nano-modeline-inactive ((,class (:bold nil :foreground ,bg-light :background ,fg))))
   `(nano-modeline-inactive-name ((,class (:bold nil :foreground ,bg-light :background ,fg))))
   `(nano-modeline-inactive-primary ((,class (:bold nil :italic t :foreground ,bg-light :background ,fg))))
   `(nano-modeline-inactive-secondary ((,class (:bold nil :italic t :foreground ,bg-light :background ,fg))))
   `(nano-modeline-inactive-status-RO ((,class (:bold nil :italic t :foreground ,bg-light :background ,fg))))
   `(nano-modeline-inactive-status-RW ((,class (:bold nil :italic t :foreground ,bg-light :background ,fg))))
   `(nano-modeline-inactive-status-** ((,class (:bold nil :italic t :foreground ,bg-light :background ,fg))))

   ;; IBuffer
   `(ibuffer-filter-group-name ((,class (:bold t :underline t : height 1.2))))
   
   ;; sh-mode
   `(sh-quoted-exec ((,class (:italic t :foreground ,fg))))

   ;; lsp mode
   `(lsp-face-highlight-textual ((,class (:background ,blue-light))))

   ;; lsp ui mode
   `(lsp-ui-peek-filename ((,class (:foreground ,fg :italic t))))
   `(lsp-ui-peek-highlight ((,class (:foreground ,fg :background ,blue-light))))
   `(lsp-ui-peek-selection ((,class (:foreground ,bg-lighter :background ,fg))))))


;;;###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'ceres)

;; Local Variables:
;; no-byte-compile: t
;; End:

;;; ceres-theme.el ends here


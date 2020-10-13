;;; synthetiq-theme.el --- Theme 

;; Copyright (C) 2020 , 

;; Author: 
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

 (deftheme synthetiq)
 (let ((class '((class color) (min-colors 89)))
       (fg "#f4f4f4")
       (fg2 "#888888")
       (bg "#000000")
       (bg-light "#333333")
       (blue "#42d9ff")       
       (warning "#e61c1c")
       (warning2 "#fcba03"))
   (custom-theme-set-faces
   'synthetiq
        `(default ((,class (:background ,bg :foreground ,fg))))
	`(font-lock-comment-face ((,class (:foreground ,fg2))))
	`(font-lock-negation-char-face ((,class (:foreground ,fg))))
	`(font-lock-reference-face ((,class (:foreground ,fg))))
	`(font-lock-constant-face ((,class (:foreground ,fg))))
	`(font-lock-builtin-face ((,class (:foreground ,fg))))
        `(font-lock-doc-face ((,class (:foreground ,fg2))))
        `(font-lock-function-name-face ((,class (:foreground ,fg :bold ,class ))))
        `(font-lock-keyword-face ((,class (:bold ,class :foreground ,fg))))
        `(font-lock-string-face ((,class (:foreground ,blue))))
        `(font-lock-type-face ((,class (:bold ,class))))
        `(font-lock-variable-name-face ((,class (:foreground ,fg))))
        `(font-lock-warning-face ((,class (:foreground ,warning))))
        `(term-color-black ((,class (:foreground ,fg :background nil))))
        `(region ((,class (:background ,bg-light))))
        `(highlight ((,class (:foreground ,bg :background ,fg))))
	`(hl-line ((,class (:background  ,bg-light))))
	`(fringe ((,class (:background ,bg))))
	`(cursor ((,class (:background ,fg :foreground ,bg))))
        `(show-paren-match-face ((,class (:background ,blue))))
        `(isearch ((,class (:bold t :foreground ,bg :background ,blue))))
        `(mode-line ((,class (:bold t :foreground ,bg :background ,fg))))
        `(mode-line-inactive ((,class ( :foreground ,fg :background ,bg-light :weight normal))))
	`(vertical-border ((,class (:foreground ,fg))))
        `(minibuffer-prompt ((,class (:bold t :foreground ,fg))))
        `(default-italic ((,class (:italic t))))
	`(link ((,class (:foreground ,fg :underline t))))
	`(org-code ((,class (:foreground ,fg2))))
	`(org-hide ((,class (:foreground ,fg2))))
        `(org-level-1 ((,class (:bold t :height 1.1))))
        `(org-level-2 ((,class (:bold nil))))
        `(org-level-3 ((,class (:bold t))))
        `(org-level-4 ((,class (:bold nil))))
        `(org-date ((,class (:underline t) )))
        `(org-footnote  ((,class (:underline t))))
        `(org-link ((,class (:underline t))))
        `(org-special-keyword ((,class (:foreground ,fg))))
	`(anzu-mode-line ((,class :inherit bold :background ,blue)))
	`(anzu-mode-line-no-match ((,class :inherit bold :background ,warning)))
	`(anzu-replace-highlight ((,class :inherit isearch :underline t)))
	`(anzu-replace-to ((,class :background ,warning2 :foreground ,bg :underline t)))
	`(objed-mode-line ((,class :background ,blue)))
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
))

;;;###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'synthetiq)

;; Local Variables:
;; no-byte-compile: t
;; End:

;;; synthetiq-theme.el ends here

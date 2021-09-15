;;; tracer.el --- auto-insert trace statements based on the current major mode          -*- lexical-binding: t; -*-

;; Copyright (C) 2021 Syver Storm-Furru

;; Author: Syver Storm-Furru <syver.storm.furru@gmail.com>
;; Keywords: programming, debugging
;; Version: 0.0.1

;;; Commentary:

;;; Code:

(defun tracer-trace ()
  "Trace the current function based on the current major mode."
  (interactive)
  (let ((thing (thing-at-point 'symbol 'no-properties)))
    (cond ((derived-mode-p 'typescript-mode) (trace-js thing))
          ((derived-mode-p 'js-mode) (trace-js thing))
          ((derived-mode-p 'haskell-mode) (trace-haskell thing)))))

(defun trace-js (symbol)
  (end-of-line)
  (newline)
  (insert (concat "console.log(\"" symbol " = \", " symbol ");"))
  (newline))
  
(defun trace-haskell (symbol)
  (backward-sexp)
  (kill-sexp)
  (insert (concat "(trace (show $ " symbol ") " symbol ") ")))

(defun trace-rust (symbol)
  (end-of-line)
  (newline)
  (insert (concat "println!(\"" symbol " = {:?}\", &" symbol ");"))
  (newline))

(define-minor-mode tracer-mode
  "Mode that sets up keybindings for tracer. Enable the minor mode in supported modes"
  :lighter "Tracer"
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "C-c .") 'tracer-trace)
            map))
   
(provide 'tracer)
;;; tracer.el ends here

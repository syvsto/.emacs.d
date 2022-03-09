;;; flower.el --- Make flow charts within emacs          -*- lexical-binding: t; -*-

;; Copyright (C) 2022 Syver Storm-Furru

;; Author: Syver Storm-Furru <syver.storm.furru@gmail.com>
;; Keywords: programming, design
;; Version: 0.0.1

;;; Commentary:

;;; Code:

(require 's)
(require 'dash)

(defvar flower-use-drop-shadows t "Whether to use shadows on nodes.")
(defvar flower--shadow-character "░" "The character to use for drop shadows.")

(setq flower-nodes '((0)))
(setq flower-links '())

(defun flower-new (name)
  "Create a new flowchart."
  (interactive "sName of the flowchart: ")
  (let ((buf (switch-to-buffer (format "%s" name))))
    (with-current-buffer buf
      (picture-mode))))

(defun flower-add-title (title)
  (interactive "sTitle: ")
  (let ((id (+ (caar flower-nodes) 1)))
    (push (list id (list (line-number-at-pos) (current-column)) title) flower-nodes))
  (flower--draw-title title))

(defun flower-add-node (title)
  (interactive "sTitle: ")
  (let ((id  (+ (caar flower-nodes) 1)))
    (push (list id (list (current-column) (line-number-at-pos)) title) flower-nodes))
  (flower--draw-node title))

(defun flower-render ()
  (interactive)
  (erase-buffer)
  (goto-char 0)
  (flower--draw-links)
  (flower--draw-nodes))

(defun flower-add-link (from->to)
  (interactive "sLink: ")
  (let* ((fromto (s-split "->" from->to))
	 (from (car fromto))
	 (to (cadr fromto))
	 (start (flower--find from))
	 (start-pos (cadr start))
	 (y0 (+ (cadr start-pos) 2))
	 (x0 (+ (car start-pos) (/ (length (caddr start)) 2)))
	 (end (flower--find to))
	 (end-pos (cadr end))
	 (y1 (+ (cadr end-pos) 1))
	 (x1 (+ (car end-pos) (/ (length (caddr end)) 2))))
    (push (list x0 x1 y0 y1) flower-links)))

(defun flower--draw-links ()
  (mapc (lambda (l) (let ((x0 (car l))
			  (x1 (cadr l))
			  (y0 (caddr l))
			  (y1 (cadddr l)))
		      (goto-char 0)
		      (if (> y1 y0)
			  (picture-move-down y0)
			(picture-move-down y1))
		      	      (picture-forward-column x0)
		      (flower--draw-arrow x0 x1 y0 y1))) flower-links))

(defun flower--draw-nodes ()
  (mapc (lambda (l) (let* ((node-pos (cadr l))
			   (title (caddr l))
			   (x (car node-pos))
			   (y (cadr node-pos)))
		     (print (list node-pos title x y))
		     (goto-char 0)
		     (picture-move-down y)
		     (picture-forward-column x)
		     (flower--draw-node title))) (butlast flower-nodes)))

(defun flower--find (idx)
  (nth (--find-index (= (string-to-number idx) (car it)) flower-nodes) flower-nodes))

(defun flower--next-line (width)
  (picture-backward-column (+ width 2))
  (picture-move-down 1))

(defun flower--draw-title (title)
  (let* ((len (length title))
	 (width (+ len 6)))
    (flower--draw-title-top width)
    (flower--draw-title-separator width)
    (flower--draw-title-title title)
    (flower--draw-title-separator width)
    (flower--draw-title-bottom width)))

(defun flower--draw-title-top (len)
  (flower--insert "╔")
  (dotimes (i len)
    (flower--insert "═"))
  (flower--insert "╗")
  (flower--next-line len))

(defun flower--draw-title-bottom (len)
  (flower--insert "╚")
  (dotimes (i len)
    (flower--insert "═"))
  (flower--insert "╝"))

(defun flower--draw-title-separator (len)
  (flower--insert "║")
  (dotimes (i len)
    (flower--insert " "))
  (flower--insert "║")
  (flower--next-line len))

(defun flower--draw-title-title (title)
  (flower--insert "║   ")
  (flower--insert title)
  (flower--insert "   ║")
  (flower--next-line (+ (length title) 6)))

(defun flower--draw-node (title)
  (let* ((len (length title))
	 (width (+ len 6)))
    (flower--draw-node-top width)
    (flower--draw-node-separator width)
    (flower--draw-node-title title)
    (flower--draw-node-separator width)
    (flower--draw-node-bottom width)))

(defun flower--draw-node-top (len)
  (flower--insert "┌")
  (dotimes (i len)
    (flower--insert "─"))
  (flower--insert "┐")
  (flower--next-line len))

(defun flower--draw-node-bottom (len)
  (flower--insert "└")
  (dotimes (i len)
    (flower--insert "─"))
  (flower--insert "┘")
  (when flower-use-drop-shadows
    (flower--shade-node-line-end)
    (flower--next-line (+ len 1))
    (flower--shade-node-bottom len)))

(defun flower--draw-node-separator (len)
  (flower--insert "│")
  (dotimes (i len)
    (flower--insert " "))
  (flower--insert "│")
  (when flower-use-drop-shadows
    (flower--shade-node-line-end))
  (flower--next-line (if flower-use-drop-shadows
			 (+ len 1)
		       len)))

(defun flower--draw-node-title (title)
  (let ((len (+ (length title) 6)))
    (flower--insert "│   ")
    (flower--insert title)
    (flower--insert "   │")
    (when flower-use-drop-shadows
      (flower--shade-node-line-end))
    (flower--next-line (if flower-use-drop-shadows
			   (+ len 1)
			 len))))

(defun flower--shade-node-line-end ()
  (flower--insert flower--shadow-character))

(defun flower--shade-node-bottom (len)
  (flower--insert " ")
  (dotimes (i (+ len 2))
    (flower--insert flower--shadow-character)))

(defun flower--draw-arrow (x0 x1 y0 y1)
  "Flower--Insert an arrow between points (x0 y0) and (x1 y1)."
  (let* ((x-len (abs (- x1 x0)))
	 (y-len (abs (- y1 y0)))
	 (wider-p (< 1.0  (/ x-len y-len))))
    (when wider-p
	(cond ((and (> y1 y0) (> x1 x0)) (flower--draw-right-arrow-below x-len y-len))
	      ((and (> y1 y0) (< x1 x0)) (flower--draw-left-arrow-below x-len y-len))
	      ((and (< y1 y0) (> x1 x0)) (flower--draw-right-arrow-above x-len y-len))
	      ((and (< y1 y0) (< x1 x0)) (flower--draw-left-arrow-above x-len y-len))))))


(defun flower--draw-right-arrow-above (x-len y-len)
  (let ((x-mid (/ x-len 2))
	(y-mid (/ y-len 2)))
    (picture-forward-column x-mid)
    (flower--insert "┌")
    (dotimes (i x-mid)
      (flower--insert "─"))
    (flower--insert "▶")
    (flower--next-line (- x-len 1))
    (dotimes (i y-len)
      (flower--draw-arrow-vertical-spacer x-mid))
    (dotimes (i x-mid)
      (flower--insert "─"))
    (flower--insert "┘")))

(defun flower--draw-right-arrow-below (x-len y-len)
  (let ((x-mid (/ x-len 2))
	(y-mid (/ y-len 2)))
    (dotimes (i x-mid)
      (flower--insert "─"))
    (flower--insert "┐")
    (flower--next-line (- x-mid 1))
    (dotimes (i y-len)
      (flower--draw-arrow-vertical-spacer x-mid))
    (picture-forward-column x-mid)
    (flower--insert "└")
    (dotimes (i x-mid)
      (flower--insert "─"))
    (flower--insert "▶")))

(defun flower--draw-left-arrow-below (x-len y-len)
  (let ((x-mid (/ x-len 2))
	(y-mid (/ y-len 2)))
    (dotimes (i x-mid)
      (flower--insert " "))
    (flower--insert "┌")
    (dotimes (i x-mid)
      (flower--insert "─"))
    (flower--next-line x-len)
    (dotimes (i y-len)
      (flower--draw-arrow-vertical-spacer x-mid))
    (flower--insert "◀")
    (dotimes (i (- x-mid 1))
      (flower--insert "─"))
    (flower--insert "┘")))

(defun flower--draw-left-arrow-above (x-len y-len)
  (let ((x-mid (/ x-len 2))
	(y-mid (/ y-len 2)))
    (flower--insert "◀")
    (dotimes (i (- x-mid 1))
      (flower--insert "─"))
    (flower--insert "┐")
    (flower--next-line x-len)
    (dotimes (i y-len)
      (flower--draw-arrow-vertical-spacer x-mid))
    (dotimes (i x-mid)
      (flower--insert " "))
    (flower--insert "└")
    (dotimes (i x-mid)
      (flower--insert "─"))))

(defun flower--draw-arrow-vertical-spacer (x)
  (picture-forward-column x)
  (flower--insert "│")
  (flower--next-line (- x 1)))

(defun flower--insert (str)
  (mapc (lambda (c) (picture-insert c 1)) str))

(provide 'flower)
;;; flower.el ends here

;;; web-search.el --- Search for things using webkit in Emacs          -*- lexical-binding: t; -*-

;; Copyright (C) 2022 Syver Storm-Furru

;; Author: Syver Storm-Furru <syver.storm.furru@gmail.com>
;; Keywords: web, search
;; Version: 0.0.1

;;; Commentary:

;;; Code:

(defun my/search-webkit ()
  "Search for results from a specific website."
  (interactive)
  (let* ((site (completing-read "Site: " '("StackOverflow" "Mozilla Developer Network" "DuckDuckGo")))
	 (search-term (read-from-minibuffer "Search for: "))
	 (keywords (s-split " " search-term)))
	(cond ((equal site "StackOverflow") (my/so-search-webkit keywords))
	      ((equal site "DuckDuckGo") (my/ddg-search-webkit keywords))
	      ((equal site "Mozilla Developer Network") (my/mdn-search-webkit keywords))))) 

(defun my/so-search-webkit (keywords)
  (let* ((search-string (s-join "+" keywords))
	 (with-site (concat search-string "+site%3Astackoverflow.com")))
    (xwidget-webkit-browse-url (concat "https://duckduckgo.com/?q=" with-site))))

(defun my/mdn-search-webkit (keywords)
  (let* ((search-string (s-join "+" keywords))
	 (with-site (concat search-string "+site%3Adeveloper.mozilla.org")))
    (xwidget-webkit-browse-url (concat "https://duckduckgo.com/?q=" with-site))))

(defun my/ddg-search-webkit (keywords)
  (let ((search-string (s-join "+" keywords)))
    (xwidget-webkit-browse-url (concat "https://duckduckgo.com/?q=" search-string))))

(provide 'web-search)

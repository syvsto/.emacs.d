(defvar observable-dataflow-mode-font-lock-keywords
  '(()))

(define-derived-mode observable-dataflow-mode js-mode "OD"
  "A major mode for editing Observable notebooks through the Dataflow CLI."
  (font-lock-add-keywords nil observable-dataflow-mode-font-lock-keywords))
(add-to-list 'auto-mode-alist '("\\.ojs\\'" . observable-dataflow-mode))

(defun observable-dataflow-run (prefix-arg)
  "Run the Observable Dataflow server using the current file as the notebook to launch."
  (interactive "P")
  (if prefix-arg
      (observable-dataflow--run-with-stdlib)
    (observable-dataflow--run-current-file)))

(defun observable-dataflow-kill-server ()
  "Stop the currently running Observable Dataflow server."
  (interactive)
  (if (get-buffer "*dataflow server*")
      (kill-buffer "*dataflow server*")
    (message "Dataflow server not started yet!")))

(defun observable-dataflow--run-with-stdlib ()
  (let (stdlib (read-string "Standard library to use: "))
    (start-process "dataflow" "*dataflow server*" "run" buffer-file-name "--stdlib" stdlib)))

(defun observable-dataflow--run-current-file ()
  (start-process "dataflow" "*dataflow server*" "dataflow" "run" buffer-file-name))

(provide 'observable-dataflow-mode)

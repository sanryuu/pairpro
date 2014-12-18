;;; package --- 
;;; Commentary:
;;; Code:
(require 'f)

(defvar pearpro-mode nil)

(defvar pp-mode-timer nil)

(defvar pp-command-emacs "/Applications/Emacs.app/Contents/MacOS/Emacs")

(defvar pp-pear-setting-file "~/.emacs.d/pear/init.el")

(defvar pp-preload-file "~/.emacs.d/pp-preload.el")

(defvar pp-file "~/Dropbox/.emacs.d/dev/pearpro/pearpro.el")

(defun start-pearpro ()
  (interactive)
  (pp-export-load-path)
  (shell-command
   (concat pp-command-emacs
           " -Q -l " pp-preload-file
           " -l " pp-pear-setting-file
           " -l " pp-file " &")))

(defun pp-export-load-path ()
  (with-temp-buffer
    (let ((tmp-load-path load-path))
      (while (car tmp-load-path)
        (insert
         (concat
          "(add-to-list 'load-path \""
          (car tmp-load-path) "\")\n"))
        (setq tmp-load-path (cdr tmp-load-path)))
    (write-region (point-min) (point-max) pp-preload-file))))

(define-minor-mode pearpro-mode
  "pearpro-mode"
  :lighter " pp" ; for mode line
  (if pearpro-mode
      (pearpro-mode-start)
    (pearpro-mode-end)))

(defun pearpro-mode-start ()
  (setq pp-mode-timer
        (run-with-idle-timer
         0.1
         t
         'pp-sync)))

(defun pearpro-mode-end ()
  (cancel-timer pp-mode-timer))


(defun pp-sync ()
  )

(provide 'pearpro)

;;; pearpro.el ends here

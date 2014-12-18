(defvar pearpro-mode nil)

(defvar pp-mode-timer nil)

(defvar pp-command-emacs "/Applications/Emacs.app/Contents/MacOS/Emacs")

(defvar pp-pear-setting-file "~/.emacs.d/pear/init.el")

(defvar pp-file "~/Dropbox/.emacs.d/dev/pearpro/pear.el")

(defun start-pearpro ()
  (interactive)
  (shell-command (concat pp-command-emacs " -Q -l " pp-pear-setting-file " -l " pp-file " &")))

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

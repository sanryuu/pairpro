(defvar pearpro-mode nil)

(defvar pp-command-emacs "/Applications/Emacs.app/Contents/MacOS/Emacs")

(defvar pp-pear-setting-file "~/.emacs.d/pear/init.el")

(defun start-pearpro ()
  (interactive)
  (shell-command (concat pp-command-emacs " -Q -l " pp-pear-setting-file " &")))

(define-minor-mode pearpro-mode
  "pearpro-mode"
  :lighter " pp" ; for mode line
  (if pearpro-mode
      (pearpro-mode-start)
    (pearpro-mode-end)))

(defun pearpro-mode-start ()
  )

(defun pearpro-mode-end ()
  )

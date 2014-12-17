
(defvar pp-command-emacs "/Applications/Emacs.app/Contents/MacOS/Emacs")

(defvar pp-pear-setting-file "~/.emacs.d/pear/init.el")

(defun start-pearpro ()
  (interactive)
  (shell-command (concat pp-command-emacs " -Q -l " pp-pear-setting-file " &")))



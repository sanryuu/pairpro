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

(defvar pp-gest-flag nil)

(defun start-pearpro ()
  (interactive)
  (f-write
   (concat
    (pp-get-gest-flag)
    (pp-get-export-load-path))
   'utf-8 pp-preload-file)
  (shell-command
   (concat pp-command-emacs
           " -Q -l " pp-preload-file
           " -l " pp-pear-setting-file
           " -l " pp-file " &")))

(defun pp-get-export-load-path ()
  "ホスト側のload-pathをエクスポートできるようにELispを取得"
  (with-temp-buffer
    (let ((tmp-load-path load-path))
      (while (car tmp-load-path)
        (insert
         (concat
          "(add-to-list 'load-path \""
          (car tmp-load-path) "\")\n"))
        (setq tmp-load-path (cdr tmp-load-path)))
    (buffer-substring-no-properties
     (point-min) (point-max)))))

(defun pp-get-gest-flag ()
  "ゲスト判定ができるように設定するElisp取得"
  "(setq pp-gest-flag t)\n")

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
"バッファと一時ファイルと比較
→変更があれば(自分がファイルを変更したとき)
    一時ファイルを書き込み
    共有ファイルを書き込み
→変更が無ければ
    バッファと共有ファイルを比較
    →変更があれば (相手がファイルを変更したとき)
        共有ファイルを読み込み
        (共有ファイルの書き込み)
    →変更がなければ
        何もしない"
  (if ;; 共有ファイルが存在しなければ作成
      (not (f-exists? (pp-get-sync-file-name t (not pp-gest-flag))))
      (pp-write-sync-file nil (not pp-gest-flag)))
  (if ;; 一時ファイルが存在しなければ作成
      (not (f-exists? (pp-get-sync-file-name nil (not pp-gest-flag))))
      (progn
        (pp-read-sync-file t (not pp-gest-flag))
        (pp-write-sync-file nil (not pp-gest-flag))))
  ;; バッファと一時ファイルを比較
  (if
      (not
       (string=
        (pp-get-sync-file-string nil (not pp-gest-flag))
        (buffer-substring-no-properties (point-min) (point-max))))
      ;; バッファと一時ファイルが異なる
      (progn
        (pp-write-sync-file nil (not pp-gest-flag))
        (pp-write-sync-file t (not pp-gest-flag)))
    ;; バッファと一時ファイルが一致
    (if
        (not
         (string=
          (pp-get-sync-file-string t (not pp-gest-flag))
          (buffer-substring-no-properties (point-min) (point-max))))
        ;; バッファと共有ファイルが異なる
        (progn
          (pp-read-sync-file t (not pp-gest-flag))
          (pp-write-sync-file t (not pp-gest-flag))))))

(defvar sync-file-dir "/tmp/")

(defun pp-get-sync-file-name (is-share is-host)
  "同期用共有ファイル名の取得
is_share: 共有ファイルかどうかt,nil
is_host: 自分がホストかどうかt,nil"
  (concat
   sync-file-dir
   (f-filename (buffer-file-name))
   (if (not is-share)
       (if is-host
           ".host"
         ".gest"))))

(defun pp-write-sync-file (is-share is-host)
  "同期用のファイルへ書き込み
is_share: 共有ファイルかどうかt,nil
is_host: 自分がホストかどうかt,nil"
  (write-region
   (point-min) (point-max)
   (pp-get-sync-file-name is-share is-host) nil 1))

;; 同期用のファイルを読み込み
(defun pp-read-sync-file (is-share is-host)
  (let ((current-point (point)))
    (erase-buffer)
    (insert-file-contents
     (pp-get-sync-file-name is-share is-host))
    (goto-char current-point)
    ))

;; 同期用ファイルの文字列の取得
(defun pp-get-sync-file-string (is-share is-host)
  (f-read (pp-get-sync-file-name is-share is-host) 'utf-8))

;; カレントバッファの文字列取得
(defun pp-get-current-buffer-string ()
  (buffer-substring-no-properties (point-min) (point-max)))

(provide 'pearpro)

;;; pearpro.el ends here

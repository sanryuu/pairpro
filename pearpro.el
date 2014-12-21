;;; package --- 
;;; Commentary:
;;; Code:
(require 'f)

(defvar pearpro-mode nil)

(defvar pp-mode-timer nil)

(defvar pp-command-emacs "/Applications/Emacs.app/Contents/MacOS/Emacs"
  "Emacs起動のコマンド")

(defvar pp-pear-setting-file "~/.emacs.d/pear/init.el"
  "ゲストの設定ファイルのPath")


(defvar pp-preload-file "~/.emacs.d/pp-preload.el"
  "ゲスト用のペアプロの事前処理ファイルPath")

(defvar pp-postload-file "~/.emacs.d/pp-postload.el"
  "ゲスト用のペアプロの事後処理ファイルPath")

(defvar pp-file "~/Dropbox/.emacs.d/dev/pearpro/pearpro.el"
  "pearpro.elのファイルPath")

(defvar pp-sync-file-dir "/tmp/"
  "共有ファイルのディレクトリ")
(defvar pp-tmp-file-dir "/tmp/cache/"
  "ゲスト用の一時ファイルのディレクトリ")


(defvar pp-gest-flag nil)

(defun pp-start-pearpro ()
  (interactive)
  (pp-write-sync-file t (not pp-gest-flag))
  (if (not pearpro-mode)
      (pearpro-mode))
  (pp-write-preload-file)
  (pp-write-postload-file)
  (pp-start-pearpro-shell-command))

(defun pp-write-preload-file ()
  (f-write
   (concat
    (pp-get-gest-flag)
    (pp-get-export-load-path))
   'utf-8 pp-preload-file))

(defun pp-write-postload-file ()
  (f-write
   (concat
    ;; find-fileするために一時書き出し
    "(f-write\n"
    "  (f-read \"" (f-this-file) "\" 'utf-8)\n"
    "  'utf-8\n"
    "  (concat \"" pp-tmp-file-dir "\" \"" (f-filename (f-this-file)) "\"))\n"
    ;; hookを動かすためにfind-file
    "(switch-to-buffer\n"
    "  (find-file-noselect\n"
    "    (concat \"" pp-tmp-file-dir "\" \"" (f-filename (f-this-file)) "\")))\n"
    ;; ペアプロモードの開始
    "(pearpro-mode)\n"
    ;; バッファ共有用のファイルを読み出し
    "(pp-read-sync-file t (not pp-gest-flag))\n")
    'utf-8 pp-postload-file))

(defun pp-start-pearpro-shell-command ()
  (shell-command
   (concat pp-command-emacs
           " -Q -l " pp-preload-file
           " -l " pp-pear-setting-file
           " -l " pp-file
           " -l " pp-postload-file
           " &")))

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

(defun pp-get-sync-file-name (is-share is-host)
  "同期用共有ファイル名の取得
is_share: 共有ファイルかどうかt,nil
is_host: 自分がホストかどうかt,nil"
  (concat
   pp-sync-file-dir
   (pp-buffer-file-name)
   (if (not is-share)
       (if is-host
           ".host"
         ".gest"))))

(defun pp-buffer-file-name ()
  (f-filename (buffer-file-name)))

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

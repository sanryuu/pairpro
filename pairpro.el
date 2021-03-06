;;; package --- 
;;; Commentary:
;;; Code:
(require 'f)

(defvar pairpro-mode nil)
(defvar pairpro-online-mode nil
  "オンラインで同期")

(defvar pp-mode-timer nil)

(defvar pp-command-emacs "/Applications/Emacs.app/Contents/MacOS/Emacs"
  "Emacs起動のコマンド")

(defvar pp-pair-setting-file "~/.emacs.d/pair/init.el"
  "ゲストの設定ファイルのPath")


(defvar pp-preload-file "~/.emacs.d/pp-preload.el"
  "ゲスト用のペアプロの事前処理ファイルPath")

(defvar pp-postload-file "~/.emacs.d/pp-postload.el"
  "ゲスト用のペアプロの事後処理ファイルPath")

(defvar pp-file "~/Dropbox/.emacs.d/dev/pairpro/pairpro.el"
  "pairpro.elのファイルPath")

(defvar pp-sync-file-dir "/tmp/"
  "共有ファイルのディレクトリ")
(defvar pp-tmp-file-dir "/tmp/cache/"
  "ゲスト用の一時ファイルのディレクトリ")

(defvar pp-share-server-url
  "http://hostname/server.php"
  "ファイル共有サーバのURL")

(defvar pp-gest-flag nil)

(defun pp-start-pairpro ()
  (interactive)
  (pp-write-sync-file t (not pp-gest-flag))
  (if (not pairpro-mode)
      (pairpro-mode))
  (pp-write-preload-file)
  (pp-write-postload-file)
  (pp-start-pairpro-shell-command))

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
    "(pairpro-mode)\n"
    ;; バッファ共有用のファイルを読み出し
    "(pp-read-sync-file t (not pp-gest-flag))\n")
    'utf-8 pp-postload-file))

(defun pp-start-pairpro-shell-command ()
  (shell-command
   (concat pp-command-emacs
           " -Q -l " pp-preload-file
           " -l " pp-pair-setting-file
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

(define-minor-mode pairpro-mode
  "pairpro-mode"
  :lighter " pp" ; for mode line
  (if pairpro-mode
      (pairpro-mode-start)
    (pairpro-mode-end)))

(defun pairpro-mode-start ()
  (setq pp-mode-timer
        (run-with-idle-timer
         0.1
         t
         'pp-sync)))

(defun pairpro-mode-end ()
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

(defun pp-post-my-code (file-name body)
  "サーバにポストする
(pp-post-my-code \"hoge.el\" \"hogefuga\")"
  (let ((url-request-method "POST")
        (url-request-extra-headers
         '(("Content-Type" . "application/x-www-form-urlencoded")))
        (url-request-data
         (format "name=%s&body=%s"
                 (url-hexify-string file-name)
                 (url-hexify-string body))))
        (url-retrieve pp-share-server-url
                      '(lambda (status)
                         (kill-buffer (current-buffer))))))

;; 同期用のファイルを読み込み
(defun pp-read-sync-file (is-share is-host)
  (let ((current-point (point)))
    (erase-buffer)
    (insert-file-contents
     (pp-get-sync-file-name is-share is-host))
    (goto-char current-point)
    ))

(defun pp-get-pair-code (file-name)
  "サーバにからペアのコードを取得する
(pp-get-pair-code \"hoge.el\")"
  (let ((response))
    (url-retrieve
      (concat pp-share-server-url
              "?"
              (format "name=%s"
                      (url-hexify-string file-name)))
     '(lambda (status)
        (goto-line 9) ;; 8行目まではbodyでないため
        (setq tmp
          (buffer-substring-no-properties (point) (point-max)))
        (kill-buffer (current-buffer))))
      response))

;; 同期用ファイルの文字列の取得
(defun pp-get-sync-file-string (is-share is-host)
  (f-read (pp-get-sync-file-name is-share is-host) 'utf-8))

;; カレントバッファの文字列取得
(defun pp-get-current-buffer-string ()
  (buffer-substring-no-properties (point-min) (point-max)))

(provide 'pairpro)

;;; pairpro.el ends here

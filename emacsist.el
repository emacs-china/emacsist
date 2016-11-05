;;; emacsist.el --- produce REAEME.org  -*- lexical-binding: t; -*-

;;

;;; Code:

(require 'f)

(defgroup emacsist nil
  "leanote mini group"
  :prefix "emacsist-"
  :group 'org)

(defcustom emacsist-repo-root "~/github/emacsist"
  "the emacsist repo root path"
  :group 'emacsist
  :type 'string)

(defun emacsist ()
  (interactive)
  (emacsist-append-articles-list)
  (message "生成文章列表成功!"))

(defun emacsist-articles-list-content ()
  (let* ((apath (expand-file-name "articles" emacsist-repo-root)))
    (mapcar
     #'(lambda (item)
         (format "[[./articles/%s][%s]]  "
                 (f-filename item)
                 (f-base item)))
     (f-files apath
              (lambda (file) (f-ext? file "org"))))
    ))

(defun emacsist-append-articles-list ()
  "Find the correct position"
  (let* ((fname (expand-file-name "README.org" emacsist-repo-root)))
    (find-file fname)
    (goto-char (point-min))
    (let ((spos (search-forward "** 往期文章")))
      (when spos
        (kill-region spos (point-max))
        (insert "\n")
        (mapc #'(lambda (item)
                  (insert item)
                  (insert "\n"))
              (emacsist-articles-list-content))
        (save-buffer)
        ))))

(provide 'emacsist)
;;; emacsist.el ends here

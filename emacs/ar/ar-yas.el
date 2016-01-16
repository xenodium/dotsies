;;; ar-yas.el --- Yasnippet support.

;;; Commentary:
;; Yasnippet helpers.


;;; Code:

(require 'yasnippet)

(defun ar/yas-install--github-yasnippets (url dir-name)
  "Install yasnippets from git URL with DIR-NAME."
  (let ((command (concat "rm -rf /tmp/%s && "
                         "git clone %s /tmp/%s && "
                         "mkdir -p ~/.emacs.d/yasnippets/%s && "
                         "cp -r /tmp/%s/*-mode ~/.emacs.d/yasnippets/%s")))
    (async-shell-command (format command
                                 dir-name
                                 url
                                 dir-name
                                 dir-name
                                 dir-name
                                 dir-name)
                         "*Default yasnippets installation*"))
  (add-to-list 'yas-snippet-dirs (concat "~/.emacs.d/yasnippets/"
                                         dir-name)))

(defun ar/yas-install-external-yasnippets ()
  "Install external yasnippets."
  (interactive)
  (ar/yas-install--github-yasnippets "https://github.com/AndreaCrotti/yasnippet-snippets.git"
                                    "yasnippet-snippets"))

(provide 'ar-yas)

;;; ar-yas.el ends here

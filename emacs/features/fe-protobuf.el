;;; -*- lexical-binding: t; -*-
(use-package protobuf-mode
  :ensure t
  :mode (("\\.proto\\'" . protobuf-mode))
  :hook ((protobuf-mode . goto-address-prog-mode)
         (protobuf-mode . ar/protobuf-setup-imenu))
  :config
  (defun ar/protobuf-setup-imenu ()
    (setq-local imenu-generic-expression
                '((nil "^[[:space:]]*message[[:space:]]+[[:alpha:]][[:alnum:]_]+" 0)
                  (nil "^[[:space:]]*enum[[:space:]]+[[:alpha:]][[:alnum:]_]+" 0)
                  (nil "^[[:space:]]*service[[:space:]]+[[:alpha:]][[:alnum:]_]+" 0)))))

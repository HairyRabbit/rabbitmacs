;; -*- mode: emacs-lisp -*-
;; -*- coding: utf-8 -*-

(defconst buffer-name-text "%b")

(defun rabbit/buffer-name ()
  "\
Main call.
"
  (propertize buffer-name-text
	      'face (list :weight 'bold)))

(rabbit/buffer-name)
(provide 'mode-line-buffer-name)

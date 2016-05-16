;; -*- mode: emacs-lisp -*-
;; -*- coding: utf-8 -*-

(defconst buffer-size-text "%I")
(defconst buffer-size-color "#ddd")

(defun rabbit/buffer-size ()
  "\
Main call.
"
  (propertize buffer-size-text 'face (list :foreground buffer-size-color
					   :family "Crystal"
					   :height 160))
  )

(rabbit/buffer-size)

(provide 'mode-line-buffer-size)

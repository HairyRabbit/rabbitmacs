;; -*- mode: emacs-lisp -*-
;; -*- coding: utf-8 -*-

(require 'dash)
(require 's)
(require 'mode-line-buffer-status)
(require 'mode-line-buffer-size)
(require 'mode-line-buffer-name)
(require 'mode-line-major-mode)

;;(defvar fill-num t)

(defun rabbit/make-format ()
  (let ((state (list buffer-read-only
		     (buffer-modified-p))))
    
    (list (s-pad-left 4 " " (rabbit/buffer-size)) ;; buffer size
	  (s-center 3 (rabbit/buffer-state state)) ;; buffer state
	  (rabbit/buffer-name) ;; buffer name
	  (s-pad-left 3 " "  (rabbit/major-mode)) ;; major-mode
	  )
    )
  )

(rabbit/make-format)


(defun rabbit/mode-line-format (format-func)
  (setq mode-line-format
	'("%e"
	  (:eval
	   (format-func)
	   ))
	)
  )

(set-face-attribute 'mode-line nil
		    :background "#000F14"
		    :foreground "#d5d5ff"
		    :box nil
		    :width 'expanded
		    :weight 'normal
		    :height 100
		    )
(set-background-color "#000f14")
(set-cursor-color "#00f")

(rabbit/mode-line-format 'rabbit/mark-format)
;;(setq fill-num t)
(print mode-line-process)

(provide 'mode-line-format)

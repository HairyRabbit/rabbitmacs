;; -*- mode: emacs-lisp -*-
;; -*- coding: utf-8 -*-
;; -*- lexical-binding: t -*-

;;; rabbit-mode-line.el custom mode line

;; Copyright (C) 2016-2017

;; Author: Rabbit <yfhj1990@hotmail.com>
;; Version: 0.0.1
;; Keywords: mode line

;;; Code:

(require 'cl)
(require 'dash)
(require 's)

;;(require 'mode-line-buffer-status)
;;(require 'mode-line-buffer-size)
;;(require 'mode-line-buffer-name)
;;(require 'mode-line-major-mode)


(defun rabbit/pick-curr (filter list)
  "\
Filter the list, return the trueth one.

sig :: [(Flag, Data)] -> [(Flag, Data)]
"
  (-filter filter list))


(defun rabbit/pick-data (tuple)
  "\
Pick the tuple data, like snd.

sig :: [(Flag, Data)] -> [Data]
"
  (-map 'cadr tuple))


(defun rabbit/with-face (str faces)
  "\
Make a propertize. The first arg is content, a String.
The Second param is a face list.

sig :: String -> [Face]

@Todo 
Convert from css config file.
"
  (propertize str 'face faces))


(defun rabbit/to-prop (proc data)
  "\
Convert data list to props. the first param is a function
that process data.

sig :: [Data] -> Prop
"
  (apply 'rabbit/with-face (funcall proc data)))



;; Buffer Status

(defvar rabbit/color-buffer-status-readonly "#e85085")
(defvar rabbit/color-buffer-status-modified "#e9a621")
(defvar rabbit/color-buffer-status-saved "#a1779b")
(defvar rabbit/content-buffer-status "\u25cf")

(defun rabbit/buffer-status ()
  "\
Define buffer status, Its a (flag data) tuple.

* 0 - readonly
* 1 - modified
* 2 - saved
"
  (list (list 0 rabbit/color-buffer-status-readonly)
	(list 1 rabbit/color-buffer-status-modified)
	(list 2 rabbit/color-buffer-status-saved)))


(defun rabbit/to-buffer-status-flag ()
  "\
Get buffer status. Get readonly from buffer-raad-only. 
Get saved from (buffer-modified-p). Return the flag 
value.
"
  (let ((readed? buffer-read-only)
	(saved? (buffer-modified-p)))
    (if readed? 0
      (if saved? 2
	1))))


(defun rabbit/filter-buffer-status (n)
  "\
Test flag is eq with every list. Return boolean used
for filter func call.
"
  (= (rabbit/to-buffer-status-flag)
     (-first-item n)))


(defun rabbit/make-buffer-status-prop (n)
  "\
Marge content and face datas, return a props.
"
  (list rabbit/content-buffer-status
	(list :foreground (car n))))


(defun rabbit/format-buffer-status (prop)
  "\
Padding whitespace. like ' * '.
"
  (s-center 3 prop))


(defun rabbit/render-buffer-status-format ()
  "\
Render buffer status.
"
  (->> (rabbit/buffer-status)
       (rabbit/pick-curr 'rabbit/filter-buffer-status)
       (rabbit/pick-data)
       (rabbit/to-prop 'rabbit/make-buffer-status-prop)
       (rabbit/format-buffer-status)))


;; Buffer Size.

(defvar rabbit/content-buffer-size "%I")
(defvar rabbit/color-buffer-size "#fff")

(defun rabbit/buffer-size ()
  "\
Define buffer size data, contianer the color.
"
  (list rabbit/color-buffer-size))

(defun rabbit/make-buffer-size-prop (n)
  "\
Merge buffer size content and data, produce a prop.
"
  (list rabbit/content-buffer-size
	(list :foreground (car n)
	      :family "Crystal"
	      :height 160)))

(defun rabbit/format-buffer-size (prop)
  "\
Padding whitespace. like '10.1k'.
"
  (s-pad-left 4 " " prop))

(defun rabbit/render-buffer-size-format ()
  "\
Render buffer size.
"
  (->> (rabbit/buffer-size)
       (rabbit/to-prop 'rabbit/make-buffer-size-prop)
       (rabbit/format-buffer-size)))


;; Major Mode and Minor Mode















(defun rabbit/make-format ()
  (let ((state (list buffer-read-only
		     (buffer-modified-p))))
    
    (list (rabbit/render-buffer-size-format)
	  (rabbit/render-buffer-status-format)
	  ;;(rabbit/buffer-name) ;; buffer name
	  ;;(s-pad-left 3 " "  (rabbit/make-mode-prop)) ;; major-mode
	  )
    )
  )



(defun rabbit/make-mode-line-format ()
  (setq mode-line-format
	'("%e" (:eval (rabbit/make-format)))
	)
  )

(set-face-attribute 'mode-line nil
		    :background "#000F14"
		    :foreground "#d5d5ff"
		    :box nil
		    :height 100
		    )




(set-background-color "#000f14")
(set-cursor-color "#81618A")
(set-face-attribute 'font-lock-function-name-face nil
		    :foreground "#d5d5ff"
		    :weight 'bold
		    ;; pink "#81618A"
		    )
(set-face-attribute 'font-lock-keyword-face nil
		    :foreground "#81618A"
		    ;;:weight 'bold
		    ) ;;#2B687B ;;#BFF8FF
(set-face-attribute 'font-lock-comment-face nil
		    :foreground "#888"
		    :background nil)
(set-face-attribute 'font-lock-comment-delimiter-face nil
		    :foreground "#555"
		    :background nil)
(set-face-attribute 'font-lock-variable-name-face nil
		    :foreground "#d5d5ff"
		    )
(set-face-attribute 'font-lock-string-face nil
		    :foreground "#e85085" ;;"#e85085" ;; "#BC4545"
		    ;;:height 136
		    ;; pink "#81618A"
		    )
(set-face-attribute 'font-lock-builtin-face nil
		    :foreground "#B97CF9"
		    )

(set-face-attribute 'font-lock-constant-face nil
		    :foreground "#9947EF"
		    )

(set-face-attribute 'font-lock-type-face nil
		    :foreground "#d5d5ff"
		    )

(set-face-attribute 'font-lock-warning-face nil
		    :foreground "#d5d5ff"
		    )



(rabbit/make-mode-line-format)
;;(setq fill-num t)
;;(print mode-line-process)

(provide 'mode-line-format)

;;; rabbit-mode-line ends here

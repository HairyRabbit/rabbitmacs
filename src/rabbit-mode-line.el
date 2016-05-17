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
  (list '(0 rabbit/color-buffer-status-readonly)
	'(1 rabbit/color-buffer-status-modified)
	'(2 rabbit/color-buffer-status-saved)))


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

(defun rabbit/render-buffer-size-format ()
  "\
"
  
  )













(defun rabbit/make-format ()
  (let ((state (list buffer-read-only
		     (buffer-modified-p))))
    
    (list (s-pad-left 4 " " (rabbit/buffer-size)) ;; buffer size
	  (rabbit/render-buffer-status-format) ;; buffer state
	  (rabbit/buffer-name) ;; buffer name
	  (s-pad-left 3 " "  (rabbit/make-mode-prop)) ;; major-mode
	  )
    )
  )



(defun rabbit/make-mode-line-format ()
  (setq mode-line-format
	'("%e"
	  (:eval
	   (rabbit/make-format)
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

(rabbit/make-mode-line-format)
;;(setq fill-num t)
;;(print mode-line-process)

(provide 'mode-line-format)

;;; rabbit-mode-line ends here

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

;; Utils.

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


;; Buffer Name.

(defvar rabbit/content-buffer-name "%b")


(defun rabbit/buffer-name ()
  "\
Buffer name datas. Its nothing.
"
  '())


(defun rabbit/make-buffer-name-prop (n)
  "\
Merge content and props.
"
  (list rabbit/content-buffer-name '(:weight 'bold)))


(defun rabbit/format-buffer-name (prop)
  "\
Format buffer name content.
"
  prop)


(defun rabbit/render-buffer-name-format ()
  "\
Render buffer name.
"
  (->> (rabbit/buffer-name)
       (rabbit/to-prop 'rabbit/make-buffer-name-prop)
       (rabbit/format-buffer-name)
       ))


;; Major Mode.

(defvar rabbit/lang-js '("\ue60a" "#7e58b5"))
(defvar rabbit/lang-elisp '("\ue60a" "#7e58b5"))

(defun rabbit/major-mode ()
  (list (list '("js") rabbit/lang-js)
	(list '("Emacs-Lisp" "Lisp Interaction") rabbit/lang-elisp)
	))


(defun rabbit/filter-mode (n)
  (-contains? (-first-item n) mode-name))


(defun rabbit/make-major-mode-prop (n)
  (let ((data (car n)))
    (list (car data)
	  (list :foreground (cadr data)
		:family "Iconfont"
		:height 200
 		))))


(defun rabbit/format-major-mode (prop)
  (s-pad-left 3 " " prop))


(defun rabbit/render-major-mode-format ()
  (->> (rabbit/major-mode)
       (rabbit/pick-curr 'rabbit/filter-mode)
       (rabbit/pick-data)
       (rabbit/to-prop 'rabbit/make-major-mode-prop)
       (rabbit/format-major-mode)
       ))


;; Minor Mode.

(defvar rabbit/content-minor-mode-yasnippet '(yas-minor-mode ("\ue60c" "#00bfff")))
(defvar rabbit/content-minor-mode-rainbow '(rainbow-mode ("\ue60b" "#ffa500")))


(defun rabbit/minor-mode ()
  "\
Defind replaced contents.
"
  (list rabbit/content-minor-mode-yasnippet
	rabbit/content-minor-mode-rainbow
	))


(defun rabbit/make-minor-mode-prop (prop)
  "\
Merge content and faces.
"
  (let ((data (car prop)))
  (list (car data)
	(list :foreground (cadr data)
	      :family "Iconfont"
	      :height 120))
  ))


(defun rabbit/format-minor-mode (prop)
  "\
Padding left and right.
"
  (s-center 2 prop))


(defun rabbit/replace-minor-display (pair)
  "\
Replace contents.
"
  (let* ((name (car pair))
	 (res (assoc-default name (rabbit/minor-mode))))
    (if res (list name
		  (->> res
		       (rabbit/to-prop 'rabbit/make-minor-mode-prop)
		       (rabbit/format-minor-mode)))
      pair)))


(defun rabbit/render-minor-mode-format ()
  "\
Render minor mode content.
"
  (-map 'rabbit/replace-minor-display minor-mode-alist))



;; Date & Time.
;; Page Scroller.

(defun rabbit/render-scroller-format ()
  "\
"
  (concat "  "
          "{%l, %c} "
          "\u1d16\u1d25\u1d16 "
          "\u25a0\u25a0\u25a0\u25a0\u25a0\u25a0\u25ba"
          " %p"
          )
)








;; Main Call.

(defun rabbit/render-content ()
  "\
Concat all contents.
"
  (list (rabbit/render-buffer-size-format)
	(rabbit/render-buffer-status-format)
	(rabbit/render-buffer-name-format)
	(rabbit/render-major-mode-format)
	(rabbit/render-minor-mode-format)
        (rabbit/render-scroller-format)
	))


(defun rabbit/main-mode-line ()
  "\
Change mode line format.
"
  (interactive)
  (setq mode-line-format
	'("%e" (:eval (rabbit/render-content)))))

(rabbit/main-mode-line)






(provide 'rabbit-mode-line)

;;; rabbit-mode-line.el ends here

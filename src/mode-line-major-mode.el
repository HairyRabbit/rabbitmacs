;; -*- mode: emacs-lisp -*-
;; -*- coding: utf-8 -*-

(require 'cl)
(require 'dash)

(defconst langs-icon '((("js") "\ue60a")
		       (("Emacs-Lisp" "Lisp Interaction") "\ue60a")
		       )
  ""
  )


(defvar rabbit-major-mode-size 200 "")
(defvar rabbit-minor-mode-size 150 "")

(defun rabbit/make-mode-icon (size)
  (lexical-let ((size size))
    (lambda (str icon)
      (let ((face (list :foreground icon
			:family "Iconfont"
			:height size)))
	(propertize str 'face face)))))


(defun rabbit/make-major-mode-icon (size icon)
  (funcall (rabbit/make-mode-icon rabbit-major-mode-size) size icon))


(defun rabbit/make-minor-mode-icon (str icon)
  (funcall (rabbit/make-mode-icon rabbit-minor-mode-size) size icon))


(defun rabbit/make-mode-prop ()
  (->> mode-name
       (rabbit/mam/pick-langs langs-icon)
       (rabbit/mam/pick-ctx)
       (rabbit/mam/to-prop)
       ))


(defun rabbit/mam/pick-langs (langs name)
  (-filter (lambda (x)
	     (-contains? (-first-item x) name))
	   langs))

(defun rabbit/mam/pick-ctx (picked)
  (-map '-last-item picked)
  )

(defun rabbit/mam/to-prop (ctxs)
  (let ((ctx (-first-item ctxs)))
    (rabbit/make-major-mode-icon "\ue60a" "#7e58b5")
    ))

(provide 'mode-line-major-mode)

;; -*- mode: emacs-lisp -*-
;; -*- coding: utf-8 -*-

(require 'dash)

(defconst langs-icon '((("js") "\ue608")
		       (("Emacs-Lisp" "Lisp Interaction") "\ue608")
		       )
  ""
  )


(defun rabbit/major-mode ()
  "\
Main call.
"
  (->> mode-name
       (rabbit/mam/pick-langs langs-icon)
       (rabbit/mam/pick-ctx)
       (rabbit/mam/to-prop)
       )
  ;;(propertize buffer-name-text 'face (list :weight 'bold))
  )


;;(rabbit/major-mode)

(defun rabbit/mam/pick-langs (langs name)
  (-filter (lambda (x)
	     (-contains? (-first-item x) name))
	   langs))

(defun rabbit/mam/pick-ctx (picked)
  (-map '-last-item picked)
  )

(defun rabbit/mam/to-prop (ctxs)
  (let ((ctx (-first-item ctxs)))
    (propertize ctx 'face (list :foreground "#BFAB68"
				:family "Iconfont"
				:height 300))))


(provide 'mode-line-major-mode)

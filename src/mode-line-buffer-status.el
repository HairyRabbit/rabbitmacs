;; -*- mode: emacs-lisp -*-
;; -*- coding: utf-8 -*-
;;;
;;;
;;;
;; Package-Requires: ((dash "2.12.1"))

(require 'dash)


(defconst buffer-readonly '(0 "#e85085") "Red.")
(defconst buffer-modified '(1 "#e9a621") "yellow.")
(defconst buffer-saved '(2 "#a1779b") "pink.")
(defconst buffer-state-icon "\u25cf" "a circle.")

;;;###autoload
(defun rabbit/buffer-state (status)
  "\
Main call.

Convert buffer status to prop font face.
"
  (let ((colors (list buffer-readonly
		      buffer-modified
		      buffer-saved))
	(icon buffer-state-icon))
    (->> status
	 (to-flag)
	 (pick-state colors)
	 (to-color)
	 (to-prop icon))))

;; Test
;; (buffer-state '(t t))


(defun to-flag (status)
  "\
sig :: List Status -> Flag
"
  (let ((readed? (-first-item status))
	(saved?  (-last-item status)))
    (if readed? 0
      (if saved? 2
	1))))


(defun pick-state (config flag)
  "\
sig :: Flag -> List (Flag, Color)
"
  (-filter (lambda (x)
	     (= flag (-first-item x)))
	   config))


(defun to-color (pairs)
  "\
sig :: List (Flag, Color) -> List Color
"
  (-map '-last-item pairs))


(defun to-prop (str colors)
  "\
sig :: List Color -> List Prop
"
  (let ((color (-first-item colors)))
    (propertize str 'face (list :foreground color))))



(provide 'mode-line-buffer-status)

;;; mode-line-buffer-status.el ends here

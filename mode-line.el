(defmacro defcontent (ctx &rest props)
  "\

Propertize text face. 

"
  `(propertize ,ctx 'face ,@props))


(defun buffer-status ()
  "\

Buffer Modified.

buffer-read-only define the buffer readable
buffer-modified-p get buffer is modified

"

  (defcontent "\u25cf"
    (let ((modi? (buffer-modified-p))
	  (read? buffer-read-only))
      
      (if read?
	  ;; red
	  (list :foreground "#e85085")
	(if modi?
	  ;; yellow
	  (list :foreground "#E9A621")
	  ;; purple
	  (list :foreground "#a1779b"))))))


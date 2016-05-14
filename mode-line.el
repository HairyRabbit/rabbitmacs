(setq header-line-format
      (concat (propertize " " 'display '((space :align-to 0)))
              ""))

(defmacro with-face (str &rest properties)                                                                     
  `(propertize ,str 'face (list ,@properties)))                                                              
(setq mode-line-format
      (list
       ;; value of `mode-name'
       "%m: "
       ;; value of current buffer name
       "%b, "
       ;;
       "%m, %[ %] "
       ;; value of current line number
       "|%l%%%c| "
       "-- user: "
       ;;(insert-image (create-image "f:/mE/Frontend/Image/Icon/android-icon.svg"))
       (with-face "\ue603"
		  ;; :background "green"
		  :foreground "#BFAB68"
		  :family "Iconfont"
		  :weight 'bold
		  :height 200
                  ;;:foreground "#81b280"
                  )
       (with-face "\ue607"
		  ;; :background "green"
		  :foreground "#6C2B23"
		  :family "Iconfont"
		  :weight 'bold
		  :height 200
                  ;;:foreground "#81b280"
                  )
       (with-face "\ue606"
		  ;; :background "green"
		  :foreground "#555555"
		  :family "Iconfont"
		  :weight 'bold
		  :height 200
                  ;;:foreground "#81b280"
                  )
       (with-face "\ue605"
		  ;; :background "green"
		  :foreground "#F588AF"
		  :family "Iconfont"
		  :weight 'bold
		  :height 200
                  ;;:foreground "#81b280"
                  )
       "-- user: "
       ;; value of user
       (getenv "USER")))


(set-face-attribute 'mode-line nil
		    :background "#101010"
		    :foreground "#337399"
		    :box nil
		    )

(set-background-color "#101010")


;; #1d2025
;; #20242d

;; builds another, simpler graph
(defun graph-1()
	'((A (B C E))
	(B (A E F))
	(C (A E F))
	(D (F))
	(E (A B C F))
	(F (B C D E))))

;;returns only the connection for a vertex
(defun get-connections (vertex vertices vertices-and-their-edges)
	(let (connections (list)))
	(setq connections (nth 1 (nth (position vertex vertices) vertices-and-their-edges)))
	connections
)

;;takes in something like (FL AZ OK NJ)
;;returns something like ((FL r) (AZ g) (OK b) (NJ r))
;;available for use: function (colors) which returns list (r b g y), get-edges which returns edges for any vertex
(defun color-minimally (vertices graph)
	(let (no-r-list (list))
	(no-b-list (list))
	(no-g-list (list))
	(no-y-list (list))
	(vertex-edges (get-edges vertices graph))
	(final-list (list))
	(dolist (i vertices)
		;;color stuff and add to the final list
		(cond
			((equal NIL (find i no-r-list)) (progn(setq final-list (append final-list (list (append (list i)(list (position 0 (colors)))))));;if not in the no-red list, color state red
												(setq no-r-list(append no-r-list (get-connections i vertices vertex-edges)));;add all its edges into the no-red list
											))	
			((equal NIL (find i no-b-list)) (progn(setq final-list (append final-list (list (append (list i)(list (position 1 (colors)))))));;if not in the no-blue list, color state blue
												(setq no-b-list(append no-b-list (get-connections i vertices vertex-edges)));;add all its edges into the no-blue list
											))	
			((equal NIL (find i no-g-list)) (progn(setq final-list (append final-list (list (append (list i)(list (position 2 (colors)))))));;if not in the no-green list, color state green
												(setq no-g-list(append no-g-list (get-connections i vertices vertex-edges)));;add all its edges into the no-green list
											))	
			(t (progn(setq final-list (append final-list (list (append (list i)(list (position 3 (colors)))))));;if not in the no-yellow list, color state yellow
												(setq no-y-list(append no-y-list (get-connections i vertices vertex-edges)));;add all its edges into the no-yellow list
											))
		)
	)
	(setq final-list (remove nil final-list))
	final-list
	)
)

;;(cond
  ;; ((evenp a) a)	;if a is even return a
   ;;((> a 7) (/ a 2))	;else if a is bigger than 7 return a/2
   ;;((< a 5) (- a 1))	;else if a is smaller than 5 return a-1
   ;;(t 17))		;else return 17
   
;;(cond
  ;; ((equal NIL (find i no-r-list)) (progn(setq final-list (append final-list (list (append (list i)(list (position 0 (colors)))))));;if not in the no-red list, color state red
	;;									(setq no-r-list(append no-r-list (get-connections i vertices vertex-edges)));;add all its edges into the no-red list
		;;							))	
  ;; ((equal NIL (find i no-b-list)) (progn(setq final-list (append final-list (list (append (list i)(list (position 1 (colors)))))));;if not in the no-blue list, color state blue
	;;									(setq no-b-list(append no-b-list (get-connections i vertices vertex-edges)));;add all its edges into the no-blue list
		;;							))	
   ;;((equal NIL (find i no-g-list)) (progn(setq final-list (append final-list (list (append (list i)(list (position 2 (colors)))))));;if not in the no-green list, color state green
	;;									(setq no-g-list(append no-g-list (get-connections i vertices vertex-edges)));;add all its edges into the no-green list
		;;							))	
   ;;(t (progn(setq final-list (append final-list (list (append (list i)(list (position 2 (colors)))))));;if not in the no-yellow list, color state yellow
	;;									(setq no-g-list(append no-g-list (get-connections i vertices vertex-edges)));;add all its edges into the no-yellow list
		;;							))
;;)

(let ((x 4) (y 5)) (print (+ x y)))

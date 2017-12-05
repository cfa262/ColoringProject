;;lst would be in the cutset form ((A R) (E G))
(defun color-non-cutset(lst graph)
	(let ((finalList lst)(orderedList '())(coloredList '())(remainList '())(verticesList '()))

		;#1 creates a remainder list of non-cutset vertices to be ordered
		(defun get-new-list (lst graph)
			(setq verticesList '())
			(dolist (i lst)
				(setq verticesList (append verticesList (list (car i)))))
			(setq remainList (get-remaining verticesList graph))	
			(order-tree-vertices remainList))

		;#2 orders non-cutset vertices tree
		(defun order-tree-vertices (lst)		
			;;gets the highest deg of the non-cutset and colors it first
			(setq orderedList (list (get-high-deg lst)))
			(color-vertex orderedList))

		;#3 randomly color first vertex given by ordered tree 
		(defun color-vertex (lst)		
			(setq coloredList lst)
			;;loops and find the vertex to color
			(dolist (x (get-possible-colors finalList graph))
				(cond ((equal lst (list (car x)))
					;colors the vertex --> with first colored index
					(setq coloredList (append coloredList (list (car (car (cdr (cdr x))))))))))
			(cond ((not(equal nil (car coloredList)))
				(setq finalList (append finalList (list coloredList)))
				;;loops back to #1 with new list of colored vertices
				(get-new-list finalList graph))))
		(get-new-list lst graph)
		finalList))

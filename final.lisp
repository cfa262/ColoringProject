;; ---------------------
;; Calvin Alvarez
;; Veronica Balabanova
;; Andrew Morato
;; Map Coloring Project
;; December 6, 2017
;; ---------------------

;; NOTE: The lines immediately above each function definition
;;       contain the name of its author.
;;       There is a guide at the bottom of this document that
;;       describes some functions and how to run them


;; takes a graph an returns a list of coloring assignments
;; Author: Calvin Alvarez
(defun color-graph (lst)
	(color-non-cutset (color-minimally (get-cutset lst) lst) lst))


;;-------------------------------------------------------------------
;;                         Cutset Functions
;;-------------------------------------------------------------------

;; returns the cutset of the graph 'lst', pseudo code:
;; update graph (rm all 0-1 degree vertices)
;; while graph is not empty
	;; v = vertex w/highest degree
	;; add v to cutset list
	;; rm v from graph
	;; update graph
;; return cutset list
;; Author: Andrew Morato
(defun get-cutset(lst)
	(defun update-graph(lst) 
		(let ((updated (rm-vertices (get-0-1-edges lst) lst)))
			(cond ((eql (get-0-1-edges updated) NIL) updated) 
				(t (update-graph updated)))))
	(defun cutset(lst) 
		(cond ((> (length lst) 0)
			(let ((v (get-high-deg lst))) (cons v 
		 		(get-cutset (update-graph (rm-vertex v lst))))))
		    (t lst)))
	(cutset (update-graph lst)))


;; removes all occurences of a vertex from lst
;; Author: Veronica Balabanova
(defun rm-vertex(vertex lst)
	;; removes a vertex from a list if present
	(defun rm-edge (vertex sublst)
		(cond((null sublst) sublst)
			(t (if (equal vertex (first sublst)) 
				(remove vertex sublst)
				;; else
			    (cons (first sublst) 
			    	(rm-edge vertex (rest sublst)))))))
	;; removes list that begin with vertex
	(setq lst (remove vertex lst :key #'first))
	(cond ((> (length lst) 0) 
		(cons (cons (car (car lst))
			  (list (rm-edge vertex (car (cdr (car lst))))))
			(rm-vertex vertex (cdr lst))))
	  (t lst)))


;; removes all vertices in a given states list from list lst
;; Author: Veronica Balabanova
(defun rm-vertices(vertices lst)
	(dolist (i vertices)
		(setq lst (rm-vertex i lst)))
	lst)


;; returns the degree of the first element of lst
;; Author: Andrew Morato
(defun get-degree(lst)
	(cond ((> (length lst) 0) 
		(length (car (cdr (car lst))))) (t -1)))


;; returns the vertex with the highest degree
;; Author: Andrew Morato
(defun get-high-deg(lst)
	(defun find-highest(vertex high lst)
		(let ((degree (get-degree lst))) 
			(cond ((= degree -1) vertex)
				(t (cond ((> degree high) 
					 (find-highest (car (car lst)) degree (cdr lst)))
					 (t (find-highest vertex high (cdr lst))))))))
   (find-highest (car (car lst)) (length (car (cdr (car lst)))) lst))


;; gets a list of all vertices with 0 or 1 edges
;; Author: Calvin Alvarez
(defun get-0-1-edges(lst)
	(cond ((> (length lst) 0)
		(cond ((> 2 (get-degree lst)) 
		 (cons (car (car lst)) (get-0-1-edges (cdr lst))))
			(t (get-0-1-edges (cdr lst)))))
(t lst)))


;;-------------------------------------------------------------------
;;                        Coloring Functions
;;-------------------------------------------------------------------

;; takes the cutset list and returns the graph without the cutset
;; vertices but with the cutset vertices as edges for other vertices.
;; Author: Andrew Morato
(defun get-remaining(lst graph) 
	(defun is-cutset(lst v)
		(cond ((> (length lst) 0)
			(cond ((eql (car lst) (car v)) t)
				(t (is-cutset (cdr lst) v))))
			(t NIL)))
	(cond ((> (length graph) 0)
		(cond ((is-cutset lst (car graph))
			(get-remaining lst (cdr graph)))
			(t (cons (car graph) (get-remaining lst (cdr graph))))))
		(t graph)))


;; takes a list of vertices and a graph and returns the list of
;; vertices with their edges extracted from the entire graph.
;; Author: Andrew Morato
(defun get-edges(lst graph)
	;; takes a vertex and a graph and returns it with its edges
	(defun get-edge-group(v graph)
		(cond ((> (length graph) 0)
			(cond ((eql v (car (car graph)))
				(cons v (list (car (cdr (car graph))))))
		    	(t (get-edge-group v (cdr graph)))))))
	(cond ((> (length lst) 1)
			(cons (get-edge-group (car lst) graph) 
				  (get-edges (cdr lst) graph)))
		    (t (list (get-edge-group (car lst) graph)))))


;; takes in something like (FL AZ OK NJ)
;; returns something like ((FL r) (AZ g) (OK b) (NJ r))
;; available for use: function (colors) which returns list
;; (r b g y), get-edges which returns edges for any vertex
;; this function colors the cutset minimally
;; Author: Veronica Balabanova
(defun color-minimally (vertices graph)
	;;returns only the connection for a vertex
	(defun get-connections (vertex vertices vertices-and-their-edges)
		(let (connections (list)))
		(setq connections (nth 1 (nth (position vertex vertices)
			vertices-and-their-edges)))
		connections)
	(let ((no-r-list (list))
		 (no-b-list (list))
		 (no-g-list (list))
		 (no-y-list (list))
		 (vertex-edges (get-edges vertices graph))
		 (final-list (list)))
	;;color stuff and add to the final list
	(dolist (i vertices)
		(cond ((equal NIL (find i no-r-list)) 
			(progn (setq final-list 
				(append final-list (list 
					;;if not in the no-red list, color state red
					(append (list i)(list 'r)))))
			(setq no-r-list (append no-r-list 
				;;add all its edges into the no-red list
				(get-connections i vertices vertex-edges)))))	
		((equal NIL (find i no-b-list))
			(progn (setq final-list 
				(append final-list (list 
					;;if not in the no-blue list, color state blue
					(append (list i)(list 'b)))))
			(setq no-b-list (append no-b-list
				;;add all its edges into the no-blue list
				(get-connections i vertices vertex-edges)))))	
		((equal NIL (find i no-g-list))
			(progn (setq final-list 
				(append final-list (list 
					;;if not in the no-green list, color state green
					(append (list i)(list 'g)))))
			(setq no-g-list (append no-g-list
				;;add all its edges into the no-green list
				(get-connections i vertices vertex-edges)))))	
		(t  
			(progn (setq final-list 
				(append final-list (list 
					;;if not in the no-yellow list, color yellow
					(append (list i)(list 'y)))))
			(setq no-y-list (append no-y-list 
				;;add all its edges into the no-yellow list
				(get-connections i vertices vertex-edges)))))))

	(setq final-list (remove nil final-list))
	final-list))


;; takes the cutset and its colors, of the form ((X R) (Y B) etc), &
;; the graph and returns possible colors for the remaining vertices.
;; Author: Andrew Morato
(defun get-possible-colors(lst graph)
	;; fills the graph will all color options
	(defun fill-graph(graph)
		(cond ((> (length graph) 0)
			(cons (append (car graph) (list (colors))) 
				  (fill-graph (cdr graph))))
			(t graph)))
	;; rm's elements in list rm from lst
	(defun rm-lst(rm lst) 
		(cond ((and (> (length lst) 0) (> (length rm) 0))
			(rm-lst (cdr rm) (remove (car rm) lst)))
			(t lst)))
	(defun has-edge(edge v)
		(defun has-e(edge v)
			(cond ((> (length v) 0) 
				(cond ((eql edge (car v)) t)
					(t (has-e edge (cdr v)))))
				(t NIL)))
			(has-e edge (car (cdr v))))
	;; given a colored cutset list, this returns a {v, e, c}
	;; without any conflicting colors
	(defun rm-colors(lst v)
		;; removes a color in {v, e, c} and its corresponding vertex
		(defun rm-color(edge color v)
			;; removes an e from a {v, e, c}
			(defun rm-edge(edge v) (remove edge (car (cdr v))))
			(cons (car v) (cons (rm-edge edge v) 
				  (list (remove color (car (cdr (cdr v))))))))
		(cond ((> (length lst) 0)
			(cond ((has-edge (car (car lst)) v)
				(rm-colors (cdr lst) 
					(rm-color (car (car lst)) (nth 1 (car lst)) v)))
				(t (rm-colors (cdr lst) v))))
			(t v)))
	;; runs rm-colors for every {v, e, c} in the graph
	(defun get-remaining-colors(lst graph)
		(cond ((> (length graph) 0) 
			(cons (rm-colors lst (car graph))
				  (get-remaining-colors lst (cdr graph))))
			(t graph)))
		(get-remaining-colors lst (fill-graph graph)))


;; lst would be in the cutset form '((A R) (E G))
;; Author: Calvin Alvarez
(defun color-non-cutset(lst graph)
	(let 
		((finalList lst)
		(orderedList '())
		(coloredList '())
		(remainList '())
		(verticesList '()))

		;creates a list of non-cutset vertices to be ordered
		(defun get-new-list (lst graph)
			(setq verticesList '())
			(dolist (i lst)
				(setq verticesList 
					(append verticesList (list (car i)))))
			(setq remainList (get-remaining verticesList graph))	
			(order-tree-vertices remainList))

		;orders non-cutset vertices tree
		(defun order-tree-vertices (lst)		
		 ;;gets the highest deg of the non-cutset and colors it first
			(setq orderedList (list (get-high-deg lst)))
			(color-vertex orderedList))

		;randomly colors first vertex given by ordered tree 
		(defun color-vertex (lst)		
			(setq coloredList lst)
			;;loops and find the vertex to color
			(dolist (x (get-possible-colors finalList graph))
				(cond ((equal lst (list (car x)))
					;randomly colors the vertex
					(setq coloredList (append coloredList 
						(list (car (car (cdr (cdr x))))))))))
			(cond ((not(equal nil (car coloredList)))
			   (setq finalList (append finalList (list coloredList)))
				;;loops back to top with new list of colored vertices
				(get-new-list finalList graph))))
		(get-new-list lst graph)
finalList))

;;-------------------------------------------------------------------
;;                     Testing Functions/Graphs
;;-------------------------------------------------------------------

;; builds the states graph
(defun states()
	'((AL (GA FL MS TN))
	(AK ())
	(AZ (CA NV UT CO NM))
	(AR (TX OK MO TN MS LA))
	(CA (OR NV AZ))
	(CO (NM AZ UT WY NE KS OK))
	(CT (RI NY MA))
	(DE (MD PA NJ))
	(DC (MD VA))
	(FL (GA AL))
	(GA (SC NC TN AL FL))
	(HI ())
	(ID (WA OR NV UT WY MT))
	(IL (WI IA MO KY IN))
	(IN (IL KY OH MI))
	(IA (MN SD NE MO IL WI))
	(KS (CO OK MO NE))
	(KY (MO TN VA WV OH IN IL))
	(LA (TX AR MS))
	(ME (NH))
	(MD (DE PA WV DC VA))
	(MA (RI CT NY VT NH))
	(MI (OH IN WI))
	(MN (WI IA SD ND))
	(MS (LA AR TN AL))
	(MO (KS NE IA IL KY TN AR OK))
	(MT (ID WY SD ND))
	(NE (WY SD IA MO KS CO))
 	(NV (CA OR ID UT AZ))
	(NH (ME MA VT))
	(NJ (NY PA DE))
	(NM (AZ UT CO OK TX))
	(NY (PA NJ CT MA VT))
	(NC (VA TN GA SC))
	(ND (MT SD MN))
	(OH (PA WV KY IN MI))
	(OK (TX NM CO KS MO AR))
	(OR (WA ID NV CA))
	(PA (NY NJ DE MD WV OH))
	(RI (CT MA))
	(SC (GA NC))
	(SD (WY MT ND MN IA NE))
	(TN (AR MO KY VA NC GA AL MS))
	(TX (NM OK AR LA))
	(UT (CO NM AZ NV ID WY))
 	(VT (NY MA NH))
	(VA (NC TN KY WV MD DC))
	(WA (ID OR))
 	(WV (KY OH PA MD VA))
	(WI (MN IA  IL MI))
	(WY (ID MT SD NE CO UT))))


;; builds another, simpler graph
(defun graph-1()
	'((A (B C E))
	(B (A E F))
	(C (A E F))
	(D (F))
	(E (A B C F))
	(F (B C D E))))


;; sets the usable colors
(defun colors() '(r b g y))


;; neatly prints the list
(defun print-list(lst)
	(dolist (x lst)
		(print x))
	(values))


;; input takes a colored solution list and a graph to be tested
;; Author: Calvin Alvarez
(defun test-solution (lst graph)
	(dolist (x lst)
		(setf color (cdr x)) ;;sets color to be checked
		(setf element (list (car x))) ;;sets element to find
		(print (list 'vertex element ': 'color 'to 'check color))
		(dolist (y graph);;loops through graph
			;;finds the specific vertex 
			(cond ((equal element (list (car y)))
					(setf el (car (cdr y)));;gets the list of edges
					(print (list 'list 'of 'edges ': el))
					 ;;loops through list of edges the through
					 ;;the saved solution list
					(dolist (z el) (dolist (vertex lst)
						;;finds edge and vertex match	
						(cond ((equal z (car vertex))
							(print 
							(list 'checking 'vertex ': (car vertex)))
							;;checks if there is a color conflict
							(cond ((equal color (cdr vertex))	
								(print 'conflict)) 
							(t (print (list 'picked 'solution
									  'color ': (cdr vertex))) 
							 ;;prints okay if no conflicts	
							(print 'okay)))))))))))		
	(values))

;;-------------------------------------------------------------------
;;                         Documentation
;;-------------------------------------------------------------------

;; This is a list of some functions, what they do, and how to run
;; them. To obtain the graphs (lists of the states/other graphs),
;; call (states) to get the representation of the us and (graph-1)
;; to get a smaller graph 


;; get-cutset(lst)
;;		-takes a graph as lst
;;		-returns its cutset
;; example: (get-cutset (states))


;;	get-high-deg(lst)
;;		-takes a graph
;;		-returns the first element with the highest
;;		 degree (most edges) in the graph
;; example: (get-high-deg (states))


;;	get-remaining(lst graph)
;;		-takes a cutset as lst and a graph
;;		-return the remaining trees
;; example: (get-remaining (get-cutset (states)) (states))


;;	get-edges(lst graph)
;;		-takes a cutset as lst and a graph
;;		-returns a list of the cutset and its edges
;; example: (get-edges (get-cutset (states)) (states))


;;	color-minmally(vertices graph)
;;		-takes a cutset as vertices and a graph
;;		-return a list with the cutset and its color assignments
;;		 (example: ((FL R) (CO B) (AK Y)))
;; example: (color-minimally (get-cutset (states)) (states))


;;	get-possible-colors(lst graph)
;;		-takes a colored cutset list as lst and a graph
;;		-returns a remaining graph with the possible colors
;;		 each vertex can be after removing the colors from the
;;		 bordering elements in the colored cutset list. Essentially
;;		 returns a list of lists like: (FL (AK TX GA) (R B Y))
;; example: (get-possible-colors (color-minimally (get-cutset 
;;          (states)) (states)) (states))


;;	color-non-cutset(lst graph)
;;		-takes a colored cutset list as lst and a graph
;;		-returns color assignments to all the vertices in graph,
;;		 a list of the form ((AK R) (AL Y) (RI G) etc)
;; example: (get-possible-colors (color-minimally (get-cutset 
;;          (states)) (states)) (states)) 


;;	color-graph(graph)
;;		-takes in a graph
;;		-returns color assignments to all the vertices in graph,
;;		 a list of the form ((AK R) (AL Y) (RI G) etc)
;;		-Essentially combines all the functions to solve the
;;		 map-coloring problem
;; example: (color-graph (states))

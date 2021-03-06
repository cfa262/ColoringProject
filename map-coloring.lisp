;; ---------------------
;; Calvin
;; Nika
;; Andy
;; Map Coloring Project
;; December 6, 2017
;; ---------------------

;;-------------------------------------------------------------------
;;                     Testing Graphs/Functions
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

;; returns the degree of the first element of lst
(defun get-degree(lst)
	(cond ((> (length lst) 0) 
		(length (car (cdr (car lst))))) (t -1)))

;; returns the vertex with the highest degree
(defun get-high-deg(lst)
	(defun find-highest(vertex high lst)
		(let ((degree (get-degree lst))) 
			(cond ((= degree -1) vertex)
				(t (cond ((> degree high) 
					 (find-highest (car (car lst)) degree (cdr lst)))
					 (t (find-highest vertex high (cdr lst))))))))
   (find-highest (car (car lst)) (length (car (cdr (car lst)))) lst))


;;-------------------------------------------------------------------
;;                        Coloring Functions
;;-------------------------------------------------------------------

;; takes the cutset list and returns the graph without the cutset
;; vertices but with the cutset vertices as edges for other vertices
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
;; vertices with their edges extracted from the entire graph
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

;; takes the cutset and its colors, of the form ((X R) (Y B) etc), &
;; the graph and returns possible colors for the remaining vertices
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

;;-------------------------------------------------------------------
;;                         Nikas Code
;;-------------------------------------------------------------------

;; removes all occurences of a vertex from lst
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
(defun rm-vertices(vertices lst)
	(dolist (i vertices)
		(setq lst (rm-vertex i lst)))
	lst)

;;-------------------------------------------------------------------
;;                         Calvins Code
;;-------------------------------------------------------------------

;; gets a list of all vertices with 0 or 1 edges
(defun get-0-1-edges(lst)
	(cond ((> (length lst) 0)
		(cond ((> 2 (get-degree lst)) 
		 (cons (car (car lst)) (get-0-1-edges (cdr lst))))
			(t (get-0-1-edges (cdr lst)))))
	  (t lst)))

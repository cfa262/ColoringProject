;; ---------------------
;; Calvin Alvarez
;; Andrew Morato
;; Map Coloring Project
;; December 6, 2017
;; ---------------------

;;-------------------------------------------------------------------
;;                              Graphs
;;-------------------------------------------------------------------

;; build the states graph
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

;;-------------------------------------------------------------------
;;                         Cutset Functions
;;-------------------------------------------------------------------

;; returns the cutset of the graph lst
;;(defun get-cutset(lst)
;;	(defun update-graph(lst) (rm-states (get-0-1-edges lst)))
;;	(let ((graph (update-graph lst)))
;;
;;		)
;;	)

;;returns the vertex with the highest degree
(defun get-high-deg(lst)
	;;returns the degree of the first element of lst
	(defun get-degree(lst)
		(cond ((> (length lst) 0) 
			(length (car (cdr (car lst))))) (t -1)))
	(defun find-highest(high lst)
		(let ((degree (get-degree lst))) 
			(cond ((= degree -1) high)
				(t (cond ((> degree high) 
						(find-highest degree (cdr lst)))
					  (t (find-highest high (cdr lst))))))))
	(find-highest 0 lst))
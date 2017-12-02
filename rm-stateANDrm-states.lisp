;;removes all occurences of a state from list lst
(defun rm-state(state lst)
	(setq lst (remove state lst :key #'first));;removes the sublist that starts with the state to be removed (aka removes the state and all its connections)
	(defun rm-state-helper (state sublst)
		(cond((null sublst) sublst);;terminates if no more elements in sublst
			(t (if (equal state (first sublst))(remove state sublst)
			(append (list (first sublst))(rm-state-helper state (rest sublst)))))
		)
	)
	(dolist (i lst)
		(rm-state-helper state i)
	)
	lst
)

;;removes all states in a given states list from list lst
(defun rm-states(states lst)
	(dolist (i states)
		(rm-state i lst)
	)
)




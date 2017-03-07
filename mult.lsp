(defun c:mult ()
  (setq dcl_id (load_dialog "mult.dcl"))		;load dialog
 
  (if (not (new_dialog "mult" dcl_id))		;test for dialog
  (exit)						;exit if no dialog
  );if

(setq layers (vla-get-layers
(vla-get-activedocument
(vlax-get-acad-object))))

(vlax-for lyr layers
(setq table (cons (vla-get-name lyr) table))
)

(setq table (acad_strlsort table))  ; tri ordre alphabétique
(setq table (Unique table))

;;;(vl-sort table
;;;           (function (lambda (e1 e2)
;;;                       (< (strcase (car e1)) (strcase (car e2)))
;;;                     )
;;;           )
;;;  )

  
;;;  (setq calques (vla-get-layers (vla-get-ActiveDocument (vlax-get-acad-object))))
;;;  (princ "yo")
;;;  (setq table (cons (vla-get-name calques) table))
;;;  ;(princ table)
;;;;;;(vlax-for calque calques
;;;;;;;;;  (vla-put-LayerOn calque :vlax-false)
;;;;;;  
;;;;;;)
  

  ; set_tile
  (start_list "calque")				;start the list box
  (mapcar 'add_list table)				;fill the list box
  (end_list)

  (setq prefixe "prefixe")
  (setq nb "5")

  (set_tile "prefixe" prefixe)
  (set_tile "nb" nb)
  
  ; action_tile
  (action_tile "prefixe" "(setq prefixe $value)")
  (action_tile "nb" "(setq nb $value)")
  (action_tile
  "cancel"						;if cancel button pressed
  "(done_dialog) (setq userclick nil)"		;close dialog, set flag
  );action_tile
 
  (action_tile
    "accept"						;if O.K. pressed
    "(saveVars)(done_dialog)(setq userclick t))"	;close dialog, set flag - fonction saveVars pour sauver les var des popup lists 
  );action tile

  
  (start_dialog)					;start dialog
 
  (unload_dialog dcl_id)				;unload

  (if (not userclick)
    (exit)
  )

  (setq n (atoi nb))
  (if (< n 1)
	(progn
	  (alert "Le nombre de multiplications à effectuer est incorrect, aucun calque ne sera créé")
	  (exit))
    )

(while (> n 0)
  (setq new_lname (strcat prefixe (itoa n) "_" lname))
  (if (not (member new_lname table))
    (progn     ; le calque 'nexiste pas donc on le crée
      (setq laylist (entget (tblobjname "layer" lname)))
      (setq laylist (subst (cons 2 new_lname) (assoc 2 laylist) laylist)) ; Remplace l'association (2 . nom du calque) par le nouveau (2 . new name)
      (entmake laylist)
    )
    )

  (setq n (- n 1))
  )
(princ)
  
) ; fin defun mult

(vl-load-com)

; supprime doublon d'une liste
(defun Unique ( l )
  (if l
    (cons (car l)
      (Unique (vl-remove (car l) (cdr l)))
    )
  )
)

(defun saveVars()
    (setq sStr1(get_tile "calque"))
    (if(= sStr1 "")
    (setq lname nil)
    (setq lname (nth (atoi sStr1) table)) ; sauvegarde du calque sélectionné
    )
)



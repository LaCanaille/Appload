;;; Fonctions proposées
;;;
;;;
;;; vect : crée une flèche vecteur. Il suffit de spécifier le point de début et de fin. La flèche a la taille 0.1 0.1.
;;; peut s'appeler directement en tapant vect (pas besoin de parenthèses)
;;;
;;; vectdim : comme la fonction vect mais on spécifie les dimensions de la flèche. exemple : (vectdim 0.16 0.16) le premier argument est la largeur de la flèche le second la longueur
;;;
;;; vectech : prend en compte une échelle. le vecteur sera de longueur pt1-pt2 * echelle. (vectech 0.1 0.1 3) par exemple
;;;
;;; BR : pour couper un objet en un seul point. sélectionnez l'objet à couper puis cliquer sur le point où couper
;;;
;;; porte : pour dessiner une porte. spécifiez le premier point, le second point, puis cliquez du côté où ouvrir la porte
;;;
;;; spécial pour Mathilde pour la gestion du calque np (pourrait s'appliquer à d'autres calques
;;; gnp : gel ou dégèle le calque np
;;; vnp : verrouille ou déverrouille le calque np 
;;;
;;; repeat_offset : permet de faire un décaler multiple. décaler l'objet selon une liste de distance (mode cumul ou interval)
;;;
;;; T2M - convert individual Texts/Dtexts to individual MTexts
;;;
;;; inc : numérote avec incrémentation les sommets d'une polyligne, avec des chiffres ou des lettres


; vecteur
(defun c:vect ()
  (vectdim 0.3 0.3)
)

(defun vectdim (width dist / pt1 pt2 pt3)
  
    
(if
  (and
    (setq pt1 (getpoint "\nSpécifiez le point de départ: "))
    (setq pt2 (getpoint pt1 "\nSpécifiez le second point: "))
  )

   (progn

     (setq pt3  (polar pt2 (angle pt2 pt1) dist) ) 

   (setq ocmd (getvar "cmdecho"))
   (setvar "CMDECHO" 0)
   (command "_.pline" "_none" pt2 "_width" "0" width "_none" pt3 "_width" "0" "0" "_none"  pt1 "")
   (setvar "CMDECHO" ocmd)
   )
)

(princ)					; fin silencieux
)

; vectech prend en compte une échelle
(defun vectech (width dist ech / pt1 pt2 pt3 pt4)
  
    
(if
  (and
    (setq pt1 (getpoint "\nSpécifiez le point de départ: "))
    (setq pt2 (getpoint pt1 "\nSpécifiez le second point: "))
  )

   (progn
     (setq long (* (distance pt1 pt2) ech))
     (setq pt3  (polar pt1 (angle pt1 pt2) (- long dist) ))
     (setq pt4  (polar pt1 (angle pt1 pt2) long) )

   (setq ocmd (getvar "cmdecho"))
   (setvar "CMDECHO" 0)
   (command "_.pline" "_none" pt4 "_width" "0" width "_none" pt3 "_width" "0" "0" "_none"  pt1 "")
   (setvar "CMDECHO" ocmd)
   )
)

(princ)					; fin silencieux
)




; cut in one point
(defun c:BR ()
  (command "_break" pause "_f" pause (getvar "lastpoint"))(princ))


; creation porte simple, en 3 points
(defun c:porte (/ pt1 pt2 ext angside edpline arc sel angext)

  ;(princ "ici3\n")
  
  (command "_.undo" "_group")

  (setq	pt1	(getpoint "\nSpécifiez le premier point: ")
	pt2	(getpoint pt1 "\nSpécifiez le second point: ")
	ext	(getpoint "\nSpécifiez le côté de l'ouverture: ")
	angside	(angle pt1 pt2)
	angext	(angle pt1 ext)
	pi4	(/ PI 4)
  )

  (if (> angside PI)

    (progn
      (if (and (< angext angside) (> angext (- angside PI)))
	(setq dir (- pi4))
	(setq dir (+ pi4))
      )

    )

    (progn				; angside > pi
      (if (and (> angext angside) (< angext (+ angside PI)))
	(setq dir (+ pi4))
	(setq dir (- pi4))

      )
    )

  )

  (setq angside (+ angside dir))
  (setq ext (polar pt1 angside (distance pt1 pt2)))

  (command "_.pline" "_none" pt1 "_none" ext "")
  (setq edpline (entlast))		; mémorise pline


  (if (< dir 0)

    (command "_.arc" "_c" "_none" pt1 ext pt2)
    (command "_.arc" "_c" "_none" pt1 pt2 ext)
  )
  (setq ltyp "CACHE2")
  (if (not (tblsearch "ltype" ltyp))
    (command "._linetype" "_load" ltyp "Acadiso.lin" "")
  )
  (if (tblsearch "ltype" ltyp)
    (command "_.chprop" "_last" "" "_ltype" ltyp "")
  )

  ; pour lier la porte et l'arc ensemble - annule le style CACHEX2
;;;  (setq arc (entlast))  
;;;  (if (zerop (getvar "peditaccept"))
;;;    (command "_.pedit" "_m" arc edpline "" "_y" "_j" "" "")
;;;    (command "_.pedit" "_m" arc edpline "" "_j" "" "")
;;;  )
  
  (command "_.undo" "_end")

  (princ)

)




; gel/ dégel le calque NP
(defun c:gnp ()
  (command "_.undo" "_group")

  (setq acadObj (vlax-get-acad-object))
  (setq doc (vla-get-ActiveDocument acadObj))

  (setq layerObj (vla-Item (vla-get-Layers doc) "NP"))
  (setq currLayer (vla-get-ActiveLayer doc))
  (if (= layerObj :vlax-false)
    (progn
      (alert "Le calque NP n'existe pas")
      (exit)
      )
    )

  ;(alert (strcat "The current layer is " (vla-get-Name currLayer)))

  (if (= (vla-get-Name currLayer) (vla-get-Name layerObj))
    (alert
      "Le calque NP ne peut être gelé/dégelé car c'est le calque courant"
    )
    (progn
      (if (= (vla-get-Freeze layerObj) :vlax-true)
	(progn
	  (vla-put-Freeze layerObj :vlax-false)
	  (princ "Le calque NP a été dégelé")
	)
	(progn
	  (vla-put-Freeze layerObj :vlax-true)
	  (princ "Le calque NP a été gelé")
	)
      )
    )
    )
    (command "_.undo" "_end")
    (princ)

)

; verrouille / déverrouille le calque NP
(defun c:vnp ()
  (command "_.undo" "_group")

  (setq acadObj (vlax-get-acad-object))
  (setq doc (vla-get-ActiveDocument acadObj))

  (setq layerObj (vla-Item (vla-get-Layers doc) "NP"))
  (if (= layerObj 0)
    (progn
      (alert "Le calque NP n'existe pas")
      (exit)
    )
  )


  (if (= (vla-get-Lock layerObj) :vlax-true)
    (progn
      (vla-put-Lock layerObj :vlax-false)
      (princ "Le calque NP a été déverrouillé")
    )
    (progn
      (vla-put-Lock layerObj :vlax-true)
      (princ "Le calque NP a été verrouillé")
    )
  )
  (command "_.undo" "_end")
  (princ)

)

(defun c:repeat_offset ( / ent mode vn lst_vn tmp side_offset)

        (setq ent "Interval" mode nil)

        (initget "Cumul Interval")

        (while (or (eq  (setq ent (entsel (strcat "\nSélectionnez l'objet à décaler plusieurs fois ou Mode d'action [Cumul/Interval] <" ent ">: "))) "Interval") (eq ent "Cumul") (not ent))

                (if (not ent) (setq ent (if mode "Cumul" "Interval")))

                (if (eq ent "Cumul") (setq mode T) (setq mode nil))

                (initget "Cumul Interval")

        )

        (cond

                (ent

                        (setq lst_vn nil)

                        (initget 70 "Median")

                        (princ  "\nSpécifiez une distance de décalage ou [Median]: ")

                        (while (setq vn (getdist (cadr ent)))

                                (princ "\nSpécifiez une distance de décalage ou [Median] [")

                                (cond

                                        ((eq vn "Median")

                                                (if (and (car lst_vn) (< (car lst_vn) 0.0))

                                                        (princ " *Déja en mode MEDIAN* ")

                                                        (progn

                                                                (foreach n (reverse lst_vn) (princ (rtos n)) (princ "/"))

                                                                (setq lst_vn (cons -1.0 lst_vn))

                                                        )

                                                )

                                                (princ "]: ")

                                        )

                                        (T

                                                (cond

                                                        (mode

                                                                (if (eq (car lst_vn) -1.0)

                                                                        (if (cadr lst_vn)

                                                                                (setq lst_vn (cons vn (cons (+ (cadr lst_vn) (/ (- vn (cadr lst_vn)) 2.0)) (cdr lst_vn))))

                                                                                (setq lst_vn (cons vn (cons (+ (/ vn 2.0)) (cdr lst_vn))))

                                                                        )

                                                                        (if (> vn (car lst_vn))

                                                                                (setq lst_vn (cons vn lst_vn))

                                                                                (princ "\n*Incorrect* La valeur doit être > à la précédente.\nSpécifiez une distance de décalage [")

                                                                        )

                                                                )

                                                                (foreach n (reverse lst_vn) (princ (rtos n)) (princ "/"))

                                                        )

                                                        (T

                                                                (if (eq (car lst_vn) -1.0)

                                                                        (setq lst_vn (cons (/ vn 2.0) (cons (/ vn 2.0) (cdr lst_vn))))

                                                                        (setq lst_vn (cons vn lst_vn))

                                                                )

                                                                (foreach n (reverse lst_vn) (princ (rtos n)) (princ "/"))

                                                        )

                                                )

                                                (princ "]: ")

                                        )

                                )

                                (initget 70 "Median")

                        )

                        (if (member -1.0 lst_vn) (setq lst_vn (vl-remove -1.0 lst_vn)))

                        (if (not mode)

                                (progn

                                        (setq tmp nil)

                                        (while lst_vn

                                                (setq tmp (cons (apply '+ lst_vn) tmp))

                                                (setq lst_vn (cdr lst_vn))

                                        )

                                        (setq lst_vn (reverse tmp))

                                )

                        )

                        (cond

                                (lst_vn

                                        (initget 1)

                                        (setq side_offset (getpoint "\nSpécifiez un point sur le côté à décaler: "))

                                        (setvar "cmdecho" 0)

                                        (command "_.undo" "_group")

                                        (foreach n lst_vn (command "_.offset" n ent side_offset ""))

                                        (command "_.undo" "_group")

                                        (setvar "cmdecho" 1)

                                )

                        )

                )

        )

        (prin1)

)

; T2M - convert individual Texts/Dtexts to individual MTexts

; modified by Xanadu - www.xanadu.cz

; ***** This routine NEEDS the Express Tools *****


(defun C:T2M (/ ss i elist)

  (prompt "\nSelect Text objects to convert to MTexts: ")

  (setq ss (ssget (list (cons 0 "TEXT"))))

  (setq i -1)

  (if ss

    (repeat (sslength ss)

      (setq elist (cdr (assoc -1 (entget (ssname ss (setq i (1+ i)))))))

      (command "TXT2MTXT" elist ""); Express Tools command

    )

  )

)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Incrementation lettre ou numero des sommets d'une polyligne
;http://cadxp.com/topic/29955-icrementation-texte-dans-lordre-dune-polyligne/


;;; place à chaque sommet d'une polyligne une chaine de caractère qui s'incrémente
; Fonctionne avec lettres et chiffres
(defun c:inc(/ cc haut ind pt r)
  (command "_.undo" "_group")

  (princ "\nAttention au sens de la polyligne d'AXE!!!")

  (while (not r)

    (setq r (getstring T (strcat "\nCalque d'insertion de la numérotation: <" (setq cc (getvar "clayer")) "> : ")))

    (cond

      ((eq r "")

        (setq r cc)

      )

      ((not (tblsearch "layer" r))

        (princ (strcat "\nCalque " r " inexistant."))

        (setq r nil)

      )

    )

  )

  (setq ind (getstring "\nLettre de départ <A>: "))
  (if (eq ind "")
	(setq ind "A")
  )
  
  (or (setq haut (getreal "\nHauteur du texte <0.4>: "))

    (setq haut 0.4)

  )

  (setq style (getstring "\nStyle du texte <style14>: "))
  (if (eq style "")
	(setq style "style14")
  )
  (if (not (tblsearch "style" style))
    (setq style "Standard"))

  (and  (setq ObjLwHaut (entsel "\nSélectionner l'AXE : "))

        (setq ObjLwHaut (entget (car ObjLwHaut)))

        (eq (cdr (assoc 0 ObjLwHaut)) "LWPOLYLINE")

    (progn
      (foreach pt (vl-remove-if-not '(lambda(x)(eq (car x) 10)) ObjLwHaut)

        (entmake (list  (cons   0 "TEXT")

                        (cons 100 "AcDbEntity")

                        (cons 100 "AcDbText")

                        (cons   1 ind)

			(cons 7  style)

                        (cons  10 (trans (cdr pt) 1 0))

                        (cons  11 (trans (cdr pt) 1 0))

                        (cons   8 r)

                        (cons  40 haut)

			;justification bas droit : (optionnel)
			;(cons 71 9)
			(cons 72 2)
			(cons 73 1)
                 )

        )
	(setq ind (A++ ind))
      )
         (princ "yoplaboum")
       ; (setq ins_pt_cell (getpoint "\nPoint d'insertion haut gauche du tableau: "))

      ;  (vla-addTable Space (vlax-3d-point ins_pt_cell) (+ 3 nb) 6 (+ h_t (* h_t 0.25)) w_c)
    )
 

  )

  (command "_.undo" "_end")
  (princ)

)



;; Alpha++ inc -  Lee Mac
;; Increments an alphabetical string by one, e.g. AZ => BA
;; a - [str] alphabetical string
;; works also with numbers in the string

(defun A++ ( a )
    (   (lambda ( f ) (vl-list->string (reverse (f (reverse (vl-string->list a)) t))))
        (lambda ( l x )
            (cond
                (   (null l) (if x '(65) '(97)))
                (   (= 090 (car l)) (cons 65 (f (cdr l)  t )))
                (   (= 122 (car l)) (cons 97 (f (cdr l) nil)))
                (   (cons (1+ (car l)) (cdr l)))
            )
        )
    )
)
(princ)
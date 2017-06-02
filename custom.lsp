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


; vecteur
(defun c:vect ()
  (vectdim 0.1 0.1)
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

  (setq arc (entlast))

  (if (zerop (getvar "peditaccept"))
    (command "_.pedit" "_m" arc edpline "" "_y" "_j" "" "")
    (command "_.pedit" "_m" arc edpline "" "_j" "" "")
  )

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

; Numero
(defun C:numero (/)
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

  (or (setq num (getint "\n Indice de départ <A>: "))

    (setq num A)

  )

  (or (setq haut (getreal "\nHauteur du texte <0.55>: "))

    (setq haut 0.55)

  )
  (and  (setq ObjLwHaut (entsel "\nSélectionner l'AXE : "))

        (setq ObjLwHaut (entget (car ObjLwHaut)))

        (eq (cdr (assoc 0 ObjLwHaut)) "LWPOLYLINE")

    (progn
      (princ "hello you")
      ) ; fin progn
      
) ; fin and
  
  (command "_.undo" "_end")

  					; fin
  (princ)
  
) ; fin numero

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;http://cadxp.com/topic/29955-icrementation-texte-dans-lordre-dune-polyligne/
(defun c:G18a()
(vl-load-com)
(princ "\nAttention au sens de la polyligne d'AXE!!!")
(setq cc (getvar "clayer"))
(setq r (strcase (getstring (strcat "\nCalque d'insertion de la numérotation: <" cc "> : "))))
(if (= "" r)(setq r cc))
(setvar "clayer" r)
(setq num (getreal "\nNuméro de départ <1>: "))
(if (= nil num)(setq num 1))
(setq haut (getreal "\nHauteur du texte <1>: "))
(if (= nil haut)(setq haut 1))
(setq ObjLwHaut (car (entsel "\nSélectionner l'AXE : ")))
(setq VlaObjLwHaut (vlax-ename->vla-object ObjLwHaut))
(SommetLWPol (entget ObjLwHaut))
(setq LstPointsLwHaut LstPointsLw)
(setq NbreSommet (length LstPointsLwHaut))
(setq lst (ssget "_f" LstPointsLwHaut (list (cons 0 "point"))))
(setq nombreau (sslength lst))
(setq indiceau 0)
(repeat nombreau
(setq ent (ssname lst indiceau))
(setq x (list lst indiceau))
(setq tl (vlax-get (vlax-ename->vla-object ent) 'coordinates))
(setq resul (+ num indiceau))
(command "_text" tl haut 0 (rtos resul 2 0))
(setq indiceau (1+ indiceau))
)
)

(defun c:G18a(/ ang cc doc esp haut js lig lst num pt1 pt2 r sel tab)

  (vl-load-com)

  (setq doc (vla-get-activedocument (vlax-get-acad-object)))

  (if (zerop (getvar "tilemode"))

    (setq esp (vla-get-paperspace doc))

    (setq esp (vla-get-modelspace doc))

  )

  (or patrick_g18a_calque (setq patrick_g18a_calque (getvar "clayer")))

  (or patrick_g18a_haut (setq patrick_g18a_haut 1))

  (or patrick_g18a_num (setq patrick_g18a_num 1))

  (princ "\nAttention au sens de la polyligne d'AXE!!!")

  (while (not r)

    (setq r (getstring T (strcat "\nCalque d'insertion de la numérotation: <" patrick_g18a_calque "> : ")))

    (cond

      ((eq r "")

        (setq r patrick_g18a_calque)

      )

      ((not (tblsearch "layer" r))

        (princ (strcat "\nCalque " r " inexistant."))

        (setq r nil)

      )

      (T

        (setq patrick_g18a_calque r)

      )

    )

  )

  (and (setq num (getint (strcat "\nNuméro de départ <" (itoa patrick_g18a_num) ">: ")))

    (setq patrick_g18a_num num)

  )

  (and (setq haut (getreal (strcat "\nHauteur du texte <" (rtos patrick_g18a_haut) ">: ")))

    (setq patrick_g18a_haut haut)

  )

  (and  (setq ObjLwHaut (entsel "\nSélectionner l'AXE : "))

        (setq ObjLwHaut (entget (car ObjLwHaut)))

        (eq (cdr (assoc 0 ObjLwHaut)) "LWPOLYLINE")

    (progn

      (vla-startundomark doc)

      (setq lst (vl-remove-if-not '(lambda(x)(eq (car x) 10)) ObjLwHaut)

            js (ssget "_x" '((0 . "POINT")))

            sel (vla-get-activeselectionset doc)

      )

      (while (and js (cadr lst))

        (setq tab nil

              pt1 (cdar lst)

              pt2 (cdadr lst)

              ang (angle pt1 pt2)

        )

        (setq lig (vla-addline esp (vlax-3d-point (trans pt1 1 0)) (vlax-3d-point (trans pt2 1 0))))

        (vlax-for pt sel

          (if (vlax-invoke lig 'intersectwith pt acextendnone)

            (if (and (equal (vlax-get pt 'coordinates) (vlax-get lig 'endpoint)))

              (or (caddr lst)

                (setq tab (cons (vlax-get pt 'coordinates) tab))

              )

              (setq tab (cons (vlax-get pt 'coordinates) tab))

            )

          )

        )

        (vla-delete lig)

        (and tab

          (cond

            ((and (>= ang 0)

                  (< ang (/ pi 2))

              )

              (setq tab (vl-sort tab '(lambda(a b)(if (eq (car a) (car b))

                                                    (< (cadr a) (cadr b))

                                                    (< (car a) (car b))

                                                  )

                                     )

                        )

              )

            )

            ((and (>= ang (/ pi 2))

                  (< ang pi)

              )

              (setq tab (vl-sort tab '(lambda(a b)(if (eq (car a) (car b))

                                                    (< (cadr a) (cadr b))

                                                    (> (car a) (car b))

                                                  )

                                     )

                        )

              )

            )

            ((and (>= ang pi)

                  (< ang (+ pi (/ pi 2)))

              )

              (setq tab (vl-sort tab '(lambda(a b)(if (eq (car a) (car b))

                                                    (> (cadr a) (cadr b))

                                                    (> (car a) (car b))

                                                  )

                                     )

                        )

              )

            )

            (T

              (setq tab (vl-sort tab '(lambda(a b)(if (eq (car a) (car b))

                                                    (> (cadr a) (cadr b))

                                                    (< (car a) (car b))

                                                  )

                                     )

                        )

              )

            )

          )

          (foreach pt tab

            (entmake (list (cons   0 "TEXT")

                           (cons 100 "AcDbEntity")

                           (cons 100 "AcDbText")

                           (cons   1 (rtos patrick_g18a_num 2 0))

                           (cons  10 (trans pt 1 0))

                           (cons  11 (trans pt 1 0))

                           (cons   8 patrick_g18a_calque)

                           (cons  40 patrick_g18a_haut)

                     )

            )

            (setq patrick_g18a_num (1+ patrick_g18a_num))

          )

        )

        (setq lst (cdr lst))

      )

      (vla-delete sel)

      (vla-endundomark doc)

    )

  )

  (princ)

)

;;;
(defun c:G18a(/ cc haut num pt r)

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

  (or (setq num (getint "\nNuméro de départ <1>: "))

    (setq num 1)

  )

  (or (setq haut (getreal "\nHauteur du texte <1>: "))

    (setq haut 1)

  )

  (and  (setq ObjLwHaut (entsel "\nSélectionner l'AXE : "))

        (setq ObjLwHaut (entget (car ObjLwHaut)))

        (eq (cdr (assoc 0 ObjLwHaut)) "LWPOLYLINE")

    (progn

      (foreach pt (vl-remove-if-not '(lambda(x)(eq (car x) 10)) ObjLwHaut)

        (entmake (list  (cons   0 "TEXT")

                        (cons 100 "AcDbEntity")

                        (cons 100 "AcDbText")

                        (cons   1 (rtos num 2 0))

                        (cons  10 (trans (cdr pt) 1 0))

                        (cons  11 (trans (cdr pt) 1 0))

                        (cons   8 r)

                        (cons  40 haut)

                 )

        )

        (setq num (1+ num))

      )

    )

  )

  (princ)

)
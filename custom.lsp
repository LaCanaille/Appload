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
;;; portec : pour dessiner une porte avec embrasure. spécifiez le premier point, le second point, puis cliquez du côté où ouvrir la porte ainsi que du côté où dessiner les embrasures.
;;; Pour changer les dimensions par défaut du cadre tapez sur une touche au début
;;;
;;; spécial pour Mathilde pour la gestion du calque np (pourrait s'appliquer à d'autres calques
;;; gnp : gel ou dégèle le calque np
;;; vnp : verrouille ou déverrouille le calque np 
;;;
;;; repeat_offset : permet de faire un décaler multiple. décaler l'objet selon une liste de distance (mode cumul ou interval)
;;;
;;; T2M - convert individual Texts/Dtexts to individual MTexts
;;;
;;; inc : numérote avec incrémentation les sommets d'une polyligne, avec des chiffres ou des lettres, et propose d'écrire le tableau de coordonnées des sommets


;;; vecteur ;;;
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

   ;; récupérer les valeurs de variables système
   (setq osm  (getvar "osmode")
	 echo (getvar "cmdecho")
   )
   (setvar "CMDECHO" 0)  
   (command "_.pline" "_none" pt2 "_width" "0" width "_none" pt3 "_width" "0" "0" "_none"  pt1 "")
  ;; restaurer les valeurs des variables système
   (setvar "osmode" osm)
   (setvar "cmdecho" echo)
   )
)

(princ)					; fin silencieux
) ; fin vect

;;; vectech prend en compte une échelle ;;;
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
   ;; récupérer les valeurs de variables système
   (setq osm  (getvar "osmode")
	 echo (getvar "cmdecho")
   )
   (setvar "CMDECHO" 0)
     
   (command "_.pline" "_none" pt4 "_width" "0" width "_none" pt3 "_width" "0" "0" "_none"  pt1 "")
  ;; restaurer les valeurs des variables système
   (setvar "osmode" osm)
   (setvar "cmdecho" echo)
   )
)

(princ)					; fin silencieux
) ; fin vectech




;;; cut in one point ;;
(defun c:BR ()
  (command "_break" pause "_f" pause (getvar "lastpoint"))(princ))

;;; creation porte simple, en 3 points ;;;

(defun c:porte (/ pt1 pt2 ext angside angext pi4 dir edpline arc sel)
  ;; récupérer les valeurs de variables système
  (setq	osm  (getvar "osmode")
	echo (getvar "cmdecho")
  )
  (setvar "CMDECHO" 0)
  (command "_.undo" "_group")
  
  (setq	pt1	(getpoint "\nSpécifiez le premier point: ")
	pt2	(getpoint "\nSpécifiez le second point: ")
	ext	(getpoint "\nSpécifiez le côté de l'ouverture: ")
	angside	(angle pt1 pt2)
	angext	(angle pt1 ext)
	pi4	(/ PI 4)
  )

  (if (> angside PI)

    (progn
      (if (and (< angext angside) (> angext (- angside PI)))
	(setq dir (- 1))
	(setq dir (+ 1))
      )

    )

    (progn				; angside > pi
      (if (and (> angext angside) (< angext (+ angside PI)))
	(setq dir (+ 1))
	(setq dir (- 1))

      )
    )

  )

  (setq dir (* dir pi4))
  (setq angside (+ angside dir))
  (setq ext (polar pt1 angside (distance pt1 pt2)))
  
  (command "_.pline" "_none" pt1 "_none" ext "")
  (setq edpline (entlast))		; mémorise pline

  (if (< dir 0)

    (command "_.arc" "_c" "_none" pt1 ext pt2)
    (command "_.arc" "_c" "_none" pt1 pt2 ext)
  )
  (command "_.chprop" "_last" "" "_ltype" "CACHE2" "")

  ; pour lier la porte et l'arc ensemble - annule le style CACHE2
;;;  (setq arc (entlast))  
;;;  (if (zerop (getvar "peditaccept"))
;;;    (command "_.pedit" "_m" arc edpline "" "_y" "_j" "" "")
;;;    (command "_.pedit" "_m" arc edpline "" "_j" "" "")
;;;  )

  (command "_.undo" "_end")
  
  ;; restaurer les valeurs des variables système
  (setvar "osmode" osm)
  (setvar "cmdecho" echo)
  (princ)

); fin porte


;;;;;;;;;;;;; creation de porte avec structure embrasure rectangle ;;;
(defun c:portec (/ pt1 pt2 ext extc pi2 pi4 angside andext dir dirc dircpi2 pt11 pt12 pt13 pt21 pt22 pt23 a b edpline arc sel)
  ;; récupérer les valeurs de variables système
  (setq	osm  (getvar "osmode")
	echo (getvar "cmdecho")
  )
  (setvar "CMDECHO" 0)
  (command "_.undo" "_group")

    ; paramètres cadre par défaut
  (if (or (/= (type *la*) 'REAL) (<= *la* 0)) (setq *la* 0.05))
  (if (or (/= (type *ht*) 'REAL) (<= *ht* 0)) (setq *ht* 0.05))
 
  ; points porte
   (while
        (and
        (princ (strcat "\nDimensions embrasures L*ht <" (rtos *la*)"*"(rtos *ht*)">: "
		       "\nSpécifiez le premier point ou Entrée pour changer les paramètres")
        )
        (not (setq pt1 (getpoint)))
        ) ; fin and

       (setq *la*
	 (cond
	   ((getreal (strcat "\nLargeur cadre <" (rtos *la*) ">: "))); if User types something, use it
	   (*la*)
	   ) ;cond
	);setq

    	(setq *ht*
	 (cond
	   ((getreal (strcat "\nHauteur cadre <" (rtos *ht*) ">: "))); if User types something, use it
	   (*ht*)
	   ) ;cond
	);setq

     ) ; fin while

  (setq
	pt2	(getpoint pt1 "\nSpécifiez le second point: ")
	ext	(getpoint "\nSpécifiez le côté de l'ouverture: ")
	extc	(getpoint "\nSpécifiez le côté des embrasures ")
  )

  (setq angside	(angle pt1 pt2)
	angext	(angle pt1 ext)
	angextc	(angle pt1 extc)
	pi4	(/ PI 4)
	pi2 	(/ PI 2)
  )

  ; direction porte
  (if (> angside PI)

    (progn
      (if (and (< angext angside) (> angext (- angside PI)))
	(setq dir (- 1))
	(setq dir (+ 1))
      )
    )

    (progn				; angside > pi
      (if (and (> angext angside) (< angext (+ angside PI)))
	(setq dir (+ 1))
	(setq dir (- 1))
      )
    )

  )

  ; direction embrasure
  (if (> angside PI)

    (progn
      (if (and (< angextc angside) (> angextc (- angside PI)))
	(setq dirc (- 1))
	(setq dirc (+ 1))
      )
    )

    (progn				; angside > pi
      (if (and (> angextc angside) (< angextc (+ angside PI)))
	(setq dirc (+ 1))
	(setq dirc (- 1))
      )
    )
  )

  ; dessin cadre
  (setq oldcolor (getvar "CECOLOR")) ; sauvegarde de la couleur courante
  (setvar "CECOLOR" "252")
  (setq dircpi2 (+ angside (* dirc pi2)))
  (setq pt11 (polar pt1 angside *la*)
	pt12 (polar pt11 dircpi2 *ht*)
	pt13 (polar pt1  dircpi2 *ht*)
	a    (polar pt11 dircpi2 (/ *ht* 2))
	pt21 (polar pt2 (+ PI angside) *la*)
	pt22 (polar pt21 dircpi2 *ht*)
	pt23 (polar pt2  dircpi2 *ht*)
	b    (polar pt21 dircpi2 (/ *ht* 2))
	)

  (command "_.pline" "_none" pt1 "_none" pt11 "_none" pt12 "_none" pt13 "c" )
  (command "chprop" "_last" "" "CO" "7" "") ; blanc
  (command "._hatch" "_S"  (entlast) "")
  (command "chprop" "_last" "" "CO" "253" "")

  (command "_.pline" "_none" pt2 "_none" pt21 "_none" pt22 "_none" pt23 "c" )
  (command "chprop" "_last" "" "CO" "7" "")
  (command "._hatch" "_S" (entlast) "")
  (command "chprop" "_last" "" "CO" "253" "")
  
  ;dessin porte

  (setq angside (+ angside (* pi4 dir)))
  (setq ext (polar a angside (distance a b)))

  (command "_.pline" "_none" a "_none" ext "")
  (setq edpline (entlast))		; mémorise pline

  (if (< dir 0)

    (command "_.arc" "_c" "_none" a ext b)
    (command "_.arc" "_c" "_none" a b ext)
  )
  ;(command "_.chprop" "_last" "" "_ltype" "CACHE2" "")

  ; pour lier la porte et l'arc ensemble - annule le style CACHE2
  (setq arc (entlast))  
  (if (zerop (getvar "peditaccept"))
    (command "_.pedit" "_m" arc edpline "" "_y" "_j" "" "")
    (command "_.pedit" "_m" arc edpline "" "_j" "" "")
  )
  (setvar "CECOLOR" oldcolor)

  (command "_.undo" "_end")
  
  ;; restaurer les valeurs des variables système
  (setvar "osmode" osm)
  (setvar "cmdecho" echo)
  (princ)
) ; fin portec


;;; gel/ dégel le calque NP ;;;
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

) ; fin gnp

;;; verrouille / déverrouille le calque NP ;;;
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

) ; fin vnp

;;; repeat_offset : permet de faire un décaler multiple. décaler l'objet selon une liste de distance (mode cumul ou interval) ;;;
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

                                        (getvar echo "cmdecho")
				 	(setvar "cmdecho" 0)

                                        (command "_.undo" "_group")

                                        (foreach n lst_vn (command "_.offset" n ent side_offset ""))

                                        (command "_.undo" "_group")

                                        (setvar "cmdecho" echo)

                                )

                        )

                )

        )

        (princ)

); fin repeat_offset


;;; T2M - convert individual Texts/Dtexts to individual MTexts ;;;
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

) ; fin T2M

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Incrementation lettre ou numero des sommets d'une polyligne
;http://cadxp.com/topic/29955-icrementation-texte-dans-lordre-dune-polyligne/


;;; place à chaque sommet d'une polyligne une chaine de caractère qui s'incrémente
; Fonctionne avec lettres et chiffres
; propose d'écrire le tableau de points
; ATTENTION cela crée et donne les coordonnées par rapport à une polyligne dans le SCU général! si vous avez changé de SCU cela ne marchera pas
(defun c:inc(/ cc haut ind pt r style ObjLwHaut nbPoints i ind itab ins_pt_cell liste nl acadObject doc ColWidth RowHeight NumColumns NumRows ColWidth
	      column row textsize doc modelSpace vlaTableau w)
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

  (setq ind (getstring "\nLettre/Indice de départ <A>: "))
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
      (setq nbPoints 0)
      (setq liste nil)
      
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
	
	(setq liste (cons ind liste))
	(setq liste (cons (rtos (cadr pt) 2 2) liste)) ; x   pt (num x y)
	(setq liste (cons (rtos (caddr pt) 2 2) liste)); y
	(setq ind (A++ ind)
	      nbPoints (+ 1 nbPoints)
	      )
	
      )
       (setq liste (reverse liste))
      
       (setq itab (getstring "\nVoulez-vous insérer le tableau? <O>: "))
       (if (eq itab "")
	 (setq itab "O")
       )
      
	(if (or (eq (strcase itab nil) "O") (eq (strcase itab nil) "OUI"))
			(progn
			  (setq	ins_pt_cell (getpoint "\nPoint d'insertion haut gauche du tableau: " ) )
			  (setq acadObj (vlax-get-acad-object))
    			  (setq doc (vla-get-ActiveDocument acadObj))
			  (setq modelSpace (vla-get-ModelSpace doc))
			  (setq NumRows (+ 2 nbPoints))	; 2 lignes de titre + total
			  (setq NumColumns 3)
			  
			  (setq textsize 0.34)          ; Voir cette variable qui contrôle la hauteur du texte
			  ;(setvar "textsize" 0.4)
   			  (setq RowHeight 0.6) ;(* 1.7 textsize))     ;0.57
			  (setq w  (max (strlen (nth 1 liste)) (strlen (nth 2 liste))))
			  (setq w (+ 4 w))
  			  (setq ColWidth (* w textsize)) ;(* 11 textsize))    ; 4.8
	
			  (AddMyTabStyle textsize)
			  (setvar "ctablestyle" "myTableStyle")
			  (setq vlaTableau (vla-addTable modelSpace (vlax-3d-point ins_pt_cell) NumRows NumColumns RowHeight ColWidth ));RowHeight ColWidth))

			  ;--- remplissage du tableau --- ;
			  (vla-setcolumnwidth vlaTableau 0 (* textsize (+ 4 (strlen (nth 0 liste)))) )

			   ;; Ligne 0
 
			   (setq row 0)
			 
			   (setq column 0)
			 
			   (SetCellProperties vlaTableau row column "Coordonnées des points" textsize acMiddleCenter nil)
			 
			   ;; Ligne 1
			 
			   (setq row 1)
			 
			   (SetCellProperties vlaTableau row 0 "MAT" textsize acMiddleCenter (cons acHorzBottom acLnWt040))
			   (SetCellProperties vlaTableau row 1 "X" textsize acMiddleCenter (cons acHorzBottom acLnWt040))
			   (SetCellProperties vlaTableau row 2 "Y" textsize acMiddleCenter (cons acHorzBottom acLnWt040))
			 
 			     ;; Lignes de résultat
 
			   (setq i 0)
			 
			   (while (< i nbPoints)
			 
			      (setq row (+ i 2))
			      (setq nl (* 3 i))
			      (SetCellProperties vlaTableau row 0 (nth nl liste) textsize acMiddleCenter nil) ; nom
			      (SetCellProperties vlaTableau row 1 (nth (+ 1 nl) liste) textsize acMiddleCenter nil) ; X
			      (SetCellProperties vlaTableau row 2 (nth (+ 2 nl) liste) textsize acMiddleCenter nil) ; Y

			      (setq i (1+ i))
			 
			   )
			  ;-- Fin écriture tableau --;
			  
			)
		  )

	    )
 

  )

  (command "_.undo" "_end")
  (princ)

) ; fin inc



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
) ; fin A++


   ;; Gère les propriétés populaires des cellules d'un tableau
 
   ;;    vlaTableau Row Column : Obligatoire, les autres sont facultatifs
 
   ;;    Row, Column : INT, base 0
 
   ;;    Alignment: INT,
 
   ;;    LineWeightPair: nil, sinon (cons "AcGridLineType enum" "acad_lweight enum"), soit (cons Position Épaisseur)
 
   ;; Support pour Acad2006 et Acad2009
(defun SetCellProperties (
 
   vlaTableau Row Column Texte TextHeight Alignment LineWeightPair
 
   )
 
   (if LineWeightPair (vla-SetCellGridLineWeight vlaTableau Row Column (car LineWeightPair) (cdr LineWeightPair)))
 
   (if Alignment (vla-SetCellAlignment vlaTableau Row Column Alignment))
 
   (if TextHeight (vla-SetCellTextHeight vlaTableau Row Column TextHeight))
 
   (if Texte
 
      (if vla-SetCellValue
 
         (vla-SetCellValue vlaTableau Row Column Texte) ; AutoCAD 2009
 
         (vla-SetText vlaTableau Row Column Texte)      ; AutoCAD 2006
 
      )
 
   )
 
) ; fin SetCellProperties

;;; TableStyle personnel
(defun AddMyTabStyle (textsize / docObj tblObj tblStlObj)

  (setq docObj (vla-get-activedocument (vlax-get-acad-object)))
  (setq tblStlObj
    (vla-addobject
      (vla-add (vla-get-dictionaries docObj) "ACAD_TABLESTYLE")
      "myTableStyle"
      "AcDbTableStyle"
    )
  )  
  (vla-put-horzcellmargin tblStlObj 0.06)
  (vla-put-vertcellmargin tblStlObj 0.06)
  (vla-settextheight tblStlObj (+ acDataRow acTitleRow acHeaderRow) textsize)
  
  (vla-settextstyle tblStlObj (+ acDataRow acTitleRow acHeaderRow) "Standard")
  (princ)
  ) ; fin AddMyTabStyle


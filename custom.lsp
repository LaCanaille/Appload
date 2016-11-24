;;; Fonctions propos�es
;;;
;;;
;;; vect : cr�e une fl�che vecteur. Il suffit de sp�cifier le point de d�but et de fin. La fl�che a la taille 0.1 0.1.
;;; peut s'appeler directement en tapant vect (pas besoin de parenth�ses)
;;;
;;; vectdim : comme la fonction vect mais on sp�cifie les dimensions de la fl�che. exemple : (vectdim 0.16 0.16) le premier argument est la largeur de la fl�che le second la longueur
;;;
;;; vectech : prend en compte une �chelle. le vecteur sera de longueur pt1-pt2 * echelle. (vectech 0.1 0.1 3) par exemple
;;;
;;; BR : pour couper un objet en un seul point. s�lectionnez l'objet � couper puis cliquer sur le point o� couper
;;;
;;; porte : pour dessiner une porte. sp�cifiez le premier point, le second point, puis cliquez du c�t� o� ouvrir la porte



; vecteur
(defun c:vect ()
  (vectdim 0.1 0.1)
)

(defun vectdim (width dist / pt1 pt2 pt3)
  
    
(if
  (and
    (setq pt1 (getpoint "\nSp�cifiez le point de d�part: "))
    (setq pt2 (getpoint pt1 "\nSp�cifiez le second point: "))
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


; vectech prend en compte une �chelle
(defun vectech (width dist ech / pt1 pt2 pt3 pt4)
  
    
(if
  (and
    (setq pt1 (getpoint "\nSp�cifiez le point de d�part: "))
    (setq pt2 (getpoint pt1 "\nSp�cifiez le second point: "))
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
  
 (setq pt1 (getpoint "\nSp�cifiez le premier point: ")
       pt2 (getpoint pt1  "\nSp�cifiez le second point: ")
       ext (getpoint  "\nSp�cifiez le c�t� de l'ouverture: ")
       angside (angle pt1 pt2)
       angext (angle pt1 ext)
       pi4 (/ PI 4)
 )

(if (> angside PI)

  (progn
    (if (and (< angext angside)(> angext (- angside PI)))
      (setq dir (- pi4))
      (setq dir (+ pi4))
      )
    
    )
  
  (progn ; angside > pi
    (if (and (> angext angside)(< angext (+ angside PI)))
      (setq dir (+ pi4))
      (setq dir (- pi4))
	
      )
    )
  
)
  
(setq angside (+ angside dir))
(setq ext (polar pt1 angside (distance pt1 pt2)))
 


  ;(setq ocmd (getvar "cmdecho"))
  ;  (setvar "CMDECHO" 0)
  
  (command "_.pline" "_none" pt1 "_none" ext "")
  (setq edpline (entlast)) ; m�morise pline


  (if (< dir 0)
    
   (command "_.arc" "_c" "_none" pt1 ext pt2)
   (command "_.arc" "_c" "_none" pt1 pt2 ext)
  )
 (princ "hello2")
; transforme arc en polyligne puis join?
(setq arc (entlast))
;(setq sel (append edpline arc))
  
  
;;;  (setq pe (getvar 'PEDITACCEPT))
;;;  (setvar 'PEDITACCEPT 1)
;  (command "_.pedit" arc "")
;;;  (princ "hello3youyouyouyou")
;  (command "_.pedit" "_m" arc edpline "_join" "" "") ; Y?
 (princ "hello3")
;;;   (setvar 'PEDITACCEPT pe)

  (if (zerop (getvar "peditaccept"))
         (command "_.pedit" "_m" arc edpline "" "_y" "_j" "" "")
         (command "_.pedit" "_m" arc edpline "" "_j" "" "")
       )
  
  ;(setvar "CMDECHO" ocmd)


(command "_.undo" "_end")

  (princ)
  
)


; gel/ d�gel le calque NP
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
      "Le calque NP ne peut �tre gel�/d�gel� car c'est le calque courant"
    )
    (progn
      (if (= (vla-get-Freeze layerObj) :vlax-true)
	(progn
	  (vla-put-Freeze layerObj :vlax-false)
	  (princ "Le calque NP a �t� d�gel�")
	)
	(progn
	  (vla-put-Freeze layerObj :vlax-true)
	  (princ "Le calque NP a �t� gel�")
	)
      )
    )
    )
    (command "_.undo" "_end")
    (princ)

)

; verrouille / d�verrouille le calque NP
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
      (princ "Le calque NP a �t� d�verrouill�")
    )
    (progn
      (vla-put-Lock layerObj :vlax-true)
      (princ "Le calque NP a �t� verrouill�")
    )
  )
  (command "_.undo" "_end")
  (princ)

)


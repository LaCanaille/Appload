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

;;; PREFIX
(defun c:ptx (/ doc ent tx1 tx2 txt)
  (vl-load-com)
  (setq doc (vla-get-activedocument (vlax-get-acad-object)))
  (vla-startundomark doc)
  (and    (ssget (list (cons 0 "TEXT,MTEXT")))
    (setq tx1 (getstring "\nTexte � ajouter : "))
    (progn
      (vlax-for ent (setq sel (vla-get-activeselectionset doc))
    (vla-put-textstring ent (strcat tx1 (vla-get-textstring ent)))
      )
      (vla-delete sel)
    )
  )
  (vla-endundomark doc)
  (princ)
)

; Ajouter un morceau de texte (suffix)
(defun c:stx(/ doc ent tx1 tx2 txt)
  (vl-load-com)
  (setq doc (vla-get-activedocument (vlax-get-acad-object)))
  (vla-startundomark doc)
  (and    (ssget (list (cons 0 "TEXT,MTEXT")))
    (setq tx1 (getstring "\nTexte � ajouter : "))
    (progn
      (vlax-for ent (setq sel (vla-get-activeselectionset doc))
    (vla-put-textstring ent (strcat (vla-get-textstring ent) tx1))
      )
      (vla-delete sel)
    )
  )
  (vla-endundomark doc)
  (princ)
)



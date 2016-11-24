;;; Fonction patate
;;; sélectionnez  la ou les polylignes fermées à transformer
;;; cliquez entrée
;;; si vous n'êtes pas satisfait du résultat faites annuler

(vl-load-com) ; Initialize vlisp functions


(defun c:patate	(/ obj_vlax sel nb)

  (command "_.undo" "_group")

  (setq sel nil)

  (while (not sel)		
    (setq sel (ssget (list
		       '(0 . "*POLYLINE") 
		     )
	      )
    )
  )

  (setq nb (sslength sel))

  (while (> nb 0)

    (setq nb (- nb 1)
	vlax_obj (vlax-ename->vla-object(ssname sel nb))
	)
    
    (polytopatate vlax_obj)
     (command "_pedit" (entlast) "_fit" "")
    (entdel (ssname sel nb))
  )


(command "_.undo" "_end")

  					; fin
  (princ)				; silencieux

)




(defun polytopatate
       (obj_vlax / lst_pt pt_start pt_end index x pt1 pt2 opt)

  (setq

    pt_start (vlax-curve-getStartPoint obj_vlax) ; if closed end = start

    pt_end   (vlax-curve-getEndPoint obj_vlax)

    index    (vlax-curve-getEndParam obj_vlax)

    lst_pt   nil

    x	     0

    opt	     ""

    sens (Clockwisep (vlax-curve-getPointAtParam obj_vlax 0) (vlax-curve-getPointAtParam obj_vlax 1) (vlax-curve-getPointAtParam obj_vlax 2))
    
  )
  
  (repeat (fix index)

    (setq
      pt1 (vlax-curve-getPointAtParam obj_vlax x)
      pt2 (vlax-curve-getPointAtParam obj_vlax (setq x (1+ x)))
    )
    (setq lst_pt (append lst_pt (list pt1) (genpoints pt1 pt2 sens)))
  )

  (setq lst_pt (append lst_pt (list pt2))) 


  ; create pline
  (newpline lst_pt)
)




;-------------------------------------

;;; Ang<2pi Retourne l'angle, à 2*k*pi près, compris entre 0 et 2*pi

(defun ang<2pi (ang)

  (if (and (<= 0 ang) (< ang (* 2 pi)))

    ang

    (ang<2pi (rem (+ ang (* 2 pi)) (* 2 pi)))

  )

)


;;; Clockwisep Retourne T si les points p1 p2 et p3 tournent dans le sens horaire

(defun Clockwisep (p1 p2 p3)

  (< pi (ang<2pi (- (angle p2 p3) (angle p1 p2))) (* 2 pi))

)


(defun genpoints ( pt1 pt2 sens / a d n r esp lst pt delta)
	(setq a  (angle pt1 pt2)    ; angle du segment
              d  (distance pt1 pt2) ; longueur de u segment
	      n  1 ; de points generer  ; n = 1 ok
	      r (/ d (* 2. n)) 
	      esp (/ d (1+ n)) ; dist entre les points sur le segment
	      lst nil
	      delta (/ PI 10) ; pi/8 angle de battement : 22 degres ; pi/10 ok

	)
  

  (if (not sens)
    (setq delta (- delta))
    )
  
  (repeat n
	(setq  
	     pt (polar pt1 (+ a delta) (* esp n))
	     
	)
	(setq n (1- n))
	(setq lst (cons pt lst)) ; retourne list

  )

  
)



  ;; create la polyligne
  (defun newpline (points)
    
    (setq ocmd (getvar "cmdecho"))
    (setvar "CMDECHO" 0)
    
    (command "_.pline")
    (foreach p points (command "_none" p))
    (command "") 
    
    (setvar "CMDECHO" ocmd)
    
  )






; génère un nombre aléatoire entre -1 et 1. ou 0 et 1
(defun random ()
  (or seed (setq seed (getvar 'date)))
  (setq seed (rem (1+ (* 1664525 seed)) 4294967296.))
  ;(- (* 2. (/ seed 4294967296.)) 1.) ; pour entre -1 et 1
   (/ seed 4294967296.)
)


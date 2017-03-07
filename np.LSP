(vl-load-com)

; gel/ dégel le calque NP
(defun c:gnp ()
  (command "_.undo" "_group")

  (setq acadObj (vlax-get-acad-object))
  (setq doc (vla-get-ActiveDocument acadObj))
  (setq layerObj (vla-Item (vla-get-Layers doc) "NP"))
  (setq currLayer (vla-get-ActiveLayer doc))

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
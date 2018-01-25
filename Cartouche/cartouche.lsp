;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Cartouche ;;;;;;;;;;;;;;;;;;;
;;; V4 07/12/12
;;;;;;;;;;;;;;;;;;;;;;;;;;; Fonction principale :
;;;
;;; ccar (createCartouche) : personnalisation des proprietes custom du dessin courant, pour remplir les champs du cartouche.
; l'appel de ccar ouvre une interface pour renseigner les différents champs. Quand on clique ok, les champs se mettent à jour dans le cartouche.
;;;
; si création d'une nouvelle présentation, cela fera appel à un onglet d'un gabarit du dossier template
; par défaut la page "TOP_A1_00200_01-01-2017_MODELE" du gabarit "CartoucheMesuralpes.dwt" situé dans le dossier template
;
; nécessite le fichier cartouche.DCL dans un dossier support d'Autocad pour que l'interface se lance (outils->options->fichier
;" Chemin de recherche de fichiers de support en cours d'utilisation " indiquez le dossier où se trouve cartouche.Dcl
;
; pour modifier les listes popup : regarder la toute dernière fonction popupVar
; pour modifier les coordonnées des bureau, le détail est plus bas dans le code dans ccar
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;; fonctions auxiliaires :
;;;
;;; GILE
;;; prop : copie les propriétés d'un fichier (dwg ou dwt) dans les proprietes du dessin courant. Fait appel à :
;;; propCustom
;;; OpenDrawingDBX : Accéder à un dessin fermé
;;; GetItem : retourne le vla-object de l item si présent dans la collection

;;; SET/GET CustomByKey ;;;;;;;
; retourne la valeur de la propriété key du document target si elle existe, nil sinon. différente variantes selon la manière de l'appeler

;;; saveVars sauvegarde des valeurs des listes déroulantes pour le cartouche.DCL

;;; CreatePres(titre gab p)
; Crée une nouvelle présentation au titre "titre" à partir de la page p du gabarit gab si existe
; (gabarit est le nom d un dwt présent dans le dossiers de gabarit template par défaut)

;;; GetIndex (key target liste)
; Indique à quel indice est stocké la propriété Custom du dessin courant dans la liste
; Renvoie un string 
; si la valeur n'y est pas renvoie "0"
; variante GetIndexV (val liste)
; l'index dans la liste commence à 0

;;; Regen() regeneration du dessin

;;; FillInfoPres()
; remplit les informations indice, planche, echelle, type de plan et date d edition
; en fonction du titre de la présentation.
; par défaut utilise la présentation choisie dans la liste déroulante

;;; Case(string which)
; change la casse d'une chaine de caractère
; which 0 met en majuscules
; which 1 met en minuscule
; which 2 met une majuscule à chaque mot
; sinon met une majuscule au début puis normal la suite

;;; words2caps : met une majuscule à chaque mot d'une chaine de caractère

;;; noSpaces(str)
; enlève tous les espaces d'une chaine de caractères

;;; isMultiple(str)
; défini si il y a des espaces, virgules, points, tirets, points virgules dans une chaine de caractères.
;retourne nil si il n'y en a pas, true sinon

;;;  isEmpty (str)
; défini si le string str est vide.
; retourne True si str est vide (des espaces ou bien nul)
; retourne nil sinon

;;; planDP10 (str)
;;; crée le titre de la présentation DP10 selon le nombre de lots
; prend en argument le nombre de lots écrit en lettres

;;; popupVar
; Create hashTable for popup list




(vl-load-com)


(defun c:test()
  (setq test "1")
  (if (getvar "ctab")
 	(setq test (getvar "ctab")))
  (princ test)
)



;; PROP (gile)
;; Importe dans le dessin courant les propriétés personnalisées d'un fichier (dwg ou dwt)

(defun c:prop( / filename)
     (setq filename (getfiled "Choisir le fichier source"
                             (getvar 'dwgprefix)
                             "dwg;dwt"
                             0
                   )
    )
  ;(setq filename "C:\\Users\\Julien\\Desktop\\cartouche.dwg")
 (propCustom filename)
  (princ)
)

(defun propCustom (filename / target  doc odbx source)

  (or *acdoc*
      (setq *acdoc* (vla-get-ActiveDocument (vlax-get-acad-object)))
  )
  (setq target (vla-get-SummaryInfo *acdoc*))
  (if filename
    
   
	;  (princ filename)
     (progn
       (princ filename)
       (if
         (not
           (and
             (setq
               doc (GetItem
                     (vla-get-Documents (vlax-get-acad-object))
                     (strcat (vl-filename-base filename) ".dwg")
                   )
             )
             (= filename (vla-get-FullName doc))
           )
         )
          (setq doc  (OpenDrawingDBX filename)
                odbx T
          )
       )
       (setq source (vla-get-SummaryInfo doc)
             n      -1
       )
       
       (foreach p
                '(Author Comments HyperlinkBase KeyWords Subject Title)
         (if (/= "" (setq prop (vlax-get source p))) ; recopie les proprietes principales du dessin. à garder??
           (vlax-put target p prop)
         )
       )
       (repeat (vla-NumCustomInfo source)
         (vla-GetCustomByIndex source (setq n (1+ n)) 'key 'val)

	 ; ancienne version, ajoute la propriété si elle n'existe pas mais ne la modifie pas si elle existe
	 ; test if the property exists
;;;	 (setq propT (GetCustomByKey key target))
;;;	 (if (not propT)
;;;         	(vla-AddCustomInfo target key val) ; 
;;;	 )
	 (SetCustomByKey key val )
	 
       )
       (and odbx (vlax-release-object doc))
     )
  )
  (princ)
) ; defun propCustom


;;; Accéder à un dessin fermé
(defun OpenDrawingDBX (filename / objdbx release)
  (setq objdbx
         (vlax-create-object
           (if (< (setq release (atoi (getvar "ACADVER"))) 16)
             "ObjectDBX.AxDbDocument"
             (strcat "ObjectDBX.AxDbDocument." (itoa release))
           )
         )
  )
  (vla-open objdbx filename)
  objdbx
)

;;; GetItem (gile)
;;; Retourne le vla-object de l'item s'il est présent dans la collection
;;;
;;; Arguments
;;; col : la collection (vla-object)
;;; name : le nom de l'objet (string) ou son indice (entier)
;;;
;;; Retour : le vla-object ou nil
(defun GetItem (col name / obj)
  (vl-catch-all-apply
    (function (lambda () (setq obj (vla-item col name))))
  )
  obj
)

;;;;;; SET/GET CustomByKey ;;;;;;;
;; retourne la valeur de la propriété key du document target si elle existe, nil sinon
(defun GetCustomByKey (key  / val)
  (GetCustomByKeyT key (vla-get-SummaryInfo (vla-get-ActiveDocument (vlax-get-acad-object))))
  )

(defun GetCustomByKeyT (key target / val)

  (vl-catch-all-apply

    '(lambda ()

       (vla-GetCustomByKey

         target 
         key
         'val
       )

     )
  )
  val
)

; like GetCustomByKeyT but return " " instead of nil
(defun GetCustomNotNil (key target / val)
  (setq val (GetCustomByKeyT key target))
  (if (not val)
    (setq val " ")
    )
  val
  )

; modifie une proprietes du dessin courant. Si elle n'existe pas, crée cette propriété
(defun SetCustomByKey (key val / propT)
    (or *acdoc*
      (setq *acdoc* (vla-get-ActiveDocument (vlax-get-acad-object)))
  )
  (setq target (vla-get-SummaryInfo *acdoc*))
  (SetCustomByKeyT key val target)
  )

(defun SetCustomByKeyT (key val target / propT)

  (setq propT (GetCustomByKeyT key target))

  (if (not val)
    (setq val "")
    )
  (if (not propT)
    (vla-AddCustomInfo target key val)
    (vla-SetCustomByKey target key val)
    )
(princ)
)

;;;;;;;;;;;;;;;;;;;;;;;;;; CARTOUCHE ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ATTENTION necessite le fichier cartouche.DCL dans un dossier support connu d'Autocad

; sauvegarde des valeurs des listes déroulantes pour le cartouche.DCL lors de la fermeture de l'interface (si clic ok)
(defun saveVars()

  ;;;--- Get the selected item from the first list
  (setq sStr1(get_tile "Bureau"))
    (if(= sStr1 "")
    (setq Bureau nil)
    (setq Bureau (nth (atoi sStr1) burList))
  )

  (setq sStr1(get_tile "Planimetrie"))
    (if(= sStr1 "")
    (setq Planimetrie nil)
    (setq Planimetrie (nth (atoi sStr1) planiList))
  )

    (setq sStr1(get_tile "PlaniMethode"))
    (if(= sStr1 "")
    (setq PlaniMethode nil)
    (setq PlaniMethode (nth (atoi sStr1) mPlaniListC))
  )

  (setq sStr1(get_tile "CoordTransformees"))
    (if(= sStr1 "")
    (setq CoordTransformees nil)
    (setq CoordTransformees (nth (atoi sStr1) transfoList))
  )

  (setq sStr1(get_tile "Altimetrie"))
    (if(= sStr1 "")
    (setq Altimetrie nil)
    (setq Altimetrie (nth (atoi sStr1) altiList))
  )

  (setq sStr1(get_tile "AltiMethode"))
    (if(= sStr1 "")
    (setq AltiMethode nil)
    (setq AltiMethode (nth (atoi sStr1) mAltiListC))
  )

  (setq sStr1(get_tile "Plan"))
  (if(= sStr1 "")
          (setq Plan nil)
    	  (setq Plan (nth (atoi sStr1) planListC)) ; nom du plan
      )
  (if(= sStr1 "")
          (setq Typ nil)
    	  (setq Typ (nth (atoi sStr1) planListT)) ; type de plan en 3 lettres
  )

  (setq sStr1(get_tile "Presentation")) ; nom de la présentation
  (if(= sStr1 "")
    (setq Presentation nil)
    (setq Presentation (nth (atoi sStr1) presList))
  )

  (setq sStr1(get_tile "DateJ"))
  (if(= sStr1 "")
    (setq DateJ nil)
    (setq DateJ (nth (atoi sStr1) jourList))
  )
    (setq sStr1(get_tile "DateM"))
  (if(= sStr1 "")
    (setq DateM nil)
    (setq DateM (nth (atoi sStr1) moisList))
  )
    (setq sStr1(get_tile "DateA"))
  (if(= sStr1 "")
    (setq DateA nil)
    (setq DateA (nth (atoi sStr1) anneeList))
  )

) ; defun saveVars

;; Dialog Box
;;;;;;; Fonction principale createCartouche : Ouvre une interface cartouche.dcl qui permet d'entrer des attributs pour le dessin courant.
;
; avant l ouverture, récupère les variables par défauts dans les champs personnalisés (set_tile (GetCustomNotNil
; start_list pour les popup list
; action_tile pour les editbox : permettra la sauvegarde de ces champs! important
;
; a la fermeture de l'interface : saveVars (sauve les variable des popup listes), puis modifie les champs personnalisés
; du dessin (SetCustomByKeyT) et enfin crée (createPres) ou modifie la présentation
;Enfin, (Regen) régénère le dessin pour tout mettre à jour
(defun c:ccar ( / *error* dcl_id )

  ; gestion erreur
  (defun *error* ( msg )
        (if osm (setvar 'osmode osm))
        (if (not (member msg '("Function cancelled" "quit / exit abort")))
            (princ (strcat "\nErreur: " msg))
        )
        (princ)
  )

  (setq osm (getvar 'osmode)) ;; sauvegarde des variables système
  (setvar 'osmode 0)
  ;;;
  
  (setq gabarit "CartoucheMesuralpes.dwt") ; gabarit cartouche dans le dossier des templates
  (setq page "TOP_A1_00200_01-01-2017_MODELE")
    
  (setq dcl_id (load_dialog "cartouche.dcl"))		;load dialog
      
  (if (not (new_dialog "cartouche" dcl_id)		;test for dialog
 
      );not
    (exit)						;exit if no dialog
 
  );if
 
  ;; set default values
  (or *acdoc*
      (setq *acdoc* (vla-get-ActiveDocument (vlax-get-acad-object)))
  )
  (setq target (vla-get-SummaryInfo *acdoc*)) ; dessin courant
  
  ; on récupère les valeurs des propCustom, si elles existent
  (set_tile "Numero" (setq Numero (GetCustomNotNil "Numero" target)))
  (set_tile "Departement" (setq Departement (GetCustomNotNil "Departement" target)))
  (set_tile "Commune" (setq Commune (GetCustomNotNil "Commune" target)))
  (set_tile "CommuneDeleguee" (setq CommuneDeleguee (GetCustomNotNil "CommuneDeleguee" target)))
  (set_tile "Lieu-dit" (setq Lieu-dit (GetCustomNotNil "Lieu-dit" target)))
  (set_tile "Section" (setq Section (GetCustomNotNil "Section" target)))
  (set_tile "Parcelle" (setq Parcelle (GetCustomNotNil "Parcelle" target)))
  (set_tile "Objet1" (setq Objet1 (GetCustomNotNil "Objet1" target)))

  ; set the popup lists
  (popupVar) ; définition des listes déroulantes
  
  (start_list "Bureau")				;start the list box
  (mapcar 'add_list burList)				;fill the list box
  (end_list)
  (set_tile "Bureau" (GetIndex "Bureau" target burList))
  
  (start_list "Planimetrie" 3)				;start the list box
  (mapcar 'add_list planiList)				;fill the list box
  (end_list)
  (set_tile "Planimetrie" (GetIndex "Planimetrie" target planiList))
  
  (start_list "PlaniMethode")				;start the list box
  (mapcar 'add_list mPlaniList)				;fill the list box
  (end_list)
  (set_tile "PlaniMethode" (GetIndex "PlaniMethode" target mPlaniListC))

  (set_tile "PlaniDate" (setq PlaniDate (substr (GetCustomNotNil "PlaniDate" target) 4)))
  (if (= "1" (GetIndex "PlaniMethode" target mPlaniListC)) ; optionnel. seulement si choix Teria
     (mode_tile "PlaniDate" 0)
     (mode_tile "PlaniDate" 1)
  )
  
  (start_list "CoordTransformees")				;start the list box
  (mapcar 'add_list transfoList)				;fill the list box
  (end_list)
  (set_tile "CoordTransformees" (GetIndex "CoordTransformees" target transfoList))
  
  (start_list "Altimetrie")				;start the list box
  (mapcar 'add_list altiList)				;fill the list box
  (end_list)
  (set_tile "Altimetrie" (GetIndex "Altimetrie" target altiList))
  
  (start_list "AltiMethode")				;start the list box
  (mapcar 'add_list mAltiList)				;fill the list box
  (end_list)
  (set_tile "AltiMethode" (GetIndex "AltiMethode" target mAltiListC))

  (set_tile "RN" (setq RN (GetCustomNotNil "RN" target)))  ; optionnel. Seulement si choix sur RN
  (if (= "sur RN n° " (GetCustomNotNil "AltiMethode" target)) ;"3" (GetIndex "AltiMethode" target mAltiListC))

       (mode_tile "RN" 0)
       (mode_tile "RN" 1)
     )

  ;;;;;; propre à la présentation
  (setq presList (append '("Nouvelle") (layoutlist)))
  (start_list "Presentation")				;start the list box
  (mapcar 'add_list presList)				;fill the list box
  (end_list)
  (set_tile "Nouv" (setq Nouv "1"))
  (set_tile "Presentation" (GetIndex "TitrePres" target presList)); "0");  par défaut propose une nouvelle présentation
  (set_tile "Template" gabarit);  toujours par défaut
  (set_tile "Onglet" page);  toujours par défaut
  
  (start_list "Plan")				;start the list box
  (mapcar 'add_list planList)				;fill the list box
  (end_list)
  (set_tile "Plan" (GetIndex "Type" target planListT))

  (set_tile "Indice" (setq Indice (GetCustomNotNil "Indice" target)))
  (set_tile "Planche" (setq Planche (GetCustomNotNil "Planche" target)))
  (set_tile "Echelle" (setq Echelle (GetCustomNotNil "Echelle" target)))

  (setq Date (GetCustomNotNil "Date" target))
  (start_list "DateJ")			
  (mapcar 'add_list jourList)			
  (end_list)
  (set_tile "DateJ" (GetIndexV (substr Date 1 2) jourList))
  (start_list "DateM")			
  (mapcar 'add_list moisList)			
  (end_list)
  (set_tile "DateM" (GetIndexV (substr Date 4 2) moisList))
  (start_list "DateA")			
  (mapcar 'add_list anneeList)			
  (end_list)
  (set_tile "DateA" (GetIndexV (substr Date 7 4) anneeList))

  ; test si sur onglet présentation on remplit les champs automatiquement
  (if (getvar "ctab")
    (progn
 	(setq val (getvar "ctab"))
        (if ( /= "Model" val) ; si on n'est pas sur l'onglet Objet
	  (progn
	  	(set_tile "Presentation" (GetIndexV val presList)) ; on se met dans la liste déroulante sur la présentation en cours
	  	(FillInfoPres)) ; si présentation selon gabarit, on remplit les informations
	  )
      ))
 
  (set_tile "Lot" (setq Lot (GetCustomNotNil "Lot" target)))  ; optionnel. Seulement si choix DP10
  (if (or (= "DP0" (GetCustomNotNil "Type" target)) (= "DP3" (GetCustomNotNil "Type" target))) 

       (mode_tile "Lot" 0)
       (mode_tile "Lot" 1)
     )
  
  
  ; get the values !! important de sauvegarder les champs des edit_box
  (action_tile "Numero" "(setq Numero $value)")
  (action_tile "Bureau" "(setq Bureau $value)")
  (action_tile "Departement" "(setq Departement $value)")
  (action_tile "Commune" "(setq Commune $value)")
  (action_tile "CommuneDeleguee" "(setq CommuneDeleguee $value)")
  (action_tile "Lieu-dit" "(setq Lieu-dit $value)")
  (action_tile "Section" "(setq Section $value)")
  (action_tile "Parcelle" "(setq Parcelle $value)")
  (action_tile "Objet1" "(setq Objet1 $value)")

  (action_tile "Indice" "(setq Indice $value)")
  (action_tile "Planche" "(setq Planche $value)")
  (action_tile "Echelle" "(setq Echelle $value)")

  (action_tile "PlaniMethode" ; active ou désactive le champs PlaniDate

    "(if (= $value \"1\")

       (mode_tile \"PlaniDate\" 0)
       (mode_tile \"PlaniDate\" 1))"
  )
  (action_tile "PlaniDate" "(setq PlaniDate $value)")
  
  (action_tile "AltiMethode" ; active ou désactive le champs RN

    "(if (= $value \"3\")

       (mode_tile \"RN\" 0)
       (mode_tile \"RN\" 1))"
  )
  (action_tile "RN" "(setq RN $value)")

  (action_tile "Presentation" ; on remplit automatiquement les champs selon le titre de la présentation sauf si Nouvelle

    "(if (/= $value \"0\") 
       (FillInfoPres))"
  )
  
  (action_tile "Plan" ; active ou désactive le champs Lot

    "(if (or (= $value \"25\") (= $value \"26\"))  

       (mode_tile \"Lot\" 0)
       (mode_tile \"Lot\" 1))"
  ); cas DP10 ou Pochettes dp1 dp9 dp10
  (action_tile "Lot" "(setq Lot $value)")

  ; présentation gabarit
    
  (action_tile "Nouv" "(setq Nouv $value)")
  (action_tile "Template" "(setq Template $value)")
  (action_tile "Onglet" "(setq Onglet $value)")
  (if (not Template)
    (setq Template gabarit))
  (if (not Onglet)
    (setq Onglet page))


  (action_tile
  "cancel"						;if cancel button pressed
  "(done_dialog) (setq userclick nil)"		;close dialog, set flag
  );action_tile
 
  (action_tile
    "accept"						;if O.K. pressed
    "(saveVars) (done_dialog)(setq userclick t))"	;close dialog, set flag - fonction saveVars pour sauver les var des popup lists
  );action tile

  
  (start_dialog)					;start dialog
 
  (unload_dialog dcl_id)				;unload

  (if (not userclick)
    (exit)
  )

  ;;;;;;;;;;;; set the values in custom properties

  ;; Bureau
  (cond
  ((= Bureau "ALBERTVILLE") ; albertville
   (progn
     ;(setq Bureau "ALBERTVILLE")
     (setq Bur "ALB")
     (setq Adresse "60 av. des Chasseurs Alpins - 73200 Albertville")
     (setq Tel "04 79 32 10 61")
     (setq Mail "albertville@mesuralpes.fr")   
		  ))
  ((= Bureau "MOÛTIERS") ; moutiers
   (progn
     ;(setq Bureau "MOÛTIERS")
     (setq Bur "MTS")
     (setq Adresse "131 rue des Grillons - 73600 Moûtiers")
     (setq Tel "04 79 24 15 42")
     (setq Mail "moutiers@mesuralpes.fr")   
		  ))
  ((= Bureau "SAINT-JEAN") ; st jean
  (progn
     ;(setq Bureau "SAINT-JEAN")
     (setq Bur "STJ")
     (setq Adresse "205 rue de la Libération - 73302 St Jean de Maurienne")
     (setq Tel "04 79 64 06 01")
     (setq Mail "stjean@mesuralpes.fr")   
		  ))
   
   (T  (progn  ; Aime
     (setq Bureau "AIME LA PLAGNE")
     (setq Bur "AIM")
     (setq Adresse "252 Grande Rue - 73210 Aime La Plagne")
     (setq Tel "04 79 55 69 23")
     (setq Mail "aime@mesuralpes.fr")   
		  ))
  )

  ;; Case upper lower
  (setq Departement (Case Departement 0)) ; Tout en majuscule
  ;(setq Commune (Case Commune 2)) ; Une majuscule à chaque mot
  ;(setq CommuneDeleguee (Case CommuneDeleguee 2)) ; Une majuscule à chaque mot
  ;(setq Lieu-dit (Case Lieu-dit 2)) ; Une majuscule à chaque mot
  (setq Section (Case Section 0)) ; Tout en majuscule
  
  (SetCustomByKeyT "Bureau" Bureau target)
  (SetCustomByKeyT "Bur" Bur target)
  (SetCustomByKeyT "Adresse" Adresse target)
  (SetCustomByKeyT "Tel" Tel target)
  (SetCustomByKeyT "Mail" Mail target)
  
  (SetCustomByKeyT "Numero" Numero target)
  (SetCustomByKeyT "Departement" Departement target)
  (SetCustomByKeyT "Commune" Commune target)
  
  ; Optional lines
  (SetCustomByKeyT "CommuneDeleguee" (Case CommuneDeleguee 2) target)
  (if (isEmpty CommuneDeleguee)
    (SetCustomByKeyT "CommuneDeleguee1" " " target)
    (SetCustomByKeyT "CommuneDeleguee1" (strcat "Commune déléguée de " CommuneDeleguee)  target)
    )
  
    (SetCustomByKeyT "Lieu-dit" Lieu-dit target)
  (if (isEmpty Lieu-dit)
    (SetCustomByKeyT "Lieu-dit1" " " target)
    (SetCustomByKeyT "Lieu-dit1" (strcat "Lieu-dit \"" Lieu-dit "\"")  target)
    )
  
    (SetCustomByKeyT "Section" Section target)
  (if (isMultiple Section)
    (setq sec "Sections ")
    (setq sec "Section "))
  (if (isEmpty Section)
    (SetCustomByKeyT "Section1" " " target)
    (SetCustomByKeyT "Section1" (strcat sec Section)  target)
    )
  
  (SetCustomByKeyT "Parcelle" Parcelle target)
  (if (isMultiple Parcelle)
    (setq par "Parcelles ")
    (setq par "Parcelle "))
  (if (isEmpty Parcelle)
    (SetCustomByKeyT "Parcelle1" " " target)
    (SetCustomByKeyT "Parcelle1" (strcat par Parcelle)  target)
    )
  
   (SetCustomByKeyT "Objet1" Objet1 target)
;;;  (if (isEmpty Objet)
;;;    (SetCustomByKeyT "Objet1" " " target)
;;;    ;(SetCustomByKeyT "Objet1" (strcat "Objet " Objet)  target)
;;;    )
  
 
  (SetCustomByKeyT "Planimetrie" Planimetrie target)
  (SetCustomByKeyT "PlaniMethode" PlaniMethode target)
  (if (and (= "1" (GetIndex "PlaniMethode" target mPlaniListC)) (not (isEmpty PlaniDate)))
  	(SetCustomByKeyT "PlaniDate" (strcat "le " PlaniDate) target)
        (SetCustomByKeyT "PlaniDate" " " target)
  )
  
  (SetCustomByKeyT "CoordTransformees" CoordTransformees target)
  (if (equal CoordTransformees " ")
      (SetCustomByKeyT "CoordTransformees1" " " target)
      (SetCustomByKeyT "CoordTransformees1" (strcat "Coordonnées planimétriques transformées en " CoordTransformees " à la demande du client"   ) target)
    )

  
  (SetCustomByKeyT "Altimetrie" Altimetrie target)
  (SetCustomByKeyT "AltiMethode" AltiMethode target)
  (if (= "sur RN n° " (GetCustomNotNil "AltiMethode" target));(= "3" (GetIndex "AltiMethode" target mAltiListC)) 
  	(SetCustomByKeyT "RN" RN target)
        (SetCustomByKeyT "RN" " " target)
  )
  (SetCustomByKeyT "Lot" Lot target)
  
  (if (or (="DP3" Typ) (="DP0" Typ)) ; transformer le titre du plan dans le cas d'une DP10. Nombre de lots à créer
    (setq Plan (planDP10 Lot))
    )
  
  ; on sauvegarde les propriétés de la dernière présentation créée
  (SetCustomByKeyT "Plan" Plan target)
  (SetCustomByKeyT "Type" Typ target)
  
  (setq Indice (Case Indice 0)) ; Indice Majuscule
  (if (isEmpty Indice) 
    (setq Indice "X"))
  (if (isEmpty Planche) 
    (setq Planche "X"))
  (SetCustomByKeyT "Indice" Indice target)
  (SetCustomByKeyT "Planche" Planche target)

  (setq Echelle (noSpaces Echelle))
  (SetCustomByKeyT "Echelle" Echelle target)
  ; transforme l'échelle en un string de 5 caractères
  (setq nb (- 5 (strlen Echelle)))
  (repeat nb
     (setq Echelle (strcat "0" Echelle))
    )

  (setq Date (strcat DateJ "-" DateM "-" DateA))
  (SetCustomByKeyT "Date" Date target)
  (setq TitrePres (strcat Typ "_" Indice Planche "_" Echelle "_" Date "_" Plan))
  (SetCustomByKeyT "TitrePres" TitrePres target)

  ; Renommer ou créer une nouvelle présentation

(cond
  ((= Presentation "Nouvelle")
   (progn
     (if (member TitrePres (layoutlist))
  	(alert "Cette présentation existe déjà, elle ne sera donc pas créée")
        (progn
	 (if (/="DP3" Typ) 
        	(CreatePres TitrePres Template Onglet Nouv)
	 	(progn  ; création des 4 onglets DP
		  (princ "creation pochettes dp")
		  	(setq TemplateDP (vl-string-subst "DP." "." Template))
	   		(CreatePres "POCHETTES DP" TemplateDP "POCHETTES DP" Nouv)
		  	(CreatePres (strcat "DP1_" Indice Planche "_" Echelle "_" Date "_PLAN DE SITUATION") TemplateDP "DP1_A1_00000_01-01-2017_PLAN DE SITUATION" Nouv)
		  	(CreatePres (strcat "DP9_" Indice Planche "_" Echelle "_" Date "_PLAN DES LIEUX" ) TemplateDP "DP9_A1_00000_01-01-2017_PLAN DES LIEUX" Nouv)
		  	(CreatePres (strcat "DP0_" Indice Planche "_" Echelle "_" Date "_" Plan) TemplateDP "DP0_A1_00000_01-01-2017_CRÉATION DE ## LOTS À BÂTIR" Nouv)
	   	)
		); if /=DP3 
	 ); progn
     )
    )
   )
  (t (progn ; pas de nouvelle présentation. on renomme simplement
         (setq ocmd (getvar "cmdecho"))
    	 (setvar "CMDECHO" 0)
       
       (if (AND (/= TitrePres Presentation) (member TitrePres (layoutlist)) )
	 (alert "Cette présentation existe déjà, la présentation choisie ne sera pas modifiée")
	 (progn 
       	 (command "_.layout" "R"  Presentation TitrePres) ; renomme
	 (command "_.layout" "e" TitrePres) ; rend la présentation courante
		  )
	 )
    	(setvar "CMDECHO" ocmd)
       ))
  )
   
  (Regen)
  (command "FILEDIA" "1")
  (setvar 'osmode osm) ; restore les variables systèmes
  (princ "Fin Create Cartouche")
    (princ)
 
);defun ccar createCartouche


; Crée une nouvelle présentation au titre "titre"
; si n = "1" elle la crée à partir de la page p du gabarit gab si existe
; (gabarit est le nom d un dwt présent dans le dossiers de gabarit par défaut
(defun CreatePres (titre gab p n)
  (setq ocmd (getvar "cmdecho"))
    	 (setvar "CMDECHO" 0) ; 0 pour ne pas voir les comments

;;;   (setq doc (GetItem
;;;                     (vla-get-Documents (vlax-get-acad-object))
;;;                     (strcat (vl-filename-base filename) ".dwg")
;;;         )
;;;   )

  ; TODO verifier que gabarit et page existent
  ;(setq layouts (vla-get-Layouts *acdoc*))
  ;(setq layoutObj (vla-Add layouts titre))
  ;(setq layoutObj (vla-item (vla-get-layouts *acdoc*) titre))
  ;(vla-put-activelayout *acdoc* layoutObj)

  (if (= "0" n) ; on n'utilise pas le gabarit
    (vla-Add (vla-get-Layouts *acdoc*) titre)
    (progn
  
	  (if (member p (layoutlist)) ; si la presentation modele existe déjà on la renomme temporairement
	    (command "_.layout" "R"  p "template"))
	  
	  (command "FILEDIA" "0") ; pour qu'il n'y ai pas de fenêtre qui s'ouvre
	  (command "_.layout" "g"  gab p "") ; import de la presentation du gabarit
	 
	  (if (member p (layoutlist))  
	    (command "_.layout" "r" p titre) ; l'import a fonctionné, on renomme l'onglet
	    (command "._u" "_.layout" "n" titre) ; l'import n'a pas fonctionné, on crée juste un nouvel onglet
	    )
	  
	   
	  (command "FILEDIA" "1")

	  (if (member "template" (layoutlist))
	    (command "_.layout" "R"  "template" p))	  
      ) ; fin else
    )
  (command "_.layout" "e" titre) ; rendre la nouvelle présentation courante
  (setvar "CMDECHO" ocmd)
  (command "FILEDIA" "1")
) ; defun Create

; Indique à quel indice est stocké la propCustom du dessin courant dans la liste
; Renvoie un string 
; si la valeur n'y est pas renvoie "0"
(defun GetIndex (key target liste)
    
  (setq val (GetCustomNotNil key target)) ; val est un string
  (GetIndexV val liste)
)

; Indique à quel indice est stockée la valeur val dans la liste
; si la valeur n'est pas présente renvoie "0"
(defun  GetIndexV (val liste)
  (setq index (vl-position val liste))
  (if (not index)
    (setq index 0)
    )

  (setq index (itoa index))
  index
)

; Regen pour régénérer le dessin
(defun Regen()
    (setq acadObj (vlax-get-acad-object))
    (setq doc (vla-get-ActiveDocument acadObj))
  
    ;; The following example regenerates the complete drawing
    (vla-Regen doc acAllViewports)
    
    ;; The following example regenerates the active viewport in the current drawing
    ;(vla-Regen doc acActiveViewport)
)

; remplit automatiquement les informations de la présentation sélectionnée.
; Par défaut c'est la présentation choisie dans le menu déroulant
; quand on sélectionne une présentation dans le menu déroulant, remplit automatiquement les champs correspondants :
; indice, planche, échelle, type de plan et date d'édition (si présents dans le titre)
(defun FillInfoPres ()
  (setq sStr1(get_tile "Presentation")) ; nom de la présentation
  (if(= sStr1 "")
    (setq pres nil)
    (setq pres (nth (atoi sStr1) presList))
  )
  (FillInfoPresV pres)
)
  
(defun FillInfoPresV (pres)
  (if (> (strlen pres) 25) 
	(if (= (ascii "_") (vl-string-elt pres 3))  ; titre selon modèle ccar
	    (progn
		  (set_tile "Plan" (GetIndexV (substr pres 1 3) planListT))

		  (set_tile "Indice" (setq Indice (substr pres 5 1)))
		  (set_tile "Planche" (setq Planche (substr pres 6 1)))
	      
		  (set_tile "Echelle" (setq Echelle (itoa (atoi (substr pres 8 5)))))

		  (set_tile "DateJ" (GetIndexV (substr pres 14 2) jourList))
		  (set_tile "DateM" (GetIndexV (substr pres 17 2) moisList))
		  (set_tile "DateA" (GetIndexV (substr pres 20 4) anneeList))	      
	     )
    )
  )
)

; change la casse d'une chaine de caractère
; which 0 met en majuscules
; which 1 met en minuscule
; which 2 met une majuscule à chaque mot
; sinon met une majuscule au début puis normal la suite
(defun Case(string which)
   (cond
  ((= which 0) ; to upper
   (strcase string nil))
   ((= which 1) ; to lower
   (strcase string T))
   ((= which 2) ; une majuscule à chaque mot
    ( words2caps string)
    )
   (T  (progn   ; Majuscule au début
	 (strcat (strcase (substr string 1 1) nil)
	 (strcase (substr string 2) T) )
	))
  )
  ; cas majuscule à chaque mot?
 ; string
); defun Case

; Met une majuscule à chaque mot
(defun words2caps (text / i c w r)
  (setq i 0 w "")
  (repeat (strlen text)
  (if (wcmatch (setq c (substr text (setq i (1+ i)) 1)) "~@")
  (setq r (cons c (cons (strcase w t) r)) w "")
  (setq w (strcat w c))
  )
  )
  (apply 'strcat
  (mapcar
  '(lambda (x) (strcat (strcase (substr x 1 1)) (substr x 2)))
  (reverse (cons (strcase w t) r))
  )
  )
) ; defun words2caps

; enlève les espaces d'une chaine de caractères
(defun noSpaces (str)
  (if (eq (type str) 'STR)
    (vl-list->string (vl-remove 32 (vl-string->list str)))
  )
)

; défini si il y a des espaces, virgules, points, tirets, points virgules dans une chaine de caractères. retourne nil si il n'y en a pas, true sinon
(defun isMultiple (str)
  (if (not str)
    (progn
      nil
      exit
      )
    )
 (setq str (vl-string-trim " " str)) ; suppression des espaces au début et à la fin
 (setq int (vl-string-position (ascii "-") str))
  (if (or (vl-string-position (ascii " ") str) (vl-string-position (ascii "-") str) (vl-string-position (ascii ",") str) (vl-string-position (ascii ".") str) (vl-string-position (ascii ";") str) (vl-string-position (ascii "/") str))
    T
    nil
    )
)
  

;; défini si le string str est vide.
;; retourne True si str est vide (des espaces ou bien nul)
;; retourne nil sinon
(defun isEmpty (str)
  (setq empty T)
  
  (if str ; check that str not nil
    (progn
	  (setq strE (noSpaces str)) ; remove spaces
	  (if (/= 0 (strlen strE))
		 (setq empty nil)
	  )    
	)
    )
  empty
  )

;;; crée le titre de la présentation DP10 selon le nombre de lots
; prend en argument le nombre de lots
(defun planDP10 (lot)
  (setq lot (noSpaces (Case lot 0)))
  (if (= "UN" lot)
  	(setq plan "CRÉATION D'UN LOT À BÂTIR")
   	(setq plan (strcat "CRÉATION DE " lot " LOTS À BÂTIR"))
    )
 )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Create hashTable for popup list
(defun popupVar ()

  (setq burList (list "AIME LA PLAGNE" "ALBERTVILLE" "MOÛTIERS" "SAINT-JEAN"))
  
  (setq planList (list ; menu déroulant
"IMPLANTATION" ;0
"PRÉ IMPLANTATION" ;1
"TOPOGRAPHIQUE" ;2
"RECOLEMENT" ;3
"VUE EN PLAN" ;4
"REPERAGE" ;5
"ETAT DES LIEUX" ;6
"BORNAGE" ;7
"DIVISION";8
"PROJET DIVISION" ;9
"PROFIL EN LONG" ;10
"PROFIL EN TRAVERS" ;11
"COPROPRIETE" ;12
"INTERIEUR" ;13
"VOLUME" ;14
"FACADE" ;15
"COUPE" ;16
"SITUATION" ; 17
"PARCELLAIRE"; 18
"PERIMETRE DUP"; 19
"PERIMETRE SUP"; 20
"TRAVAUX"; 21
"SERVITUDE";22
"DP1";23
"DP9";24
"DP10";25
"DP1 DP9 DP10";26
"PIQUETAGE";27
"EPURE";28
"ALIGNEMENT";29
 ))
  
  ;LISTE TITRE POUR LE CARTOUCHE
    (setq planListC (list 
"PLAN D'IMPLANTATION" ;0
"PLAN DE PRÉ IMPLANTATION" ;1
"PLAN TOPOGRAPHIQUE" ;2
"PLAN DE RÉCOLEMENT" ;3
"VUE EN PLAN" ;4
"PLAN DE REPÉRAGE" ;5
"PLAN D'ÉTAT DES LIEUX" ;6
"PLAN DE BORNAGE" ;7 
"PLAN DE DIVISION";8
"PROJET DE DIVISION" ;9
"PROFIL EN LONG" ;10
"PROFIL EN TRAVERS" ;11
"PLAN DE COPROPRIÉTÉ" ;12
"PLAN D'INTERIEUR" ;13
"PLAN DE DIVISION EN VOLUMES" ;14
"PLAN DE FAÇADE" ;15
"COUPE" ;16
"PLAN DE SITUATION";17
"PLAN PARCELLAIRE";18
"PÉRIMÈTRE DE LA DUP"; 19
"PÉRIMÈTRE DE LA SUP"; 20
"PLAN GÉNÉRAL DES TRAVAUX"; 21
"PLAN DE SERVITUDE"; 22
"PLAN DE SITUATION";23
"PLAN DES LIEUX";24
"CRÉATION D'UN LOT À BÂTIR";25
"POCHETTES DP";26
"PLAN DE PIQUETAGE";27
"ÉPURE D'IMPLANTATION";28
"ALIGNEMENT";29
))

  (setq planListT (list ; type 3 lettres
"IMP" ;0
"PRI" ;1
"TOP" ;2
"REC" ;3
"VPL" ;4
"REP" ;5
"EDL" ;6
"BOR" ;7
"DIV" ;8
"PJD" ;9
"PRL" ;10
"PRT" ;11
"COP" ;12
"INT" ;13
"VOL" ;14
"FAC" ;15
"CPE" ;16
"SIT" ;17
"PAR" ;18
"PRD" ;19
"PRS" ;20
"TRA" ;21
"SRV";22
"DP1";23
"DP9";24
"DP0";25
"DP3";26
"PIQ";27
"EPU";28
"ALI";29
))		   

  
  (setq planiList (list
"Indépendante" ;0
"LAMBERT I" ;1
"LAMBERT II" ;2
"LAMBERT III" ;3
"LAMBERT 93 CC41" ;4
"LAMBERT 93 CC42" ;5
"LAMBERT 93 CC43" ;6
"LAMBERT 93 CC44" ;7
"LAMBERT 93 CC45" ;8
"LAMBERT 93 CC46" ;9
"LAMBERT 93 CC47" ;10
"LAMBERT 93 CC48" ;11
"LAMBERT 93 CC49" ;12
"LAMBERT 93")) ;13

  
 ; (setq mPlaniList '("Aucun" "TERIA"))
  (setq mPlaniList (list " " "TERIA"))
   (setq mPlaniListC (list " " "par GNSS sur le réseau TERIA"))
  
  (setq transfoList (list " " "Lambert III"))
  
  (setq altiList (list " " "Indépendante" "NGF-IGN69" "NGF-IGN78" ))

  
  (setq mAltiList (list " " "TERIA Metropole" "TERIA Corse" "sur RN"))
  (setq mAltiListC (list " " "par GNSS sur le réseau TERIA avec la grille RAF09"
		     "par GNSS sur le réseau TERIA avec la grille RAC09"
		     "sur RN n° ")) ; a completer

  ; Date calendrier
  (setq i 31)
  (setq jourList nil)
  (repeat 31
    (setq si (itoa i))
    (if (< i 10)
      (setq si (strcat "0" si))
      )
    (setq jourList (cons si jourList ))
    (setq i (- i 1))
  )

  (setq i 12)
  (setq moisList nil)
  (repeat 12
    (setq si (itoa i))
    (if (< i 10)
      (setq si (strcat "0" si))
      )
    (setq moisList (cons si moisList ))
    (setq i (- i 1))
  )

  (setq i 2025)
  (setq anneeList nil)
  (repeat 10
    (setq anneeList (cons (itoa i) anneeList ))
    (setq i (- i 1))  
  )

) ; fin defun popupVar



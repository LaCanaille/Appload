cartouche : dialog {				//dialog name
      label = "Personnalisation du cartouche" ;		//give it a label

	: row {

	: boxed_column {			//define boxed column
        label = "&Dossier";			//give it a label

        // Dossier
        : edit_box
	{
	label = "Dossier n° (9999-16) :";
	//mnemonic = "N";
	key = "Numero";
	alignment = centered;
	edit_limit = 10;
	edit_width = 10;
	}

	// Bureau
	: row {

	: text_part {				//define more text
     	label = " Bureau :";			//some more text
     	}						//end text
        : popup_list {				//define popup list
        key = "Bureau";			//give it a name
        //value = "0" ;				//initial value
        }					//end list

        
        }
 
	// Lieu
	: edit_box
	{
	label = "Département :";
	key = "Departement";
	alignment = centered;
	edit_limit = 30;
	edit_width = 30;
	}

	: edit_box
	{
	label = "Commune :";
	key = "Commune";
	alignment = centered;
	edit_limit = 30;
	edit_width = 30;
	}

	: edit_box
	{
	label = "Commune déléguée de :";
	key = "CommuneDeleguee";
	alignment = centered;
	edit_limit = 30;
	edit_width = 30;
	}

	: edit_box
	{
	label = "Lieu-dit :";
	key = "Lieu-dit";
	alignment = centered;
	edit_limit = 30;
	edit_width = 30;
	}
	
	: row {
	: edit_box
	{
	label = "Section :";
	key = "Section";
	alignment = left;
	edit_limit = 10;
	edit_width = 10;
	}

	: edit_box
	{
	label = "Parcelle :";
	key = "Parcelle";
	alignment = left;
	edit_limit = 10;
	edit_width = 10;
	}
	}

	: edit_box
	{
	label = "Objet :";
	key = "Objet1";
	alignment = centered;
	edit_limit = 60;
	edit_width = 60;
	}
 
        }					//fin colonne de gauche
         

//// Rattachement. colonne de droite
        : boxed_column {			//define boxed column
        label = "&Rattachement";			//give it a label

        : row {

	: text_part {			
     	label = " Planimétrie :";			
     	}					
        : popup_list {			
        key = "Planimetrie";
        width = 35;
        }				
        }

        : row {

	: text_part {			
     	label = " Méthode :";			
     	}					
        : popup_list {			
        key = "PlaniMethode";
        width = 15;
        }
        spacer_0;
        spacer_0;
        spacer_0;
        spacer_0;
         // date rattachement        
     	: edit_box
	{
	label = "le (jj/mm/aaaa):";
	key = "PlaniDate";
	alignment = centered;
	edit_limit = 11;
	edit_width = 11;
	is_enabled = false;
	}
     	
        }

        : row {

	: text_part {			
     	label = " Transformées en :";			
     	}					
        : popup_list {
        key = "CoordTransformees";
        width = 30;
        }				
        }

        : row {

	: text_part {			
     	label = " Altimétrie :";			
     	}					
        : popup_list {			
        key = "Altimetrie";
        width = 30;
        }
        }

        : row {
	: text_part {			
     	label = " Méthode :";			
     	}					
        : popup_list {			
        key = "AltiMethode";
        width = 20;
        }
        : edit_box
	{
	label = "RN n° :";
	key = "RN";
	alignment = centered;
	edit_limit = 20;
	edit_width = 20;
	is_enabled = false;
	}
        }
          

        
        } // fin colonne de droite
 


	} // end row

	
	: boxed_column{
	label = "&Présentation";
	
	: row {
	: text_part {			
     	label = " Présentation :";			
     	}
	: popup_list {			
        key = "Presentation";
        width = 50;
        }
        	
	spacer_0;
	spacer_0;
	spacer_0;
	spacer_0;
		
	spacer_0;
	spacer_0;
	spacer_0;
	spacer_0;
        : toggle {
        key = "Nouv";
        //value = "1"; 
//        label = "Vérifié par :";
        }
     	: edit_box
	{
	label = "à partir du gabarit du dossier Template :";
	key = "Template";
	alignment = centered;
	edit_limit = 30;
	edit_width = 23;
	is_enabled = true;
	}
	spacer_0;
	spacer_0;
	spacer_0;
	spacer_0;
	: edit_box
	{
	label = "onglet :";
	key = "Onglet";
	alignment = centered;
	edit_limit = 30;
	edit_width = 30;
	is_enabled = true;
	}	
        }
        
	: row {
        : text_part {			
     	label = " Type de plan :";			
     	}
	: popup_list {			
        key = "Plan";
        width = 30;
        }
        
        }
	
        
	: row {
	: edit_box
	{
	label = "Indice (Lettre) :";
	key = "Indice";
	alignment = right;
	edit_limit = 1;
	edit_width = 2;
	width = 5;
	fixed_width = true;
	}
	
	: edit_box
	{
	label = "Planche (Numéro) :";
	key = "Planche";
	alignment = right;
	edit_limit = 1;
	edit_width = 2;
	width = 5;
	fixed_width = true;
	}

	: edit_box
	{
	label = "Echelle 1/";
	key = "Echelle";
	alignment = right;
	edit_limit = 5;
	edit_width = 5;
	width = 5;
	fixed_width = true;
	}

	: edit_box
	{
	label = "DP10 : Nombre de lots (en lettres)";
	key = "Lot";
	alignment = right;
	edit_limit = 25;
	edit_width = 25;
	width = 5;
	fixed_width = true;
	}
	
	spacer_0;
	spacer_0;
	spacer_0;
	spacer_0;
		
	spacer_0;
	spacer_0;
	spacer_0;
	spacer_0;

	: text_part {			
     	label = " Date d'édition :";			
     	}					
        : popup_list {			
        key = "DateJ";
        }
        : popup_list {			
        key = "DateM";		
        }
        : popup_list {			
        key = "DateA";
        }

        }
        : text_part {			
     	label = "";
     	}
        : text_part {			
     	label = "\nNB : Si une nouvelle présentation est créée, elle le sera à partir du gabarit CartoucheMesuralpes.dwt localisé dans le dossier Template, en utilisant l'onglet TOP_A1_00200_01-01-2017_MODELE";			
     	}

//        : row {
//        : toggle {
//        key = "Check";				//give it a name
//        label = "Vérifié par :";
//        }
//        : edit_box
//	{
//	key = "CheckBy";
//	edit_limit = 30;
//	edit_width = 30;
//	}
//	spacer_0;
//	spacer_0;
//	spacer_0;
//	spacer_0;
//	spacer_0;
//	spacer_0;
//	spacer_0;
//	spacer_0;
//	spacer_0;
//	spacer_0;
//	spacer_0;
//	spacer_0;
//	spacer_0;
//	spacer_0;
//	spacer_0;
//	spacer_0;
//	spacer_0;
//	spacer_0;
//        }
	
	}

 
     ok_cancel ;				//predifined OK/Cancel
     					
     }						//end dialog
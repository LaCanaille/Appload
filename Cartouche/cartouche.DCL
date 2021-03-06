// V4 07/12/17

cartouche : dialog {				//dialog name
      label = "Personnalisation du cartouche" ;		//give it a label

// Dossier //
           : row {
           : boxed_column {			//define boxed column
        label = "&Dossier";			//give it a label

        // Dossier
        : edit_box
	{
	label = "Dossier n� (9999-16) :";
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
	label = "D�partement :";
	key = "Departement";
	alignment = centered;
	edit_limit = 50;
	edit_width = 50;
	}

	: edit_box
	{
	label = "Commune :";
	key = "Commune";
	alignment = centered;
	edit_limit = 50;
	edit_width = 50;
	}

	: edit_box
	{
	label = "Commune d�l�gu�e de :";
	key = "CommuneDeleguee";
	alignment = centered;
	edit_limit = 50;
	edit_width = 50;
	}

	: edit_box
	{
	label = "Lieu-dit :";
	key = "Lieu-dit";
	alignment = centered;
	edit_limit = 50;
	edit_width = 50;
	}
	
	: row {
	: edit_box
	{
	label = "Section(s) :";
	key = "Section";
	alignment = left;
	edit_limit = 30;
	edit_width = 30;
	}

	: edit_box
	{
	label = "Parcelle(s) :";
	key = "Parcelle";
	alignment = right;
	edit_limit = 50;
	edit_width = 50;
	}
	}

	: edit_box
	{
	label = "Objet :";
	key = "Objet1";
	alignment = centered;
	edit_limit = 110;
	edit_width = 110;
	}
 
        }
           
           } // fin dossier


           // Rattachement //
          :row{
          : boxed_column{			//define boxed column
        label = "&Rattachement";			//give it a label

        : row {

	: text_part {			
     	label = " Planim�trie :";			
     	}					
        : popup_list {			
        key = "Planimetrie";
        width = 10;
        }				
        }

        : row {

	: text_part {			
     	label = " M�thode :";			
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
	alignment = right;
	edit_limit = 11;
	edit_width = 11;
	is_enabled = false;
	}
     	
        }

        : row {

	: text_part {			
     	label = " Transform�es en :";			
     	}					
        : popup_list {
        key = "CoordTransformees";
        width = 15;
        }				
        }

        : row {

	: text_part {			
     	label = " Altim�trie :";			
     	}					
        : popup_list {			
        key = "Altimetrie";
        width = 15;
        }
        }

        : row {
	: text_part {			
     	label = " M�thode :";			
     	}					
        : popup_list {			
        key = "AltiMethode";
        width = 20;
        }
        : edit_box
	{
	label = "RN n� :";
	key = "RN";
	alignment = centered;
	edit_limit = 20;
	edit_width = 20;
	is_enabled = false;
	}
        }
          
        
       }
          } // fin rattachement

           // Presentation //

           : row {
           : boxed_column {
           label = "&Pr�sentation";
           
	   : row{
	: text_part {			
     	label = " Pr�sentation :";			
     	}
	: popup_list {			
        key = "Presentation";
        width = 80;
        }
        }
        
        :row {
        	
        : toggle {
        key = "Nouv";
        //value = "1"; 
//        label = "V�rifi� par :";
        }
     	: edit_box
	{
	label = "� partir du gabarit du dossier Template :";
	key = "Template";
	alignment = centered;
	edit_limit = 30;
	edit_width = 23;
	is_enabled = true;
	}
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
        
        : row{
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
	label = "Planche (Num�ro) :";
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


	: text_part {			
     	label = " Date d'�dition :";			
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

        
        :row{
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
        }

           
           } // fin bowed_column
           
           } // fin row presentation

        : text_part {			
     	label = "";
     	}
        : text_part {			
     	label = "\nNB : Si une nouvelle pr�sentation est cr��e, elle le sera � partir du gabarit CartoucheMesuralpes.dwt";			
     	}
     	: text_part {			
     	label = "localis� dans le dossier Template, en utilisant l'onglet TOP_A1_00200_01-01-2017_MODELE";			
     	}
           
           ok_cancel ;				//predifined OK/Cancel
           
     					
     }	
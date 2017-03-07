mult : dialog {				//dialog name
      label = "Multiplier un calque" ;

	: popup_list {
	key = "calque";
	label ="Calque à multiplier";
	width = 60;
	}
        
	: row {
	: edit_box
	{
	label = "Préfixe :";
	key = "prefixe";
	edit_limit = 8;
	edit_width = 8;
	}

	: edit_box
	{
	label = "Nombre :";
	key = "nb";
	edit_limit = 2;
	edit_width = 2;
	}
	}
      
      ok_cancel ;
 }
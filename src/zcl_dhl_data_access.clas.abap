class ZCL_DHL_DATA_ACCESS definition
  public
  final
  create public .

public section.

  methods MODIFY_DB_ACCESS_TOKEN
    importing
      !IS_ACCESS_TOKEN type ZDHL_ACCCS_TOKEN .
protected section.
private section.
ENDCLASS.



CLASS ZCL_DHL_DATA_ACCESS IMPLEMENTATION.


  METHOD modify_db_access_token.

    " Erstellt einen neuen Eintrag, wenn wir das Token zum ersten Mal anfordern
    MODIFY zdhl_acccs_token FROM @is_access_token.

  ENDMETHOD.
ENDCLASS.

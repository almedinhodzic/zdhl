class ZCL_DHL_DATA_PROVIDER definition
  public
  final
  create public .

public section.

  methods GET_DB_ACCESS_TOKEN
    importing
      !IV_DATE type DATS default SY-DATUM
      !IV_API type STRING
      !IV_TIME type TIMS default SY-UZEIT
    returning
      value(RS_ACCESS_TOKEN) type ZDHL_ACCCS_TOKEN .
protected section.
private section.
ENDCLASS.



CLASS ZCL_DHL_DATA_PROVIDER IMPLEMENTATION.


  METHOD get_db_access_token.

    SELECT SINGLE id, access_token, api, expiring_date, expiring_time
      INTO CORRESPONDING FIELDS OF @rs_access_token
      FROM zdhl_acccs_token
      WHERE ( expiring_date GT @iv_date OR ( expiring_date EQ @iv_date AND expiring_time GT @iv_time ) ) AND api = @iv_api.

  ENDMETHOD.
ENDCLASS.

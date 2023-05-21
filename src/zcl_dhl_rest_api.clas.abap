class ZCL_DHL_REST_API definition
  public
  inheriting from ZCL_DHL_REST_CLIENT
  final
  create public .

public section.

  types:
    BEGIN OF ty_attributes ,
        gpv_nr TYPE char10 ,
      END OF ty_attributes .
  types:
    BEGIN OF ty_recipients_attr,
        attributes TYPE ty_attributes,
      END OF ty_recipients_attr .

  constants MC_SERVICE_VERSION type STRING value '1' ##NO_TEXT.

  methods GET_LISTS
    exporting
      !ES_LOG type ZDHL_REST_LOG
    changing
      !CT_DATA type ANY .
  methods GET_RECIPIENTS
    exporting
      !ET_DATA type ZDHL_REST_LOG
      !ES_LOG type ZDHL_REST_LOG
    changing
      !CT_PARAMS type ZDHL_TT_REST_PARAMS optional .
  methods SET_RECIPIENTS_ATTR
    importing
      !ID_GPV_NR type CHAR10
      value(ID_INXMAIL_ID) type CHAR10
    exporting
      !ES_LOG type ZDHL_REST_LOG .
  methods CREATE_ORDER
    importing
      !IV_CUSTOMER_KEY type STRING
      !IV_CUSTOMER_SECRET type STRING
      !IS_ORDER type STRING
    exporting
      !ES_LOG type ZDHL_REST_LOG
      !ES_ORDER type STRING
    changing
      !CT_PARAMS type ZDHL_TT_REST_PARAMS optional .
  methods CONSTRUCTOR .
  methods GET_ORDER
    importing
      !IV_ORDER_ID type STRING
      !IV_CUSTOMER_KEY type STRING
      !IV_CUSTOMER_SECRET type STRING
    exporting
      !ES_ORDER type ZDHL_REST_ORDER_RES
      !ES_LOG type ZDHL_REST_LOG .
  methods GET_ITEM
    importing
      !IV_ITEM_ID type STRING
      !IV_CUSTOMER_KEY type STRING
      !IV_CUSTOMER_SECRET type STRING
    exporting
      !ES_ITEM type ZDHL_REST_ITEM_RES
      !ES_LOG type ZDHL_REST_LOG .
  PROTECTED SECTION.
private section.

  methods _GET_ACCESS_TOKEN
    importing
      !IV_CUSTOMER_SECRET type STRING
      !IV_CUSTOMER_KEY type STRING
    exporting
      !ES_LOG type ZDHL_REST_LOG
      !ES_ACCESS_TOKEN type ZDHL_REST_ACCESS_TOKEN_RES .
  methods _CREATE_ORDER
    importing
      !IS_ORDER type STRING
      !IV_ACCESS_TOKEN type STRING
    exporting
      !ES_LOG type ZDHL_REST_LOG
      !ES_ORDER type DATA .
  methods _REFRESH_ACCESS_TOKEN
    importing
      !IV_ACCESS_TOKEN type STRING
    exporting
      !ES_LOG type ZDHL_REST_LOG .
  methods _GET_ORDER
    importing
      !IV_ORDER_ID type STRING
      !IV_ACCESS_TOKEN type STRING
    exporting
      !ES_LOG type ZDHL_REST_LOG
      !ES_ORDER type ZDHL_REST_ORDER_RES .
  methods _GET_ITEM
    importing
      !IV_ITEM_ID type STRING
      !IV_ACCESS_TOKEN type STRING
    exporting
      !ES_LOG type ZDHL_REST_LOG
      !ES_ITEM type ZDHL_REST_ITEM_RES .
  methods _UPDATE_ACCESS_TOKEN
    importing
      !IV_ACCESS_TOKEN type STRING .
ENDCLASS.



CLASS ZCL_DHL_REST_API IMPLEMENTATION.


  METHOD constructor.

    super->constructor( ).

    DATA: ls_service  LIKE LINE OF mt_registered_services.

    set_base_url( 'https://api-sandbox.dhl.com/' ).
    set_base_uri( 'dpi/' ).

* ---------------------------------------------------------------------*
    CLEAR mt_registered_services.

    ls_service-class = 'ZCL_DHL_REST_API'.

* Access key GET
* ---------------------------------------------------------------------*
    ls_service-service  = 'auth/accesstoken'.
    ls_service-version  = '1'.
    ls_service-method   = if_rest_message=>gc_method_get.
    ls_service-status   = '2'.
    ls_service-handler  = 'GET_ACCESS_TOKEN'.
    APPEND ls_service TO mt_registered_services.
* ---------------------------------------------------------------------*

* Access key resfresh GET
* ---------------------------------------------------------------------*
    ls_service-service  = 'auth/accesstoken/&ref1'.
    ls_service-version  = '1'.
    ls_service-method   = if_rest_message=>gc_method_get.
    ls_service-status   = '2'.
    ls_service-handler  = 'REFRESH_ACCESS_TOKEN'.
    APPEND ls_service TO mt_registered_services.
* ---------------------------------------------------------------------*

* Create order POST
* ---------------------------------------------------------------------*
    ls_service-service  = 'shipping/&ref1/&ref2'.
    ls_service-method   = if_rest_message=>gc_method_post.
    ls_service-status   = '2'.
    ls_service-handler  = 'CREATE_ORDER'.
    APPEND ls_service TO mt_registered_services.
* ---------------------------------------------------------------------*

* Get order GET
* ---------------------------------------------------------------------*
    ls_service-service  = 'shipping/v1/orders/&ref1'.
    ls_service-method   = if_rest_message=>gc_method_get.
    ls_service-status   = '2'.
    ls_service-handler  = 'GET_ORDER'.
    APPEND ls_service TO mt_registered_services.
* ---------------------------------------------------------------------*

* Get item GET
* ---------------------------------------------------------------------*
    ls_service-service  = 'shipping/v1/items/&ref1'.
    ls_service-method   = if_rest_message=>gc_method_get.
    ls_service-status   = '2'.
    ls_service-handler  = 'GET_ITEM'.
    APPEND ls_service TO mt_registered_services.
* ---------------------------------------------------------------------*

  ENDMETHOD.


  METHOD create_order.

    " Zugriffstoken erhalten
    _get_access_token(
      EXPORTING
        iv_customer_secret = iv_customer_secret
        iv_customer_key    = iv_customer_key
      IMPORTING
        es_log  =   es_log
        es_access_token = DATA(ls_access_token) ).

    IF es_log-f_error EQ abap_true.
      RETURN.
    ENDIF.

    IF ls_access_token-access_token IS INITIAL.
      es_log-f_error = abap_true.
      es_log-text    = 'Das Zugriffstoken konnte nicht abgerufen werden'.
      es_log-http_code = 401.
      RETURN.
    ENDIF.

    _create_order(
      EXPORTING
        is_order        = is_order
        iv_access_token = ls_access_token-access_token
      IMPORTING
        es_log          = es_log
        "es_order        =
    ).


    " Hints to continue:
    " Check if token is not empty DONE
    " Parse data to json and send it for posting
    " Return data with newly created order if everything was successful

  ENDMETHOD.


  METHOD get_item.

    DATA: lv_access_token TYPE string.

    " Zugriffstoken erhalten
    _get_access_token(
      EXPORTING
        iv_customer_secret = iv_customer_secret
        iv_customer_key    = iv_customer_key
      IMPORTING
        es_log  =   es_log
        es_access_token = DATA(ls_access_token) ).

    IF es_log-f_error EQ abap_true.
      RETURN.
    ENDIF.

    IF ls_access_token-access_token IS INITIAL.
      es_log-f_error = abap_true.
      es_log-text    = 'Das Zugriffstoken konnte nicht abgerufen werden'.
      es_log-http_code = 401.
      RETURN.
    ENDIF.

    _get_item(
      EXPORTING
        iv_item_id = iv_item_id
        iv_access_token = lv_access_token
      IMPORTING
        es_log      =   es_log
        es_item    = es_item ).

  ENDMETHOD.


  METHOD get_lists.
*
    DATA ld_json_out  TYPE string.

* ---------------------------------------------------------------------*

    CLEAR:  ct_data,es_log.

* Daten lesen als JSON
    exec( EXPORTING id_service = 'GET_LISTS'
          IMPORTING ed_json    = ld_json_out
                    es_log     = es_log ).

*    mac_error_out.

* Datenebene fallen lassen
    zcl_json=>drop_data_level( EXPORTING iv_left_offset  = 26
                                         iv_right_offset = 1
                               CHANGING  cv_json = ld_json_out ).

* JSON nach ABAP
    json_2_abap( EXPORTING id_json = ld_json_out
                 IMPORTING es_log  = es_log
                 CHANGING  cd_data = ct_data ).

* ---------------------------------------------------------------------*
  ENDMETHOD.


  METHOD get_order.

    DATA: lv_access_token TYPE string.

    " Zugriffstoken erhalten
    _get_access_token(
      EXPORTING
        iv_customer_secret = iv_customer_secret
        iv_customer_key    = iv_customer_key
      IMPORTING
        es_log  =   es_log
        es_access_token = DATA(ls_access_token) ).

    IF es_log-f_error EQ abap_true.
      RETURN.
    ENDIF.

    IF ls_access_token-access_token IS INITIAL.
      es_log-f_error = abap_true.
      es_log-text    = 'Das Zugriffstoken konnte nicht abgerufen werden'.
      es_log-http_code = 401.
      RETURN.
    ENDIF.

    _get_order(
      EXPORTING
        iv_order_id = iv_order_id
        iv_access_token = lv_access_token
      IMPORTING
        es_log      =   es_log
        es_order    = es_order ).

  ENDMETHOD.


  METHOD get_recipients.
*
*    DATA: lt_data      TYPE TABLE OF any,"zgpv_rest_recipients,
*          ld_json_out  TYPE string,
*          ld_last_page TYPE c.
*
** ---------------------------------------------------------------------*
*
*    CLEAR: et_data,es_log.
*
** Pagination - result size is set to 1000 entrie
*    WHILE ld_last_page NE 'X'.
*      TRY.
** Daten lesen als JSON
*          exec( EXPORTING id_service = 'GET_RECIPIENTS'
*                          it_params  = ct_params
*                IMPORTING ed_json    = ld_json_out
*                          es_log     = es_log ).
*
*          IF ld_json_out(10) EQ '{"_links":'. "Keine Daten ausgewählt
*            EXIT.
*          ENDIF.
*
*          mac_error_out.
*
*          FIND FIRST OCCURRENCE OF '"next":{"href":' IN ld_json_out.
*          IF sy-subrc EQ 0.
*            set_pagination( EXPORTING id_json_out  = ld_json_out
*                            CHANGING  ct_params    = ct_params ).
*          ELSE.
*            ld_last_page = abap_true.
*          ENDIF.
*
** Datenebene fallen lassen
*          zcl_json=>drop_data_level( EXPORTING iv_left_offset  = 31
*                                               iv_right_offset = 2
*                                     CHANGING  cv_json = ld_json_out ).
*
** JSON nach ABAP
*          json_2_abap( EXPORTING id_json = ld_json_out
*                       IMPORTING es_log  = es_log
*                       CHANGING  cd_data = lt_data ).
*
*          MOVE-CORRESPONDING lt_data TO et_data KEEPING TARGET LINES.
*        CATCH cx_root.
*          RETURN.
*      ENDTRY.
*    ENDWHILE.
* ---------------------------------------------------------------------*
  ENDMETHOD.


  METHOD set_recipients_attr.
*
    DATA: ld_json_out TYPE string,
          id_json     TYPE string.

    SHIFT id_inxmail_id LEFT DELETING LEADING '0'.
    CONCATENATE '{"attributes":{"gpv_nr":"' id_gpv_nr '"}}' & '' INTO id_json.

* Daten lesen als JSON
    exec( EXPORTING id_service = 'SET_RECIPIENTS_ATTR'
                    id_ref1    = CONV #( id_inxmail_id )
                    id_json    = id_json
          IMPORTING ed_json    = ld_json_out
                    es_log     = es_log ).
*
  ENDMETHOD.


  METHOD _create_order.

    DATA: lt_params   TYPE zdhl_tt_rest_params,
          ld_json_out TYPE string,
          ld_body     TYPE string,
          lt_header   TYPE zdhl_tt_rest_header.

    " Körper in JSON parsen
    abap_2_json(
      EXPORTING
        it_data = is_order
      IMPORTING
        ed_json = ld_body
        es_log  = es_log ).

    IF es_log-f_error EQ abap_true.
      RETURN.
    ENDIF.

    " Kopfzeilen setzen
    APPEND VALUE #( name = 'Authorization' value = |Bearer { iv_access_token }| ) TO lt_header.

    set_params(
      EXPORTING
        id_handler = 'CREATE_ORDER'
      CHANGING
        ct_params  = lt_params ).

    exec( EXPORTING id_service = 'CREATE_ORDER'
                    it_params  = lt_params
                    it_header  = lt_header
                    id_json    = ld_body
                    id_ref1    = 'v1'
                    id_ref2    = 'orders'
          IMPORTING ed_json    = ld_json_out
                    es_log     = es_log ).

    IF es_log-f_error NE abap_true.
      json_2_abap(
        EXPORTING
          id_json = ld_json_out
        IMPORTING
          es_log  = es_log
        CHANGING
          cd_data = es_order ).
    ENDIF.

  ENDMETHOD.


  METHOD _get_access_token.

    DATA: ld_json_out     TYPE string,
          lt_header       TYPE zdhl_tt_rest_header,
          lv_access_token TYPE string.

    DATA(lo_data_provider) = NEW zcl_dhl_data_provider( ).

    " Überprüfen Sie, ob das Token nicht abgelaufen ist. Wenn ja, aktualisieren Sie es einfach
    DATA(ls_db_access_token) = lo_data_provider->get_db_access_token(
      EXPORTING
        iv_api          = 'DHL' ).

    IF ls_db_access_token-access_token IS NOT INITIAL.
      _refresh_access_token(
        EXPORTING
          iv_access_token = ls_db_access_token-access_token
        IMPORTING
          es_log          = es_log ).

      IF es_log-f_error NE abap_true.
        _update_access_token( iv_access_token = ls_db_access_token-access_token ).
        RETURN.
      ENDIF.
    ENDIF.

    " Wenn das Token nicht vorhanden ist oder die Aktualisierung nicht erfolgreich war

    " Kundenschlüssel verschlüsseln
    DATA(lv_encoded_key) = cl_http_utility=>encode_base64( unencoded = |{ iv_customer_key }:{ iv_customer_secret }| ).

    " Kopfzeilen setzen
    APPEND VALUE #( name = 'Authorization' value = |Basic { lv_encoded_key }| ) TO lt_header.

    exec( EXPORTING id_service = 'GET_ACCESS_TOKEN'
                    it_header = lt_header
          IMPORTING ed_json    = ld_json_out
                    es_log     = es_log ).

    IF es_log-f_error NE abap_true.
      json_2_abap(
        EXPORTING
          id_json = ld_json_out
        IMPORTING
          es_log  = es_log
        CHANGING
          cd_data = es_access_token ).
    ENDIF.

    IF es_log-f_error EQ abap_true.
      RETURN.
    ENDIF.

    _update_access_token( iv_access_token = es_access_token-access_token ).

  ENDMETHOD.


  METHOD _get_item.

    DATA: ld_json_out TYPE string,
          lt_header   TYPE zdhl_tt_rest_header.

    " Autorisierungs-Header-Wert mit Bearer-Token
    APPEND VALUE #( name = 'Authorization' value = |Bearer { iv_access_token }| ) TO lt_header.

    exec( EXPORTING id_service = 'GET_ITEM'
                    it_header  = lt_header
                    id_ref1    = iv_item_id
          IMPORTING ed_json    = ld_json_out
                    es_log     = es_log ).

    IF es_log-f_error NE abap_true.
      json_2_abap(
        EXPORTING
          id_json = ld_json_out
        IMPORTING
          es_log  = es_log
        CHANGING
          cd_data = es_item ).
    ENDIF.

  ENDMETHOD.


  METHOD _get_order.

    DATA: ld_json_out TYPE string,
          lt_header   TYPE zdhl_tt_rest_header.

    " Autorisierungs-Header-Wert mit Bearer-Token
    APPEND VALUE #( name = 'Authorization' value = |Bearer { iv_access_token }| ) TO lt_header.

    exec( EXPORTING id_service = 'GET_ORDER'
                    it_header  = lt_header
                    id_ref1    = iv_order_id
          IMPORTING ed_json    = ld_json_out
                    es_log     = es_log ).

    IF es_log-f_error NE abap_true.
      json_2_abap(
        EXPORTING
          id_json = ld_json_out
        IMPORTING
          es_log  = es_log
        CHANGING
          cd_data = es_order ).
    ENDIF.

  ENDMETHOD.


  METHOD _refresh_access_token.

    " Refresh current token
    exec( EXPORTING id_service = 'REFRESH_ACCESS_TOKEN'
                    id_ref1 = |revoke?token={ iv_access_token }|
          IMPORTING es_log     = es_log ).

  ENDMETHOD.


  METHOD _update_access_token.

    DATA: ls_access_token_db TYPE zdhl_acccs_token.
    DATA(lo_data_access) = NEW zcl_dhl_data_access( ).

    ls_access_token_db-id = 1.
    ls_access_token_db-api = 'DHL'.
    ls_access_token_db-access_token = iv_access_token.
    ls_access_token_db-expiring_date = sy-datum.
    ls_access_token_db-expiring_time = sy-uzeit + 17999.

    lo_data_access->modify_db_access_token( is_access_token = ls_access_token_db ).

  ENDMETHOD.
ENDCLASS.

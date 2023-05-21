class ZCL_DHL_PARCEL_D_SHIP_REST_API definition
  public
  inheriting from ZCL_DHL_REST_CLIENT
  final
  create public .

public section.

  constants MC_SERVICE_VERSION type STRING value '1' ##NO_TEXT.

  methods CREATE_ORDER
    importing
      !IS_ORDER type ZDHL_PARCEL_D_ORDER_REQ optional
      !ID_JSON type STRING optional
    exporting
      !ES_LOG type ZDHL_REST_LOG
      !ES_ORDER type ZDHL_PARCEL_D_ORDER_RES
    changing
      !IT_PARAMS type ZDHL_TT_REST_PARAMS optional .
  methods CONSTRUCTOR .
  methods SET_CREATE_ORDER_PARAMS
    importing
      !IV_VALIDATE type ABAP_BOOL default ''
      !IV_MUST_ENCODE type ABAP_BOOL default ''
      !IV_INCLUDE_DOCS type STRING default 'include'
      !IV_DOC_FORMAT type STRING default 'PDF'
      !IV_PRINT_FORMAT type STRING optional
      !IV_RETOURE_PRINT_FORMAT type STRING optional
      !IV_COMBINE type ABAP_BOOL default 'X' .
  PROTECTED SECTION.
  PRIVATE SECTION.

    DATA mt_create_order_params TYPE zdhl_tt_rest_params .
ENDCLASS.



CLASS ZCL_DHL_PARCEL_D_SHIP_REST_API IMPLEMENTATION.


  METHOD constructor.

    super->constructor( ).

    DATA: ls_service  LIKE LINE OF mt_registered_services.

    set_base_url( 'https://api-sandbox.dhl.com/' ).
    set_base_uri( 'parcel/de/shipping/' ).

* Set default parameters
    set_create_order_params( ).

* ---------------------------------------------------------------------*
    CLEAR mt_registered_services.

    ls_service-class = 'ZCL_DHL_PARCEL_D_SHIP_REST_API'.

* Create order POST
* ---------------------------------------------------------------------*
    ls_service-service  = 'orders'.
    ls_service-method   = if_rest_message=>gc_method_post.
    ls_service-status   = '2'.
    ls_service-version = '2'.
    ls_service-handler  = 'CREATE_ORDER'.
    APPEND ls_service TO mt_registered_services.
* ---------------------------------------------------------------------*

  ENDMETHOD.


  METHOD create_order.

    " Authorization header will be inserted automatically!

    DATA: lt_params   TYPE zdhl_tt_rest_params,
          ld_json_out TYPE string,
          ld_body     TYPE string,
          lt_header   TYPE zdhl_tt_rest_header.

    IF is_order IS INITIAL AND id_json IS INITIAL.
      es_log-f_error = abap_true.
      es_log-text    = 'Body for creating order can not be empty.'.
      es_log-http_code = 400.
      RETURN.
    ENDIF.

    " If json is provided, use it for request body.
    IF id_json IS SUPPLIED.
      ld_body = id_json.
    ELSE.
      " Körper in JSON parsen
      abap_2_json(
        EXPORTING
          it_data = is_order
        IMPORTING
          ed_json = ld_body
          es_log  = es_log ).

      IF es_log-f_error EQ abap_true.
        es_log-f_error = abap_true.
        es_log-text    = 'Body for creating shipment can not be empty.'.
        es_log-http_code = 400.
        RETURN.
      ENDIF.
    ENDIF.

    exec( EXPORTING id_service = 'CREATE_ORDER'
                    it_params  = mt_create_order_params
                    it_header  = lt_header
                    id_json    = ld_body
                    id_ref1    = 'orders'
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


  METHOD set_create_order_params.
    CLEAR: mt_create_order_params.
    " Set parameters for order creation. New values can be provided. If not, default values are set.
    APPEND VALUE #( name = 'validate' value = |{ COND string( WHEN iv_validate EQ abap_true THEN 'true' ELSE 'false' ) }| ) TO mt_create_order_params.
    APPEND VALUE #( name = 'mustEncode' value = |{ COND string( WHEN iv_must_encode EQ abap_true THEN 'true' ELSE 'false' ) }| ) TO mt_create_order_params.
    APPEND VALUE #( name = 'includeDocs' value = iv_include_docs ) TO mt_create_order_params.
    APPEND VALUE #( name = 'docFormat' value = iv_doc_format ) TO mt_create_order_params.
    IF iv_print_format IS SUPPLIED.
      APPEND VALUE #( name = 'printFormat' value = iv_print_format ) TO mt_create_order_params.
    ENDIF.
    IF iv_retoure_print_format IS SUPPLIED.
      APPEND VALUE #( name = 'retourePrintFormat' value = iv_retoure_print_format ) TO mt_create_order_params.
    ENDIF.
    APPEND VALUE #( name = 'combine' value = |{ COND string( WHEN iv_combine EQ abap_true THEN 'true' ELSE 'false' ) }| ) TO mt_create_order_params.
  ENDMETHOD.
ENDCLASS.

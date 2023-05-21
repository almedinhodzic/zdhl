class ZCL_DHL_EXPRESS_REST_API definition
  public
  inheriting from ZCL_DHL_REST_CLIENT
  final
  create public .

public section.

  methods CONSTRUCTOR .
  methods TRACK_SHIPMENT
    importing
      !IV_SHIPPMENT_ID type STRING
    exporting
      !ES_SHIPPMENT type ZDHL_REST_EXP_SHIPMENTS_RES
      !ES_LOG type ZDHL_REST_LOG .
  methods CREATE_SHIPMENT
    importing
      !IS_SHIPMENT type ZDHL_REST_EXP_SHIPMENT_REQ optional
      !ID_JSON type STRING optional
    exporting
      !ES_LOG type ZDHL_REST_LOG
      !ES_SHIPMENT type ZDHL_REST_EXP_SHIPMENT_RES .
protected section.
private section.
ENDCLASS.



CLASS ZCL_DHL_EXPRESS_REST_API IMPLEMENTATION.


  METHOD constructor.

    super->constructor( ).

    DATA: ls_service  LIKE LINE OF mt_registered_services.

    set_base_url( 'https://api-mock.dhl.com/mydhlapi/' ).
    set_base_uri( '' ).

* ---------------------------------------------------------------------*
    CLEAR mt_registered_services.

    ls_service-class = 'ZCL_DHL_EXPRESS_REST_API'.

* Track shippment GET
* ---------------------------------------------------------------------*
    ls_service-service  = 'shipments/&ref1/&ref2'.
    ls_service-method   = if_rest_message=>gc_method_get.
    ls_service-status   = '2'.
    ls_service-handler  = 'TRACK_SHIPPMENT'.
    APPEND ls_service TO mt_registered_services.
* ---------------------------------------------------------------------*

* Create order POST
* ---------------------------------------------------------------------*
    ls_service-service  = 'shipments'.
    ls_service-method   = if_rest_message=>gc_method_post.
    ls_service-status   = '2'.
    ls_service-handler  = 'CREATE_SHIPMENT'.
    APPEND ls_service TO mt_registered_services.
* ---------------------------------------------------------------------*

  ENDMETHOD.


  METHOD create_shipment.
    " Authorization header will be inserted automatically!

    DATA: ld_json_out TYPE string,
          ld_body     TYPE string,
          lt_header   TYPE zdhl_tt_rest_header.

    IF is_shipment IS INITIAL AND id_json IS INITIAL.
      es_log-f_error = abap_true.
      es_log-text    = 'Body for creating shipment can not be empty.'.
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
          it_data = is_shipment
        IMPORTING
          ed_json = ld_body
          es_log  = es_log ).

      IF es_log-f_error EQ abap_true.
        es_log-f_error = abap_true.
        es_log-text    = 'Bad request.'.
        es_log-http_code = 400.
        RETURN.
      ENDIF.
    ENDIF.

    exec( EXPORTING id_service = 'CREATE_SHIPMENT'
                    it_header  = lt_header
                    id_json    = ld_body
          IMPORTING ed_json    = ld_json_out
                    es_log     = es_log ).

    IF es_log-f_error NE abap_true.
      json_2_abap(
        EXPORTING
          id_json = ld_json_out
        IMPORTING
          es_log  = es_log
        CHANGING
          cd_data = es_shipment ).
    ENDIF.

  ENDMETHOD.


  METHOD TRACK_SHIPMENT.

    DATA: ld_json_out TYPE string,
          lt_header   TYPE zdhl_tt_rest_header.

    exec( EXPORTING id_service = 'TRACK_SHIPPMENT'
                    it_header  = lt_header
                    id_ref1    = iv_shippment_id
                    id_ref2    = 'tracking'
          IMPORTING ed_json    = ld_json_out
                    es_log     = es_log ).

    IF es_log-f_error NE abap_true.
      json_2_abap(
        EXPORTING
          id_json = ld_json_out
        IMPORTING
          es_log  = es_log
        CHANGING
          cd_data = es_shippment ).
    ENDIF.

  ENDMETHOD.
ENDCLASS.

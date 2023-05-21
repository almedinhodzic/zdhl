class ZCL_DHL_REST_CLIENT definition
  public
  abstract
  create public .

public section.

  data _MC_BASE_URI type STRING value '/rest/' ##NO_TEXT.
  data MT_REGISTERED_SERVICES type ZDHL_TT_REST_SERVICES .

  methods SET_PARAMS
    importing
      !ID_HANDLER type STRING
    changing
      !CT_PARAMS type ZDHL_TT_REST_PARAMS .
  methods SET_PAGINATION
    importing
      !ID_JSON_OUT type STRING
    changing
      !CT_PARAMS type ZDHL_TT_REST_PARAMS .
  methods LOG_HTTP_ERROR
    importing
      !IO_HTTP_CLIENT type ref to IF_HTTP_CLIENT
      !IO_RESPONSE type ref to IF_REST_ENTITY
      !IO_REQUEST type ref to IF_REST_ENTITY
    exporting
      !ES_LOG type ZDHL_REST_LOG .
  methods ABAP_2_JSON
    importing
      !IT_DATA type ANY
    exporting
      !ED_JSON type STRING
      !ES_LOG type ZDHL_REST_LOG .
  methods JSON_2_ABAP
    importing
      !ID_JSON type STRING
    exporting
      !ES_LOG type ZDHL_REST_LOG
    changing
      !CD_DATA type ANY .
  methods GET_DATA_CONNECTION
    importing
      !ID_CUSTOMER_ID type CHAR30 optional .
  methods HTTP .
  methods CONSTRUCTOR .
protected section.

  data MC_BASE_URL type STRING value 'https://api-sandbox.dhl.com/' ##NO_TEXT.

  methods EXEC
    importing
      !ID_SERVICE type ZDHL_REST_SERVICES-HANDLER
      !ID_REF1 type STRING optional
      !ID_REF2 type STRING optional
      !ID_JSON type STRING optional
      !IT_PARAMS type ZDHL_TT_REST_PARAMS optional
      !IT_HEADER type ZDHL_TT_REST_HEADER optional
    exporting
      !ED_JSON type STRING
      !ES_LOG type ZDHL_REST_LOG .
  methods SET_BASE_URI
    importing
      !IV_BASE_URL type STRING .
  methods SET_BASE_URL
    importing
      !IV_BASE_URL type STRING .
private section.

  data _MD_CUSTOMERID type STRING .
  data _MD_DESTINATION type RFCDEST .
  data _MC_HTTP_ACCEPT type STRING value 'text/plain, application/json, application/*+json, */*' ##NO_TEXT.

  methods _GET_BASE_URL
    returning
      value(RD_URL) type STRING .
  methods _HTTP
    importing
      !ID_PATH type STRING
      !ID_METHOD type STRING
      !ID_BODY type STRING optional
      !IT_PARAMS type ZDHL_TT_REST_PARAMS optional
      !IT_HEADER type ZDHL_TT_REST_HEADER optional
    exporting
      !ED_JSON type STRING
      !ED_HTTP_STATUS type STRING
      !ED_HTTP_ERRORTEXT type STRING
      !ES_LOG type ZDHL_REST_LOG
    exceptions
      UNAUTHORIZED .
  methods _HTTP_GET
    importing
      !ID_SERVICE type STRING
      !ID_VERSION type STRING optional
      !ID_BODY type STRING optional
      !ID_REF1 type STRING optional
      !ID_REF2 type STRING optional
      !IT_PARAMS type ZDHL_TT_REST_PARAMS optional
      !IT_HEADER type ZDHL_TT_REST_HEADER optional
    exporting
      !ED_JSON type STRING
      !ES_LOG type ZDHL_REST_LOG .
  methods _HTTP_POST
    importing
      !ID_SERVICE type STRING
      !ID_VERSION type STRING optional
      !ID_BODY type STRING optional
      !ID_REF1 type STRING optional
      !ID_REF2 type STRING optional
      !IT_HEADER type ZDHL_TT_REST_HEADER optional
    exporting
      !ES_LOG type ZDHL_REST_LOG
      value(ED_JSON) type STRING .
  methods _HTTP_PATCH
    importing
      !ID_SERVICE type STRING
      !ID_VERSION type STRING optional
      !ID_BODY type STRING optional
      !ID_REF1 type STRING optional
      !ID_REF2 type STRING optional
    exporting
      !ES_LOG type ZDHL_REST_LOG
      value(ED_JSON) type STRING .
  methods _HTTP_PUT
    importing
      !ID_SERVICE type STRING
      !ID_VERSION type STRING optional
      !ID_BODY type STRING optional
      !ID_REF1 type STRING optional
      !ID_REF2 type STRING optional
      !IT_HEADER type ZDHL_TT_REST_HEADER optional
    exporting
      !ES_LOG type ZDHL_REST_LOG
      value(ED_JSON) type STRING .
  methods _HTTP_DELETE
    importing
      !ID_SERVICE type STRING
      !ID_VERSION type STRING optional
      !ID_BODY type STRING optional
      !ID_REF1 type STRING optional
      !ID_REF2 type STRING optional
    exporting
      !ES_LOG type ZDHL_REST_LOG
      !ED_JSON type STRING .
ENDCLASS.



CLASS ZCL_DHL_REST_CLIENT IMPLEMENTATION.


  METHOD abap_2_json.
*
* Daten in JSON serialisieren, Anfangsfelder überspringen und ABAP-Feldnamen in CamelCase konvertieren
    CALL METHOD /ui2/cl_json=>serialize
      EXPORTING
        data        = it_data
        compress    = abap_true
        pretty_name = /ui2/cl_json=>pretty_mode-camel_case
      RECEIVING
        r_json      = ed_json.

    IF it_data IS NOT INITIAL AND ed_json IS INITIAL.
      es_log-f_error = abap_true.
      es_log-text    = 'ABAP-zu-JSON-Konvertierungsfehler'.
      RETURN.
    ENDIF.
*
  ENDMETHOD.


  METHOD constructor.

    get_data_connection( ).

  ENDMETHOD.


  METHOD exec.

    DATA ls_service TYPE zdhl_rest_services.


    READ TABLE mt_registered_services INTO ls_service WITH KEY handler = id_service.

    CHECK ( sy-subrc = 0 ).

    CASE ls_service-method.

      WHEN 'GET'.
        _http_get( EXPORTING id_service = ls_service-service
                             id_version = ls_service-version
                             id_ref1    = id_ref1
                             id_ref2    = id_ref2
                             it_params  = it_params
                   IMPORTING ed_json    = ed_json
                             es_log     = es_log ).

      WHEN 'PUT'.
        _http_put( EXPORTING id_service = ls_service-service
                             id_body    = id_json
                             id_version = ls_service-version
                             id_ref1    = id_ref1
                             id_ref2    = id_ref2
                             it_header  = it_header
                   IMPORTING ed_json    = ed_json
                             es_log     = es_log ).

      WHEN 'DELETE'.
        _http_delete( EXPORTING id_service = ls_service-service
                                id_body    = id_json
                                id_version = ls_service-version
                                id_ref1    = id_ref1
                                id_ref2    = id_ref2
                      IMPORTING ed_json    = ed_json
                                es_log     = es_log ).
      WHEN 'PATCH'.
        _http_patch( EXPORTING id_service = ls_service-service
                               id_body    = id_json
                               id_version = ls_service-version
                               id_ref1    = id_ref1
                               id_ref2    = id_ref2
                     IMPORTING ed_json    = ed_json
                               es_log     = es_log ).

      WHEN 'POST'.
        _http_post( EXPORTING id_service = ls_service-service
                              id_body    = id_json
                              id_version = ls_service-version
                              id_ref1    = id_ref1
                              id_ref2    = id_ref2
                              it_header  = it_header
                    IMPORTING ed_json    = ed_json
                              es_log    = es_log ).

    ENDCASE.

  ENDMETHOD.


  METHOD get_data_connection.
*
    CLEAR: _md_customerid,
           _md_destination.

*      SELECT SINGLE * FROM zgpv_inxmail_con INTO @DATA(ls_con)
*        WHERE system_id EQ @sy-sysid.
*
*    _md_customerid = ls_con-x_customer_id.
*    _md_destination = ls_con-x_destination.
*
  ENDMETHOD.


  METHOD http.
  ENDMETHOD.


  METHOD json_2_abap.
*
* Deserialisierung des JSON-Zeichenfolge json in interne Datentypen, indem Sie CamelCase zu ABAP wie eine Feldnamenzuordnung durchführen
    CALL METHOD /ui2/cl_json=>deserialize
      EXPORTING
        json = id_json
        pretty_name = /ui2/cl_json=>pretty_mode-camel_case
      CHANGING
        data = cd_data.

    IF cd_data IS INITIAL.
      es_log-f_error = abap_true.
      es_log-text    = 'JSON-zu-ABAP-Konvertierungsfehler'.
      RETURN.
    ENDIF.

*
  ENDMETHOD.


  METHOD log_http_error.
*
    DATA ld_json    TYPE string.
    DATA lt_rc      TYPE zdhl_tt_rest_mix_responses.
    DATA ls_rc      TYPE zdhl_rest_mix_responses.
    DATA ld_err     TYPE string.
    DATA lo_error   TYPE REF TO cx_root.

* ---------------------------------------------------------------------*

    ld_json = io_http_client->response->get_cdata( ).

* Unicode-String konvertieren
    zcl_json=>convert_from_unicode( CHANGING cv_string = ld_json ).

* Das Rückgabe-JSON-Format ist nicht immer gültig, da ' maskiert ist -> Trafo-Fehler
    REPLACE ALL OCCURRENCES OF '\''' IN ld_json WITH ''''.

* Versuchen zu kriegen weitere Informationen ab Rücksendung
    TRY.

*     Result aufbereiten
        json_2_abap( EXPORTING id_json = ld_json CHANGING cd_data = lt_rc ).

        READ TABLE lt_rc INTO ls_rc INDEX 1.

        CONCATENATE 'REST-Service Error. Title:' ls_rc-title
                    'Reference:' ls_rc-reference
                    'Details:' ls_rc-details
                    INTO ld_err SEPARATED BY space.


        es_log-f_error  = abap_true.
        es_log-response = ld_json.
        es_log-text     = ld_err.
        IF ( io_request IS NOT INITIAL ).
          es_log-command  = io_request->get_string_data( ).
        ENDIF.

      CATCH cx_root INTO lo_error.

*
        es_log-f_error  = abap_true.
        IF ( io_request IS BOUND ).
          es_log-command  = io_request->get_string_data( ).
        ENDIF.
        es_log-response = ld_json.
        es_log-text     = 'Transformation Error'.

    ENDTRY.
  ENDMETHOD.


  METHOD SET_BASE_URI.

    _mc_base_uri = iv_base_url.

  ENDMETHOD.


  method SET_BASE_URL.

    mc_base_url = iv_base_url.

  endmethod.


  METHOD set_pagination.
*
    DELETE ct_params WHERE name EQ 'afterId'.
    DELETE ct_params WHERE name EQ 'pageSize'.

    DATA(lv_url) = substring_to( val = substring_from( val = id_json_out sub = '"next":{"href"'  ) sub = '"},' ).
    APPEND VALUE #( name = 'afterId'  value = substring_before( val = substring_after( val = lv_url sub = 'afterId=' ) sub = '&' )  ) TO ct_params.
    APPEND VALUE #( name = 'pageSize' value = substring_before( val = substring_after( val = lv_url sub = 'pageSize=' ) sub = '&' ) ) TO ct_params.
*
  ENDMETHOD.


  METHOD set_params.
*
*    CASE id_handler.
*      WHEN 'GET_RECIPIENTS'.
*        APPEND VALUE #( name = 'attributes'                  value = 'vorname,name,gpv_nr,geschlecht' ) TO ct_params.
*        APPEND VALUE #( name = 'subscriptionDatesForLists'   value = zcl_gpv_inxmail_crud=>get_lists_map_for_rest( ) ) TO ct_params. "An-/Abmeldedatum der Empfänger für die Liste
*        APPEND VALUE #( name = 'unsubscriptionDatesForLists' value = zcl_gpv_inxmail_crud=>get_lists_map_for_rest( ) ) TO ct_params. "Listen IDs( z.B. NEWS@BDO = '3', ZUORDNUNG = '4' )
*      WHEN 'GET_BOUNCES'.
*        APPEND VALUE #( name = 'bounceCategory' value = 'HARD' ) TO ct_params.
*      WHEN OTHERS.
*    ENDCASE.
*
  ENDMETHOD.


  METHOD _get_base_url.
*
* Deutsche Post International (Post & Parcel Germany)
    rd_url = |https://api-sandbox.dhl.com/|.
*
  ENDMETHOD.


  METHOD _http.

    DATA lo_http_client   TYPE REF TO if_http_client.
    DATA lo_rest_client   TYPE REF TO zcl_dhl_rest_http_clnt.
    DATA lo_err           TYPE REF TO cx_root.
    DATA lo_request       TYPE REF TO if_rest_entity.
    DATA lo_response      TYPE REF TO if_rest_entity.
    DATA ld_token         TYPE string.
    DATA ls_rc            TYPE zdhl_rest_rc.
    DATA ld_rc            TYPE string.
    DATA ls_log           TYPE zdhl_rest_log.
    DATA ld_log           TYPE string.
    DATA ls_header        TYPE zdhl_rest_header.
    DATA ls_params        TYPE zdhl_rest_params.
* ---------------------------------------------------------------------*

    ASSERT ( id_path   IS NOT INITIAL AND
             id_method IS NOT INITIAL ).

    es_log-command = id_body.
    es_log-url     = mc_base_url.
    CONCATENATE es_log-url _md_customerid id_path INTO es_log-url.

    CLEAR: ed_http_status, ed_http_errortext.

* Wenn der Auth. nicht erzeugt wurde -> return
* Mandatory data
    IF ( _md_destination IS INITIAL OR
         _md_customerid  IS INITIAL ).

      es_log-f_error = abap_true.
      es_log-text    = 'Unvollständige Parameter (ZCL_DHL_REST_CLIENT=>_EXECUTEP)'.
      RETURN.

    ENDIF.

* Allowed HTTP method?
    CASE id_method.
      WHEN if_rest_message=>gc_method_get.
      WHEN if_rest_message=>gc_method_post.
      WHEN if_rest_message=>gc_method_put.
      WHEN if_rest_message=>gc_method_delete.
      WHEN if_rest_message=>gc_method_head.
      WHEN if_rest_message=>gc_method_options.
      WHEN if_rest_message=>gc_method_patch.
      WHEN if_rest_message=>gc_method_merge.
      WHEN OTHERS.
        es_log-f_error = abap_true.
        es_log-text    = 'Nicht unterstützte HTTP- Methode (ZCL_DHL_REST_CLIENT=>_EXECUTEP)'.
        RETURN.

    ENDCASE.

* Create by SM59 destination
    cl_http_client=>create_by_destination(
      EXPORTING
        destination              = _md_destination
      IMPORTING
        client                   = lo_http_client
      EXCEPTIONS
        argument_not_found       = 1
        destination_not_found    = 2
        destination_no_authority = 3
        plugin_not_active        = 4
        internal_error           = 5
        OTHERS                   = 6 ).

    IF ( sy-subrc <> 0 ).

      es_log-f_error = abap_true.
      es_log-text    = 'Fehler bei der Erzeugung der REST- Destination (ZCL_DHL_REST_CLIENT=>_EXECUTEP)'.
      RETURN.

    ENDIF.

* Create REST client instance
    CREATE OBJECT lo_rest_client
      EXPORTING
        io_http_client = lo_http_client.

* Proto - settings
    lo_http_client->request->set_version( if_http_request=>co_protocol_version_1_0 ).

* Set URI- path
    cl_http_utility=>set_request_uri(
      EXPORTING
        request = lo_http_client->request
        uri     = es_log-url ).

    lo_http_client->request->set_method( id_method ).

* Header fields from cust (as strings)
*  lo_http_client->authenticate( username = _md_user  password = _md_password ).
    lo_http_client->request->set_header_field( name  = 'Accept'  value = _mc_http_accept ).
    IF id_method EQ if_rest_message=>gc_method_patch.
      lo_http_client->request->set_header_field( name  = 'Content-Type'  value = 'application/merge-patch+json' ).
    ENDIF.

* WEITERE param-fields (optional)
    IF ( it_params IS SUPPLIED AND it_params IS NOT INITIAL ).
      LOOP AT it_params INTO ls_params.
        lo_http_client->request->set_form_field( name  = ls_params-name value = ls_params-value ).
      ENDLOOP.
    ENDIF.

* WEITERE header-fields (optional)
    IF ( it_header IS SUPPLIED AND it_header IS NOT INITIAL ).
      LOOP AT it_header INTO ls_header.
        lo_rest_client->if_rest_client~set_request_header( iv_name  = ls_header-name iv_value = ls_header-value ).
      ENDLOOP.
    ENDIF.

* Payload?
    IF ( id_method <> if_rest_message=>gc_method_get AND
         id_method <> if_rest_message=>gc_method_delete AND
         id_body IS NOT INITIAL ).

      lo_request = lo_rest_client->if_rest_client~create_request_entity( ).
      lo_request->set_string_data( id_body ).

    ENDIF.

* Start REST call
    TRY.

        CASE id_method.
          WHEN if_rest_message=>gc_method_post.
            lo_rest_client->if_rest_client~post( lo_request ).
          WHEN if_rest_message=>gc_method_put.
            lo_rest_client->if_rest_client~put( lo_request ).
          WHEN if_rest_message=>gc_method_get.
            lo_rest_client->if_rest_client~get( ).
          WHEN if_rest_message=>gc_method_delete.
            lo_rest_client->if_rest_client~delete( ).
          WHEN if_rest_message=>gc_method_patch.
            lo_rest_client->patch( lo_request ).
        ENDCASE.

      CATCH cx_rest_client_exception INTO lo_err.

        es_log-text    = lo_err->get_text( ).
        es_log-f_error = abap_true.
        CONCATENATE 'Fehler bei REST- Calls (ZCL_DHL_REST_CLIENT=>_HTTP)'
          es_log-text
          INTO es_log-text
          SEPARATED BY space.
        lo_http_client->close( ).
        RETURN.

    ENDTRY.

* HTTP response
    lo_response = lo_rest_client->if_rest_client~get_response_entity( ).

* Logging
    es_log-response = lo_response->get_string_data( ).

* Handle HTTP response
    ed_http_status   = lo_response->get_header_field( '~status_code' ).
    es_log-http_code = ed_http_status.

    CASE lo_response->get_header_field( '~status_code' ).
      WHEN cl_rest_status_code=>gc_success_ok.                "200 - OK
      WHEN cl_rest_status_code=>gc_success_created.           "201 - OK
      WHEN cl_rest_status_code=>gc_client_error_unauthorized. "401
        ed_http_errortext = es_log-response.
        lo_http_client->close( ).
        RAISE unauthorized.                                   "Bad credentials?
      WHEN cl_rest_status_code=>gc_server_error_internal OR   "500
           cl_rest_status_code=>gc_client_error_bad_request.  "400
        ed_http_errortext = es_log-response.

        log_http_error( EXPORTING io_http_client = lo_http_client
                                  io_request     = lo_request
                                  io_response    = lo_response
                        IMPORTING es_log         = es_log ).
        lo_http_client->close( ).
        RETURN.

      WHEN OTHERS.

        ld_log = lo_response->get_header_field( '~status_code' ).
        ed_http_errortext = es_log-response.

*     Result
        ed_json = lo_response->get_string_data( ).

        CONCATENATE 'Fehlerhafter REST- Call. HTTP- Return Code = '
          ld_log '. JSON = '
          ed_json
          INTO ld_log SEPARATED BY space.

        es_log-f_error  = abap_true.
        es_log-text     = ld_log.
        lo_http_client->close( ).
        RETURN.

    ENDCASE.

* Result
    ed_json = lo_response->get_string_data( ).

* Convert unicode string
    zcl_json=>convert_from_unicode( CHANGING cv_string = ed_json ).

    lo_http_client->close( ).

* ---------------------------------------------------------------------*
  ENDMETHOD.


  METHOD _http_delete.
*
    DATA ld_url TYPE string.

* ---------------------------------------------------------------------*

    CLEAR ed_json.

    ASSERT ( id_service IS NOT INITIAL ).

    IF ( id_version IS INITIAL ).

      CONCATENATE _mc_base_uri id_service INTO ld_url.

    ELSE.

      CONCATENATE _mc_base_uri 'v' id_version '/' id_service INTO ld_url.

    ENDIF.

    IF ( id_ref1 IS NOT INITIAL ).

      REPLACE FIRST OCCURRENCE OF '&ref1' IN ld_url WITH id_ref1.

    ENDIF.

    IF ( id_ref2 IS NOT INITIAL ).

      REPLACE FIRST OCCURRENCE OF '&ref2' IN ld_url WITH id_ref2.

    ENDIF.

    _http( EXPORTING  id_method    = if_rest_message=>gc_method_delete
                      id_path      = ld_url
                      id_body      = id_body
           IMPORTING  ed_json      = ed_json
                      es_log       = es_log
           EXCEPTIONS unauthorized = 1
                      OTHERS       = 9 ).

  ENDMETHOD.


  METHOD _http_get.
    DATA ld_url TYPE string.

* ---------------------------------------------------------------------*

    CLEAR:  ed_json.

    ASSERT ( id_service IS NOT INITIAL ).


    IF ( id_version IS INITIAL ).

      CONCATENATE _mc_base_uri id_service INTO ld_url.

    ELSE.

      CONCATENATE _mc_base_uri 'v' id_version '/' id_service INTO ld_url.

    ENDIF.

    IF ( id_ref1 IS NOT INITIAL ).

      REPLACE FIRST OCCURRENCE OF '&ref1' IN ld_url WITH id_ref1.

    ENDIF.

    IF ( id_ref2 IS NOT INITIAL ).

      REPLACE FIRST OCCURRENCE OF '&ref2' IN ld_url WITH id_ref2.

    ENDIF.

    _http( EXPORTING  id_method    = if_rest_message=>gc_method_get
                      id_path      = ld_url
                      id_body      = id_body
                      it_params    = it_params
           IMPORTING  ed_json      = ed_json
                      es_log       = es_log
           EXCEPTIONS unauthorized = 1 ).

    IF ( sy-subrc       = 1 AND
         es_log-f_error = abap_false ).
      es_log-f_error = abap_true.
      es_log-text    = 'Unautorisierter Zugriff.'.
    ENDIF.

* ---------------------------------------------------------------------*
  ENDMETHOD.


  METHOD _http_patch.
*
    DATA ld_url TYPE string.

* ---------------------------------------------------------------------*

    CLEAR: ed_json.

    ASSERT ( id_service IS NOT INITIAL ).

    IF ( id_version IS INITIAL ).

      CONCATENATE _mc_base_uri id_service INTO ld_url.

    ELSE.

      CONCATENATE _mc_base_uri 'v' id_version '/' id_service INTO ld_url.

    ENDIF.

    IF ( id_ref1 IS NOT INITIAL ).

      REPLACE FIRST OCCURRENCE OF '&ref1' IN ld_url WITH id_ref1.

    ENDIF.

    IF ( id_ref2 IS NOT INITIAL ).

      REPLACE FIRST OCCURRENCE OF '&ref2' IN ld_url WITH id_ref2.

    ENDIF.


    _http( EXPORTING  id_method    = if_rest_message=>gc_method_patch
                      id_path      = ld_url
                      id_body      = id_body
           IMPORTING  es_log       = es_log
                      ed_json      = ed_json
           EXCEPTIONS unauthorized = 1 ).

  ENDMETHOD.


  METHOD _http_post.

    DATA ld_url TYPE string.

* ---------------------------------------------------------------------*

    CLEAR: ed_json.

    ASSERT ( id_service IS NOT INITIAL ).

    IF ( id_version IS INITIAL ).

      CONCATENATE _mc_base_uri id_service INTO ld_url.

    ELSE.

      CONCATENATE _mc_base_uri 'v' id_version '/' id_service INTO ld_url.

    ENDIF.

    IF ( id_ref1 IS NOT INITIAL ).

      REPLACE FIRST OCCURRENCE OF '&ref1' IN ld_url WITH id_ref1.

    ENDIF.

    IF ( id_ref2 IS NOT INITIAL ).

      REPLACE FIRST OCCURRENCE OF '&ref2' IN ld_url WITH id_ref2.

    ENDIF.


    _http( EXPORTING  id_method    = if_rest_message=>gc_method_post
                      id_path      = ld_url
                      id_body      = id_body
                      it_header    = it_header
           IMPORTING  es_log       = es_log
                      ed_json      = ed_json
           EXCEPTIONS unauthorized = 1 ).

  ENDMETHOD.


  METHOD _http_put.
*
    DATA ld_url TYPE string.

* ---------------------------------------------------------------------*

    CLEAR: ed_json.

    ASSERT ( id_service IS NOT INITIAL ).

    IF ( id_version IS INITIAL ).

      CONCATENATE _mc_base_uri id_service INTO ld_url.

    ELSE.

      CONCATENATE _mc_base_uri 'v' id_version '/' id_service INTO ld_url.

    ENDIF.

    IF ( id_ref1 IS NOT INITIAL ).

      REPLACE FIRST OCCURRENCE OF '&ref1' IN ld_url WITH id_ref1.

    ENDIF.

    IF ( id_ref2 IS NOT INITIAL ).

      REPLACE FIRST OCCURRENCE OF '&ref2' IN ld_url WITH id_ref2.

    ENDIF.

    _http( EXPORTING  id_method    = if_rest_message=>gc_method_put
                      id_path      = ld_url
                      id_body      = id_body
                      it_header    = it_header
           IMPORTING  ed_json      = ed_json
                      es_log       = es_log
           EXCEPTIONS unauthorized = 1 ).

  ENDMETHOD.
ENDCLASS.

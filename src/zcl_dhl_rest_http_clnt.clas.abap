class ZCL_DHL_REST_HTTP_CLNT definition
  public
  final
  create public .

public section.

  interfaces IF_REST_RESOURCE .
  interfaces IF_REST_CLIENT .

  methods CONSTRUCTOR
    importing
      !IO_HTTP_CLIENT type ref to IF_HTTP_CLIENT .
  methods REFRESH_REQUEST .
  methods PATCH
    importing
      !IO_ENTITY type ref to IF_REST_ENTITY .
protected section.
private section.

  data MO_HTTP_CLIENT type ref to IF_HTTP_CLIENT .
  data MO_REQUEST_ENTITY type ref to IF_REST_ENTITY .

  methods SEND_RECEIVE
    importing
      !IV_HTTP_METHOD type STRING
      !IO_ENTITY type ref to IF_REST_ENTITY optional .
ENDCLASS.



CLASS ZCL_DHL_REST_HTTP_CLNT IMPLEMENTATION.


  METHOD constructor.
    super->constructor( ).

    mo_http_client = io_http_client.
    IF mo_http_client IS INITIAL.
      RAISE EXCEPTION TYPE cx_rest_client_exception
        EXPORTING
          textid = cx_rest_client_exception=>http_client_initial.
    ENDIF.
  ENDMETHOD.


  METHOD if_rest_client~close.
    IF mo_http_client IS BOUND.
      mo_http_client->close( ).
      CLEAR mo_http_client.
    ENDIF.
  ENDMETHOD.


  METHOD if_rest_client~create_request_entity.
    ro_entity = mo_request_entity = cl_rest_message_builder=>create_http_message_entity( mo_http_client->request ).
  ENDMETHOD.


  METHOD if_rest_client~get_response_entity.
    ro_response_entity = cl_rest_message_builder=>create_http_message_entity( mo_http_client->response ).
  ENDMETHOD.


  METHOD if_rest_client~get_response_header.
    rv_value = mo_http_client->response->get_header_field( iv_name ).
  ENDMETHOD.


  METHOD if_rest_client~get_response_headers.
    mo_http_client->response->get_header_fields( CHANGING fields = rt_header_fields ).
  ENDMETHOD.


  METHOD if_rest_client~get_status.
    mo_http_client->response->get_status( IMPORTING code = rv_status ).
  ENDMETHOD.


  METHOD if_rest_client~set_request_header.
    mo_http_client->request->set_header_field( name = iv_name value = iv_value ).
  ENDMETHOD.


  method IF_REST_CLIENT~SET_REQUEST_HEADERS.
  mo_http_client->request->set_header_fields( it_header_fields ).
  endmethod.


  METHOD if_rest_resource~delete.
    send_receive( if_rest_message=>gc_method_delete ).
  ENDMETHOD.


  method IF_REST_RESOURCE~GET.
    send_receive( if_rest_message=>gc_method_get ).
  endmethod.


  METHOD if_rest_resource~head.
   send_receive( if_rest_message=>gc_method_head ).
  ENDMETHOD.


  METHOD if_rest_resource~options.
    send_receive( if_rest_message=>gc_method_options ).
  ENDMETHOD.


  METHOD if_rest_resource~post.
    send_receive( iv_http_method = if_rest_message=>gc_method_post io_entity = io_entity ).
  ENDMETHOD.


  METHOD if_rest_resource~put.
    send_receive( iv_http_method = if_rest_message=>gc_method_put io_entity = io_entity ).
  ENDMETHOD.


  METHOD patch.
    send_receive( iv_http_method = if_rest_message=>gc_method_patch io_entity = io_entity ).
  ENDMETHOD.


  METHOD refresh_request.
    mo_http_client->refresh_request( ).
    CLEAR mo_request_entity.
  ENDMETHOD.


  method SEND_RECEIVE.
    "add first entity to the process table
    mo_http_client->request->set_method( iv_http_method ).

    IF io_entity IS NOT INITIAL.
      IF io_entity <> mo_request_entity.
        RAISE EXCEPTION TYPE cx_rest_client_exception
          EXPORTING
            textid = cx_rest_client_exception=>http_client_invalid_entity.
      ENDIF.
    ENDIF.

    CALL METHOD mo_http_client->send
      EXCEPTIONS
        http_communication_failure = 1
        http_invalid_state         = 2
        http_processing_failed     = 3
        http_invalid_timeout       = 4
        OTHERS                     = 5.

    IF sy-subrc = 0.
      CALL METHOD mo_http_client->receive
        EXCEPTIONS
          http_communication_failure = 1
          http_invalid_state         = 2
          http_processing_failed     = 3
          OTHERS                     = 5.
    ENDIF.

    " error handlign after send and receive
    IF sy-subrc <> 0.

      CASE sy-subrc.
        WHEN 1.
          RAISE EXCEPTION TYPE cx_rest_client_exception
            EXPORTING
              textid = cx_rest_client_exception=>http_client_comm_failure.
        WHEN 2.
          RAISE EXCEPTION TYPE cx_rest_client_exception
            EXPORTING
              textid = cx_rest_client_exception=>http_client_invalid_state.
        WHEN 3.
          RAISE EXCEPTION TYPE cx_rest_client_exception
            EXPORTING
              textid = cx_rest_client_exception=>http_client_processing_failed.
        WHEN 4.
          RAISE EXCEPTION TYPE cx_rest_client_exception
            EXPORTING
              textid = cx_rest_client_exception=>http_client_invalid_timeout.
        WHEN 5.
          RAISE EXCEPTION TYPE cx_rest_client_exception.
      ENDCASE.

    ENDIF.
  endmethod.
ENDCLASS.

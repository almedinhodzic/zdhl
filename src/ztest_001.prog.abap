*&---------------------------------------------------------------------*
*& Report ZTEST_001
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT ztest_001.

*DATA: lt_params       TYPE zdhl_tt_rest_params,
*      lt_log_messages TYPE bal_t_msg.
*
** REST API - weitere Parameter einstellen
*zcl_dhl_rest_api=>set_params( EXPORTING id_handler = 'GET_RECIPIENTS'
*                              CHANGING  ct_params  = lt_params ).
*
** REST API - Empfänger anrufen
*zcl_dhl_rest_api=>get_recipients( IMPORTING et_data   = DATA(lt_rest_recipient)
*                                            es_log    = DATA(ls_rest_log)
*                                  CHANGING  ct_params = lt_params ).
*
** Web Service - Protokoll prüfen
*IF ls_rest_log-f_error EQ abap_true." REST log
*  APPEND VALUE #( msgid = 'ZDHL' msgty = 'E' msgno = '001'
*                  msgv1 = CONV #( ls_rest_log-http_code )
*                  msgv2 = CONV #( ls_rest_log-response )
*                  msgv3 = CONV #( ls_rest_log-url )
*                  msgv4 = CONV #( ls_rest_log-text ) ) TO lt_log_messages.
*ENDIF.

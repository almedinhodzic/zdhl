class ZCL_JSON definition
  public
  final
  create public .

public section.

  class-methods CONVERT_TO_UTF8
    changing
      !CV_STRING type STRING .
  class-methods CONVERT_FROM_UNICODE
    changing
      !CV_STRING type STRING .
  class-methods DROP_DATA_LEVEL
    importing
      !IV_LEFT_OFFSET type INT4
      !IV_RIGHT_OFFSET type INT4
    changing
      !CV_JSON type STRING .
protected section.
private section.
ENDCLASS.



CLASS ZCL_JSON IMPLEMENTATION.


  method CONVERT_FROM_UNICODE.
*
  REPLACE ALL OCCURRENCES OF '\u00c4' in cv_string WITH 'Ä'.
  REPLACE ALL OCCURRENCES OF '\u00e4' in cv_string WITH 'ä'.
  REPLACE ALL OCCURRENCES OF '\u00d6' in cv_string WITH 'Ö'.
  REPLACE ALL OCCURRENCES OF '\u00f6' in cv_string WITH 'ö'.
  REPLACE ALL OCCURRENCES OF '\u00fc' in cv_string WITH 'ü'.
  REPLACE ALL OCCURRENCES OF '\u00dc' in cv_string WITH 'Ü'.
  REPLACE ALL OCCURRENCES OF '\u00df' in cv_string WITH 'ß'.
  REPLACE ALL OCCURRENCES OF '\u0022' in cv_string WITH ''''.
  REPLACE ALL OCCURRENCES OF '\u0027' in cv_string WITH ''''.

* ---------------------------------------------------------------------*
  endmethod.


  method CONVERT_TO_UTF8.

  DATA lo_con_i TYPE REF TO cl_abap_conv_in_ce.
  DATA lo_con_o TYPE REF TO cl_abap_conv_out_ce.
  DATA ld_xstr  TYPE xstring.
  DATA ld_len   TYPE i.

* ---------------------------------------------------------------------*

* Create booth convert- objects
  lo_con_i = cl_abap_conv_in_ce=>create(
        encoding = 'UTF-8'
        endian   = 'B' ).
  lo_con_o = cl_abap_conv_out_ce=>create( ).
* Convert input to xstring
  ld_len = strlen( cv_string ).
  lo_con_o->write( EXPORTING
                     n    = ld_len
                     data = cv_string ).
  ld_xstr = lo_con_o->get_buffer( ).

  lo_con_i->convert(
    EXPORTING input = ld_xstr
    IMPORTING data  = cv_string ).

* ---------------------------------------------------------------------*
  endmethod.


  method DROP_DATA_LEVEL.
*
  DATA lv_len      TYPE int4.
* ---------------------------------------------------------------------*

  SHIFT cv_json LEFT BY iv_left_offset PLACES.
  CONDENSE cv_json.
  lv_len = strlen( cv_json ).
  lv_len = lv_len - iv_right_offset.
  cv_json = cv_json(lv_len).

* ---------------------------------------------------------------------*
  endmethod.
ENDCLASS.

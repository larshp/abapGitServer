REPORT zags_start.

START-OF-SELECTION.
  PERFORM run.

FORM run.

  DATA: lv_url    TYPE string,
        lv_urlc   TYPE c LENGTH 2048,
        lo_viewer TYPE REF TO cl_gui_html_viewer,
        lo_empty  TYPE REF TO cl_gui_container.


* misuse the BSP runtime to construct SICF url
  cl_bsp_runtime=>construct_bsp_url(
    EXPORTING
      in_application = 'foobar'
    IMPORTING
      out_abs_url    = lv_url ).

  REPLACE FIRST OCCURRENCE OF 'sap/bc/bsp/sap/foobar'
    IN lv_url
    WITH 'sap/zabapgitserver'.

  CREATE OBJECT lo_viewer
    EXPORTING
      parent = lo_empty.

  lv_urlc = lv_url.
  lo_viewer->enable_sapsso( abap_true ).
  lo_viewer->detach_url_in_browser( lv_urlc ).
  cl_gui_cfw=>flush( ).

ENDFORM.

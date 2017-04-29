REPORT zags_start.

START-OF-SELECTION.
  PERFORM run.

FORM run.

  DATA: lv_url TYPE string.

* misuse the BSP runtime to construct SICF url
  cl_bsp_runtime=>construct_bsp_url(
    EXPORTING
      in_application = 'foobar'
    IMPORTING
      out_abs_url    = lv_url ).

  REPLACE FIRST OCCURRENCE OF 'sap/bc/bsp/sap/foobar'
    IN lv_url
    WITH 'sap/zabapgitserver'.

  cl_gui_frontend_services=>execute(
    EXPORTING
      document               = lv_url
    EXCEPTIONS
      cntl_error             = 1
      error_no_gui           = 2
      bad_parameter          = 3
      file_not_found         = 4
      path_not_found         = 5
      file_extension_unknown = 6
      error_execute_failed   = 7
      synchronous_failed     = 8
      not_supported_by_gui   = 9
      OTHERS                 = 10 ).
  IF sy-subrc <> 0.
    WRITE: / 'Error when calling frontend services:', sy-subrc.
  ENDIF.

ENDFORM.

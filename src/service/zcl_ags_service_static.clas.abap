class ZCL_AGS_SERVICE_STATIC definition
  public
  create public .

public section.

  interfaces ZIF_AGS_SERVICE .

  methods CONSTRUCTOR
    importing
      !II_SERVER type ref to IF_HTTP_SERVER .
  PROTECTED SECTION.
private section.

  data MI_SERVER type ref to IF_HTTP_SERVER .

  methods READ_MIME
    importing
      !IV_URL type STRING
    returning
      value(RV_DATA) type XSTRING .
ENDCLASS.



CLASS ZCL_AGS_SERVICE_STATIC IMPLEMENTATION.


  METHOD constructor.

    mi_server = ii_server.

  ENDMETHOD.


  METHOD read_mime.

    DATA: li_api TYPE REF TO if_mr_api,
          lv_url TYPE string.


    CONCATENATE '/SAP/PUBLIC/zgit/' iv_url INTO lv_url.

    li_api = cl_mime_repository_api=>if_mr_api~get_api( ).

    li_api->get(
      EXPORTING
        i_url = lv_url
      IMPORTING
        e_content = rv_data ).

  ENDMETHOD.


  METHOD zif_ags_service~run.

    DATA: lv_name TYPE string,
          lv_path TYPE string.


    lv_path = mi_server->request->get_header_field( '~path' ).

    FIND REGEX '/sap/zgit/static/(.*)'
      IN lv_path
      SUBMATCHES lv_name ##NO_TEXT.

    IF lv_name IS INITIAL.
      lv_name = 'index.html'.
    ENDIF.

    mi_server->response->set_data( read_mime( lv_name ) ) ##NO_TEXT.

  ENDMETHOD.
ENDCLASS.
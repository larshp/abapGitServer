CLASS zcl_ags_service_static DEFINITION
  PUBLIC
  CREATE PUBLIC.

  PUBLIC SECTION.

    INTERFACES zif_ags_service.

    METHODS constructor
      IMPORTING
        !ii_server TYPE REF TO if_http_server.
  PROTECTED SECTION.
  PRIVATE SECTION.

    DATA mi_server TYPE REF TO if_http_server.

    METHODS read_mime
      IMPORTING
        !iv_url        TYPE string
      RETURNING
        VALUE(rv_data) TYPE xstring.
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
      lv_name = 'index.html' ##NO_TEXT.
    ENDIF.

    mi_server->response->set_data( read_mime( lv_name ) ) ##NO_TEXT.

  ENDMETHOD.
ENDCLASS.
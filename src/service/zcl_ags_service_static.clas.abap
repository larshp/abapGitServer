CLASS zcl_ags_service_static DEFINITION
  PUBLIC
  CREATE PUBLIC.

  PUBLIC SECTION.

    METHODS run
      IMPORTING
        !ii_server TYPE REF TO if_http_server.
  PROTECTED SECTION.
  PRIVATE SECTION.

    METHODS read_mime
      IMPORTING
        !iv_url        TYPE string
      RETURNING
        VALUE(rv_data) TYPE xstring.
ENDCLASS.



CLASS ZCL_AGS_SERVICE_STATIC IMPLEMENTATION.


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


  METHOD run.

    ii_server->response->set_data( read_mime( 'index.html' ) ) ##NO_TEXT.

  ENDMETHOD.
ENDCLASS.
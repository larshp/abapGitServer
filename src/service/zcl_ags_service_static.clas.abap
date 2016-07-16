CLASS zcl_ags_service_static DEFINITION
  PUBLIC
  CREATE PUBLIC.

  PUBLIC SECTION.

    INTERFACES zif_ags_service.
  PROTECTED SECTION.
  PRIVATE SECTION.

    DATA mi_server TYPE REF TO if_http_server.

    METHODS read_mime
      IMPORTING
        !iv_url TYPE string.
ENDCLASS.



CLASS ZCL_AGS_SERVICE_STATIC IMPLEMENTATION.


  METHOD read_mime.

    DATA: li_api       TYPE REF TO if_mr_api,
          lv_data      TYPE xstring,
          lv_changed   TYPE smimphio-chng_time,
          lv_timestamp TYPE char14,
          lv_modified  TYPE string,
          lv_mime      TYPE string,
          lv_url       TYPE string.


    CONCATENATE '/SAP/PUBLIC/zgit/' iv_url INTO lv_url.

    li_api = cl_mime_repository_api=>if_mr_api~get_api( ).

    li_api->get(
      EXPORTING
        i_url                  = lv_url
      IMPORTING
        e_content              = lv_data
        e_mime_type            = lv_mime
        e_content_last_changed = lv_changed
      EXCEPTIONS
        not_found              = 1 ).
    IF sy-subrc = 1.
      mi_server->response->set_cdata( '404' ).
      mi_server->response->set_status( code = 404 reason = '404' ).
      RETURN.
    ENDIF.

    lv_timestamp = lv_changed.
    lv_modified = cl_bsp_utility=>date_to_string_http( lv_timestamp ).
    DATA(lv_value) = mi_server->request->get_header_field(
      name  = 'If-Modified-Since' ) ##NO_TEXT.
    IF lv_modified = lv_value.
      mi_server->response->set_status( code = 304 reason = '' ).
      RETURN.
    ENDIF.

    mi_server->response->set_header_field(
      name  = 'Cache-Control'
      value = 'max-age=86400' ) ##NO_TEXT.

    mi_server->response->set_header_field(
      name  = 'Last-Modified'
      value = lv_modified ) ##NO_TEXT.

    mi_server->response->set_compression( ).
    mi_server->response->set_content_type( lv_mime ).
    mi_server->response->set_data( lv_data ).

  ENDMETHOD.


  METHOD zif_ags_service~run.

    DATA: lv_name TYPE string,
          lv_path TYPE string.


    mi_server = ii_server.

    lv_path = mi_server->request->get_header_field( '~path' ).

    FIND REGEX '/sap/zgit/static/(.*)'
      IN lv_path
      SUBMATCHES lv_name ##NO_TEXT.

    IF lv_name IS INITIAL.
      lv_name = 'index.html' ##NO_TEXT.
    ENDIF.

    read_mime( lv_name ).

  ENDMETHOD.
ENDCLASS.
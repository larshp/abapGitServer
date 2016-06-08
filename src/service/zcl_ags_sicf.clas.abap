CLASS zcl_ags_sicf DEFINITION
  PUBLIC
  CREATE PUBLIC.

  PUBLIC SECTION.

    INTERFACES if_http_extension.
  PROTECTED SECTION.
  PRIVATE SECTION.

    METHODS dump_headers
      IMPORTING
        !ii_server       TYPE REF TO if_http_server
      RETURNING
        VALUE(rv_string) TYPE string.
    METHODS dump_test_link
      RETURNING
        VALUE(rv_string) TYPE string.
    METHODS read_mime
      IMPORTING
        !iv_url        TYPE string
      RETURNING
        VALUE(rv_data) TYPE xstring.
ENDCLASS.



CLASS ZCL_AGS_SICF IMPLEMENTATION.


  METHOD dump_headers.

* todo

  ENDMETHOD.


  METHOD dump_test_link.

    DATA: lv_host TYPE string,
          lv_port TYPE string.


    CALL FUNCTION 'TH_GET_VIRT_HOST_DATA'
      EXPORTING
        protocol = 1
        virt_idx = 0
      IMPORTING
        hostname = lv_host
        port     = lv_port.

    CONCATENATE 'http://' lv_host ':' lv_port '/zgit/foobar.git'
      INTO rv_string ##NO_TEXT.

  ENDMETHOD.


  METHOD if_http_extension~handle_request.

    DATA: lv_reason TYPE string,
          lv_path   TYPE string.


    lv_path = server->request->get_header_field( '~path' ).

    IF lv_path CP 'sap/zgit/static/*'.
      server->response->set_data( read_mime( 'index.html' ) ) ##NO_TEXT.
    ELSE.
*    server->response->set_data( read_mime( 'index.html' ) ).
      server->response->set_cdata( dump_test_link( ) ).
    ENDIF.

    server->response->set_status( code = 200
                                  reason = lv_reason ).

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
ENDCLASS.
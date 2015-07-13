class ZCL_ABAPGITSERVER definition
  public
  final
  create public .

public section.

  interfaces IF_HTTP_EXTENSION .
protected section.

  methods READ_MIME
    importing
      !IV_URL type STRING
    returning
      value(RV_DATA) type XSTRING .
private section.
ENDCLASS.



CLASS ZCL_ABAPGITSERVER IMPLEMENTATION.


METHOD if_http_extension~handle_request.

  DATA: lv_reason TYPE string,
        lv_path   TYPE string.


  lv_path = server->request->get_header_field( '~path' ).

  IF lv_path CP 'sap/zgit/static/*'.
    server->response->set_data( read_mime( 'index.html' ) ).
  ELSE.
    server->response->set_data( read_mime( 'index.html' ) ).
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
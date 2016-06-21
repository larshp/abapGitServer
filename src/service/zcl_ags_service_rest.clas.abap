class ZCL_AGS_SERVICE_REST definition
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

  methods TO_JSON
    importing
      !IG_DATA type ANY
    returning
      value(RV_JSON) type XSTRING .
ENDCLASS.



CLASS ZCL_AGS_SERVICE_REST IMPLEMENTATION.


  METHOD CONSTRUCTOR.

    mi_server = ii_server.

  ENDMETHOD.


  METHOD to_json.

    DATA: lo_writer TYPE REF TO cl_sxml_string_writer.

    lo_writer = cl_sxml_string_writer=>create( if_sxml=>co_xt_json ).
    CALL TRANSFORMATION id
      SOURCE data = ig_data
      RESULT XML lo_writer.
    rv_json = lo_writer->get_output( ).

  ENDMETHOD.


  METHOD zif_ags_service~run.

    DATA: lv_path TYPE string.


    lv_path = mi_server->request->get_header_field( '~path' ).

    IF lv_path CP '*/repositories/'.
      DATA(lt_list) = zcl_ags_repo=>list( ).
      mi_server->response->set_data( to_json( lt_list ) ).
    ELSE.
      mi_server->response->set_status( code   = 500
                                       reason = 'Error' ) ##NO_TEXT.
      mi_server->response->set_cdata( 'rest error' ).
    ENDIF.

  ENDMETHOD.
ENDCLASS.
class ZCL_AGS_SICF definition
  public
  create public .

public section.

  interfaces IF_HTTP_EXTENSION .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_AGS_SICF IMPLEMENTATION.


  METHOD if_http_extension~handle_request.

    DATA: lv_reason TYPE string,
          lv_path   TYPE string.


    lv_path = server->request->get_header_field( '~path' ).

    TRY.
        IF lv_path CP '/sap/zgit/static/*'.
          DATA(lo_static) = NEW zcl_ags_service_static( ).
          lo_static->run( server ).
        ELSEIF lv_path CP '/sap/zgit/git/*'.
          DATA(lo_git) = NEW zcl_ags_service_git( ).
          lo_git->run( server ).
        ELSE.
          RAISE EXCEPTION TYPE zcx_ags_error
            EXPORTING
              textid = zcx_ags_error=>m006
              string = lv_path.
        ENDIF.

        server->response->set_status( code   = 200
                                      reason = lv_reason ).
      CATCH zcx_ags_error INTO DATA(lx_error).
        server->response->set_status( code   = 500
                                      reason = 'Error' ) ##NO_TEXT.
        server->response->set_cdata( lx_error->if_message~get_text( ) ).
    ENDTRY.

  ENDMETHOD.
ENDCLASS.
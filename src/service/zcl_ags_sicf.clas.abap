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

    DATA: lv_reason  TYPE string,
          lv_path    TYPE string,
          li_service TYPE REF TO zif_ags_service.


    lv_path = server->request->get_header_field( '~path' ).

    TRY.
        IF lv_path CP '/sap/zgit/git/*'.
          li_service = NEW zcl_ags_service_git( server ).
        ELSEIF lv_path CP '/sap/zgit/rest/*'.
          li_service = NEW zcl_ags_service_rest( server ).
        ELSE.
          li_service = NEW zcl_ags_service_static( server ).
        ENDIF.

        li_service->run( ).

        server->response->set_status( code   = 200
                                      reason = lv_reason ).

        COMMIT WORK.
      CATCH zcx_ags_error INTO DATA(lx_error).
        ROLLBACK WORK.

        server->response->set_status( code   = 500
                                      reason = 'Error' ) ##NO_TEXT.
        server->response->set_cdata( lx_error->if_message~get_text( ) ).
    ENDTRY.

  ENDMETHOD.
ENDCLASS.
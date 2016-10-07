CLASS zcl_ags_sicf DEFINITION
  PUBLIC
  CREATE PUBLIC.

  PUBLIC SECTION.

    INTERFACES if_http_extension.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_AGS_SICF IMPLEMENTATION.


  METHOD if_http_extension~handle_request.

    DATA: lv_reason  TYPE string,
          lv_path    TYPE string,
          lx_error   TYPE REF TO zcx_ags_error,
          li_service TYPE REF TO zif_ags_service.


    lv_path = server->request->get_header_field( '~path' ).

    TRY.
        IF lv_path CP '/sap/zabapgitserver/git/*'.
          CREATE OBJECT li_service TYPE zcl_ags_service_git.
        ELSEIF lv_path CP '/sap/zabapgitserver/rest/*'.
          CREATE OBJECT li_service TYPE zcl_ags_service_rest.
        ELSE.
          CREATE OBJECT li_service TYPE zcl_ags_service_static.
        ENDIF.

        li_service->run( server ).

        COMMIT WORK.
      CATCH zcx_ags_error INTO lx_error.
        ROLLBACK WORK.
        server->response->set_cdata( '500, error' ).
        server->response->set_status( code   = 500
                                      reason = 'Error' ) ##no_text.
        server->response->set_cdata( lx_error->if_message~get_text( ) ).
    ENDTRY.

  ENDMETHOD.
ENDCLASS.
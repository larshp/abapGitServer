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
          lo_git     TYPE REF TO zcl_ags_service_git,
          lo_rest    TYPE REF TO zcl_ags_service_rest,
          lo_static  TYPE REF TO zcl_ags_service_static,
          li_service TYPE REF TO zif_ags_service.


    lv_path = server->request->get_header_field( '~path' ).

    TRY.
        IF lv_path CP '/sap/zgit/git/*'.
          CREATE OBJECT lo_git
            EXPORTING
              ii_server = server.
          li_service ?= lo_git.
        ELSEIF lv_path CP '/sap/zgit/rest/*'.
          CREATE OBJECT lo_rest
            EXPORTING
              ii_server = server.
          li_service ?= lo_rest.
        ELSE.
          CREATE OBJECT lo_static
            EXPORTING
              ii_server = server.
          li_service ?= lo_static.
        ENDIF.

        li_service->run( ).

        server->response->set_status( code   = 200
                                      reason = lv_reason ).

        COMMIT WORK.
      CATCH zcx_ags_error INTO DATA(lx_error).
        ROLLBACK WORK.
        DATA(lv_text) = lx_error->if_message~get_text( ).
        server->response->set_status( code   = 500
                                      reason = 'Error' ) ##no_text.
        server->response->set_cdata( lv_text ).
    ENDTRY.

  ENDMETHOD.
ENDCLASS.
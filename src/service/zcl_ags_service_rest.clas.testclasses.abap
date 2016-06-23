CLASS ltcl_list_files DEFINITION DEFERRED.
CLASS zcl_ags_service_rest DEFINITION LOCAL FRIENDS ltcl_list_files.

CLASS ltcl_list_files DEFINITION FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS
  FINAL.

  PRIVATE SECTION.
    DATA: mo_rest TYPE REF TO zcl_ags_service_rest.

    METHODS:
      setup,
      teardown,
      list_files FOR TESTING
        RAISING zcx_ags_error.

ENDCLASS.       "ltcl_List_Files

CLASS ltcl_list_files IMPLEMENTATION.

  METHOD setup.
    DATA: li_server TYPE REF TO if_http_server.

    CREATE OBJECT mo_rest
      EXPORTING
        ii_server = li_server.
  ENDMETHOD.

  METHOD teardown.
    ROLLBACK WORK.
  ENDMETHOD.

  METHOD list_files.

    DATA(lo_repo) = zcl_ags_repo=>create(
      iv_name        = 'unit_test'
      iv_description = 'foobar' ).
    DATA(lo_branch) = lo_repo->get_branch( lo_repo->get_data( )-head ).

    DATA(lt_files) = mo_rest->list_files( lo_branch->get_data( )-sha1 ).

    cl_abap_unit_assert=>assert_not_initial( lt_files ).

  ENDMETHOD.

ENDCLASS.
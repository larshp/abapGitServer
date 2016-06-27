CLASS ltcl_rest DEFINITION DEFERRED.
CLASS zcl_ags_service_rest DEFINITION LOCAL FRIENDS ltcl_rest.

CLASS ltcl_rest DEFINITION FOR TESTING
    DURATION SHORT
    RISK LEVEL HARMLESS
    FINAL.

  PUBLIC SECTION.
    INTERFACES: if_http_server PARTIALLY IMPLEMENTED.

  PRIVATE SECTION.
    CONSTANTS:
      c_name        TYPE zags_repos-name VALUE 'unit_test' ##NO_TEXT,
      c_description TYPE zags_repos-description VALUE 'description, foobar' ##NO_TEXT.

    DATA: mo_rest TYPE REF TO zcl_ags_service_rest,
          mo_repo TYPE REF TO zcl_ags_repo.

    METHODS:
      setup
        RAISING zcx_ags_error,
      teardown,
      list_files FOR TESTING
        RAISING zcx_ags_error,
      list_branches FOR TESTING
        RAISING zcx_ags_error.

ENDCLASS.       "ltcl_List_Files

CLASS ltcl_rest IMPLEMENTATION.

  METHOD setup.

    CREATE OBJECT mo_rest
      EXPORTING
        ii_server = me.

    mo_repo = zcl_ags_repo=>create(
      iv_name        = c_name
      iv_description = c_description ).

  ENDMETHOD.

  METHOD teardown.
    ROLLBACK WORK.                                     "#EC CI_ROLLBACK
  ENDMETHOD.

  METHOD list_files.

    DATA(lo_branch) = mo_repo->get_branch( mo_repo->get_data( )-head ).

    DATA(lt_files) = mo_rest->list_files_commit( lo_branch->get_data( )-sha1 ).

    cl_abap_unit_assert=>assert_not_initial( lt_files ).

  ENDMETHOD.

  METHOD list_branches.

    DATA(lt_branches) = mo_rest->list_branches( c_name ).

    cl_abap_unit_assert=>assert_not_initial( lt_branches ).

  ENDMETHOD.

ENDCLASS.
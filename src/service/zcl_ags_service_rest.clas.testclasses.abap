CLASS ltcl_rest DEFINITION DEFERRED.
CLASS zcl_ags_service_rest DEFINITION LOCAL FRIENDS ltcl_rest.

CLASS ltcl_rest DEFINITION FOR TESTING DURATION SHORT RISK LEVEL HARMLESS FINAL.

  PRIVATE SECTION.
    CONSTANTS:
      c_name        TYPE zags_repos-name VALUE 'UNIT_TEST',
      c_description TYPE zags_repos-description VALUE 'DESCRIPTION, FOOBAR'.

    DATA: mo_rest TYPE REF TO zcl_ags_service_rest,
          mo_repo TYPE REF TO zcl_ags_repo.

    METHODS:
      setup RAISING zcx_ags_error,
      list_files FOR TESTING RAISING zcx_ags_error,
      list_branches FOR TESTING RAISING zcx_ags_error.

ENDCLASS.       "ltcl_List_Files

CLASS ltcl_rest IMPLEMENTATION.

  METHOD setup.

    zcl_ags_db=>set_fake( ).

    CREATE OBJECT mo_rest.

    mo_repo = zcl_ags_repo=>create(
      iv_name        = c_name
      iv_description = c_description ).

  ENDMETHOD.

  METHOD list_files.

    DATA: lt_files TYPE zcl_ags_cache=>ty_files_tt.


    lt_files = mo_rest->list_files(
      iv_repo   = c_name
      iv_branch = mo_repo->get_data( )-head
      iv_path   = '' ).

    cl_abap_unit_assert=>assert_not_initial( lt_files ).

  ENDMETHOD.

  METHOD list_branches.

    DATA: lt_branches TYPE zcl_ags_service_rest=>ty_branches_tt.


    lt_branches = mo_rest->list_branches( c_name ).

    cl_abap_unit_assert=>assert_not_initial( lt_branches ).

  ENDMETHOD.

ENDCLASS.

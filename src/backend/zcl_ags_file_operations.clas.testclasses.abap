CLASS ltcl_test DEFINITION FOR TESTING DURATION SHORT RISK LEVEL HARMLESS FINAL.

  PRIVATE SECTION.
    DATA: mo_branch TYPE REF TO zcl_ags_branch.

    METHODS:
      setup RAISING zcx_ags_error,
      add FOR TESTING RAISING zcx_ags_error.

ENDCLASS.       "ltcl_Test

CLASS ltcl_test IMPLEMENTATION.

  METHOD setup.
    zcl_ags_db=>set_fake( ).

    mo_branch = zcl_ags_repo=>create(
      iv_name        = 'UNITTEST'
      iv_description = 'FOOBAR'
      )->get_default_branch( ).
  ENDMETHOD.

  METHOD add.

    CONSTANTS: lc_filename TYPE string VALUE 'NEW.TXT'.

    DATA: lt_before TYPE zcl_ags_cache=>ty_files_simple_tt,
          lt_after  TYPE zcl_ags_cache=>ty_files_simple_tt.


    lt_before = mo_branch->get_cache( )->list_files_simple( ).

    mo_branch->get_files( )->add(
      iv_filename       = lc_filename
      iv_path           = '/'
      iv_file_contents  = 'WELLO'
      iv_commit_message = 'BLAH' ).

    lt_after = mo_branch->get_cache( )->list_files_simple( ).

    cl_abap_unit_assert=>assert_equals(
      act = lines( lt_before ) + 1
      exp = lines( lt_after ) ).

    READ TABLE lt_after WITH KEY filename = lc_filename TRANSPORTING NO FIELDS.
    cl_abap_unit_assert=>assert_subrc( ).

  ENDMETHOD.

ENDCLASS.

CLASS ltcl_find_folders DEFINITION DEFERRED.
CLASS zcl_ags_file_operations DEFINITION LOCAL FRIENDS ltcl_find_folders.

CLASS ltcl_find_folders DEFINITION FOR TESTING DURATION SHORT RISK LEVEL HARMLESS FINAL.

  PRIVATE SECTION.
    CONSTANTS: c_root TYPE string VALUE '/'.

    DATA: mo_branch TYPE REF TO zcl_ags_branch.

    METHODS:
      setup RAISING zcx_ags_error,
      test01 FOR TESTING RAISING zcx_ags_error,
      test02 FOR TESTING RAISING zcx_ags_error,
      test03 FOR TESTING RAISING zcx_ags_error.

ENDCLASS.       "ltcl_Find_Folders

CLASS ltcl_find_folders IMPLEMENTATION.

  METHOD setup.
    zcl_ags_db=>set_fake( ).

    mo_branch = zcl_ags_repo=>create(
      iv_name        = 'UNITTEST'
      iv_description = 'FOOBAR'
      )->get_default_branch( ).
  ENDMETHOD.

  METHOD test01.

    DATA: lt_files   TYPE zcl_ags_cache=>ty_files_simple_tt,
          lt_folders TYPE zcl_ags_file_operations=>ty_folders_tt.

    FIELD-SYMBOLS: <ls_file>   LIKE LINE OF lt_files,
                   <ls_folder> LIKE LINE OF lt_folders.


    APPEND INITIAL LINE TO lt_files ASSIGNING <ls_file>.
    <ls_file>-filename  = 'FOO.TXT'.
    <ls_file>-path      = c_root.
    <ls_file>-blob_sha1 = '12345'.
    <ls_file>-chmod     = zcl_ags_obj_tree=>c_chmod-file.

    lt_folders = mo_branch->get_files( )->find_folders( lt_files ).

    cl_abap_unit_assert=>assert_equals(
      act = lines( lt_folders )
      exp = 1 ).

    READ TABLE lt_folders INDEX 1 ASSIGNING <ls_folder>.
    cl_abap_unit_assert=>assert_subrc( ).

    cl_abap_unit_assert=>assert_equals(
      act = <ls_folder>-path
      exp = c_root ).

  ENDMETHOD.

  METHOD test02.

    DATA: lt_files   TYPE zcl_ags_cache=>ty_files_simple_tt,
          lt_folders TYPE zcl_ags_file_operations=>ty_folders_tt.

    FIELD-SYMBOLS: <ls_file>   LIKE LINE OF lt_files,
                   <ls_folder> LIKE LINE OF lt_folders.


    APPEND INITIAL LINE TO lt_files ASSIGNING <ls_file>.
    <ls_file>-filename  = 'FOO.TXT'.
    <ls_file>-path      = c_root.
    <ls_file>-blob_sha1 = '12345'.
    <ls_file>-chmod     = zcl_ags_obj_tree=>c_chmod-file.

    APPEND INITIAL LINE TO lt_files ASSIGNING <ls_file>.
    <ls_file>-filename  = 'BAR.TXT'.
    <ls_file>-path      = c_root.
    <ls_file>-blob_sha1 = '12345'.
    <ls_file>-chmod     = zcl_ags_obj_tree=>c_chmod-file.

    lt_folders = mo_branch->get_files( )->find_folders( lt_files ).

    cl_abap_unit_assert=>assert_equals(
      act = lines( lt_folders )
      exp = 1 ).

    READ TABLE lt_folders INDEX 1 ASSIGNING <ls_folder>.
    cl_abap_unit_assert=>assert_subrc( ).

    cl_abap_unit_assert=>assert_equals(
      act = <ls_folder>-path
      exp = c_root ).

  ENDMETHOD.

  METHOD test03.

    DATA: lt_files   TYPE zcl_ags_cache=>ty_files_simple_tt,
          lt_folders TYPE zcl_ags_file_operations=>ty_folders_tt.

    FIELD-SYMBOLS: <ls_file> LIKE LINE OF lt_files.


    APPEND INITIAL LINE TO lt_files ASSIGNING <ls_file>.
    <ls_file>-filename  = 'FOO.TXT'.
    <ls_file>-path      = '/FOO/BAR/'.
    <ls_file>-blob_sha1 = '12345'.
    <ls_file>-chmod     = zcl_ags_obj_tree=>c_chmod-file.

    lt_folders = mo_branch->get_files( )->find_folders( lt_files ).

    cl_abap_unit_assert=>assert_equals(
      act = lines( lt_folders )
      exp = 3 ).

    READ TABLE lt_folders WITH KEY path = '/' TRANSPORTING NO FIELDS.
    cl_abap_unit_assert=>assert_subrc( ).
    READ TABLE lt_folders WITH KEY path = '/FOO/' TRANSPORTING NO FIELDS.
    cl_abap_unit_assert=>assert_subrc( ).
    READ TABLE lt_folders WITH KEY path = '/FOO/BAR/' TRANSPORTING NO FIELDS.
    cl_abap_unit_assert=>assert_subrc( ).

  ENDMETHOD.

ENDCLASS.

CLASS ltcl_extract_folder_name DEFINITION DEFERRED.
CLASS zcl_ags_file_operations DEFINITION LOCAL FRIENDS ltcl_extract_folder_name.

CLASS ltcl_extract_folder_name DEFINITION FOR TESTING DURATION SHORT RISK LEVEL HARMLESS FINAL.

  PRIVATE SECTION.
    METHODS:
      test01 FOR TESTING,
      test02 FOR TESTING.

ENDCLASS.       "ltcl_Extract_Folder_Name

CLASS ltcl_extract_folder_name IMPLEMENTATION.

  METHOD test01.

    DATA: lv_result TYPE string.

    lv_result = zcl_ags_file_operations=>extract_folder_name( '/foo/' ).

    cl_abap_unit_assert=>assert_equals(
      act = lv_result
      exp = 'foo' ).

  ENDMETHOD.

  METHOD test02.

    DATA: lv_result TYPE string.

    lv_result = zcl_ags_file_operations=>extract_folder_name( '/foo/bar/' ).

    cl_abap_unit_assert=>assert_equals(
      act = lv_result
      exp = 'bar' ).

  ENDMETHOD.

ENDCLASS.

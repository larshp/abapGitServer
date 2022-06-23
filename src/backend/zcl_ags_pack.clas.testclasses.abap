
*----------------------------------------------------------------------*
*       CLASS ltcl_encode DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS ltcl_encode DEFINITION FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS
  FINAL.

  PRIVATE SECTION.
    METHODS: test1 FOR TESTING
      RAISING zcx_ags_error.

ENDCLASS.       "ltcl_Encode

*----------------------------------------------------------------------*
*       CLASS ltcl_encode IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS ltcl_encode IMPLEMENTATION.

  METHOD test1.

    CONSTANTS: lc_data TYPE xstring VALUE '12345'.

    DATA: lo_blob    TYPE REF TO zcl_ags_obj_blob,
          lv_raw     TYPE xstring,
          lt_objects TYPE zif_abapgit_definitions=>ty_objects_tt,
          lt_result  TYPE zif_abapgit_definitions=>ty_objects_tt.


    lo_blob = zcl_ags_obj_blob=>new( 'test' ).
    lo_blob->set_data( lc_data ).

    lt_objects = zcl_ags_pack=>explode( iv_repo = 'test' ii_object = lo_blob ).

    lv_raw = zcl_ags_pack=>encode( lt_objects ).
    lt_result = zcl_ags_pack=>decode( lv_raw ).

    cl_abap_unit_assert=>assert_not_initial( lv_raw ).

    cl_abap_unit_assert=>assert_not_initial( lt_objects ).

    cl_abap_unit_assert=>assert_equals(
      act = lt_result
      exp = lt_objects ).

  ENDMETHOD.                    "test1

ENDCLASS.                    "ltcl_encode IMPLEMENTATION

*----------------------------------------------------------------------*
*       CLASS ltcl_explode DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS ltcl_explode DEFINITION FOR TESTING DURATION SHORT RISK LEVEL HARMLESS FINAL.

  PRIVATE SECTION.
    METHODS:
      simple FOR TESTING
        RAISING zcx_ags_error.

ENDCLASS.       "ltcl_Encode

*----------------------------------------------------------------------*
*       CLASS ltcl_explode IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS ltcl_explode IMPLEMENTATION.

  METHOD simple.

    DATA: lt_result TYPE zif_abapgit_definitions=>ty_objects_tt,
          lo_blob   TYPE REF TO zcl_ags_obj_blob.


    lo_blob = zcl_ags_obj_blob=>new( 'test' ).
    lt_result = zcl_ags_pack=>explode( iv_repo = 'test' ii_object = lo_blob ).

    cl_abap_unit_assert=>assert_equals(
      act = lines( lt_result )
      exp = 1 ).

  ENDMETHOD.                    "test1

ENDCLASS.                    "ltcl_explode IMPLEMENTATION


CLASS ltcl_explode_repo DEFINITION FOR TESTING DURATION SHORT RISK LEVEL HARMLESS FINAL.

  PRIVATE SECTION.

    DATA: mo_commit TYPE REF TO zcl_ags_obj_commit,
          mv_repo   TYPE zags_repos-repo.

    METHODS:
      setup RAISING zcx_ags_error,
      test FOR TESTING
        RAISING zcx_ags_error.

ENDCLASS.

CLASS ltcl_explode_repo IMPLEMENTATION.

  METHOD setup.

    DATA: lo_repo TYPE REF TO zcl_ags_repo.

    zcl_ags_db=>set_fake( ).

    lo_repo = zcl_ags_repo=>create(
      iv_name        = 'UNITTEST'
      iv_description = 'FOOBAR' ).

    mv_repo = lo_repo->get_data( )-repo.

    mo_commit = zcl_ags_obj_commit=>load(
        iv_repo = mv_repo
        iv_sha1 = lo_repo->get_default_branch( )->get_data( )-sha1 ).

  ENDMETHOD.

  METHOD test.

    DATA: lt_result TYPE zif_abapgit_definitions=>ty_objects_tt.

    lt_result = zcl_ags_pack=>explode(
      iv_repo   = mv_repo
      ii_object = mo_commit ).

* expect: 1 commit + 1 tree + 1 blob
    cl_abap_unit_assert=>assert_equals(
      act = lines( lt_result )
      exp = 3 ).
    READ TABLE lt_result WITH KEY type COMPONENTS type = zif_ags_constants=>c_type-commit
      TRANSPORTING NO FIELDS.
    cl_abap_unit_assert=>assert_subrc( ).
    READ TABLE lt_result WITH KEY type COMPONENTS type = zif_ags_constants=>c_type-tree
      TRANSPORTING NO FIELDS.
    cl_abap_unit_assert=>assert_subrc( ).
    READ TABLE lt_result WITH KEY type COMPONENTS type = zif_ags_constants=>c_type-blob
      TRANSPORTING NO FIELDS.
    cl_abap_unit_assert=>assert_subrc( ).

  ENDMETHOD.

ENDCLASS.

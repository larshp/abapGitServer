*----------------------------------------------------------------------*
*       CLASS ltcl_test DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS ltcl_test DEFINITION FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS
  FINAL.

  PRIVATE SECTION.
    CONSTANTS: c_sha1 TYPE zags_sha1 VALUE '5f46cb3c4b7f0b3600b64f744cde614a283a88dc'.

    METHODS:
      serialize FOR TESTING
        RAISING zcx_ags_error.

ENDCLASS.       "ltcl_Test


*----------------------------------------------------------------------*
*       CLASS ltcl_test IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS ltcl_test IMPLEMENTATION.

  METHOD serialize.

    CONSTANTS: lc_field TYPE string
      VALUE 'Foobar <foo@bar.com> 1466596513 +0000' ##no_text.

    DATA: lo_old  TYPE REF TO zcl_ags_obj_commit,
          lo_new  TYPE REF TO zcl_ags_obj_commit,
          lv_xstr TYPE xstring.


    CREATE OBJECT lo_old.
    lo_old->set_author( lc_field ).
    lo_old->set_body( 'body' ).
    lo_old->set_committer( lc_field ).
    lo_old->set_parent( c_sha1 ).
    lo_old->set_tree( c_sha1 ).
    lv_xstr = lo_old->zif_ags_object~serialize( ).

    CREATE OBJECT lo_new.
    lo_new->zif_ags_object~deserialize( lv_xstr ).

    cl_abap_unit_assert=>assert_equals(
        act = lo_new->get( )-author
        exp = lo_old->get( )-author ).

    cl_abap_unit_assert=>assert_equals(
        act = lo_new->get( )-body
        exp = lo_old->get( )-body ).

    cl_abap_unit_assert=>assert_equals(
        act = lo_new->get( )-committer
        exp = lo_old->get( )-committer ).

    cl_abap_unit_assert=>assert_equals(
        act = lo_new->get( )-parent
        exp = lo_old->get( )-parent ).

    cl_abap_unit_assert=>assert_equals(
        act = lo_new->get( )-tree
        exp = lo_old->get( )-tree ).

  ENDMETHOD.                    "serialize

ENDCLASS.                    "ltcl_test IMPLEMENTATION

CLASS ltcl_userfield DEFINITION DEFERRED.
CLASS zcl_ags_obj_commit DEFINITION LOCAL FRIENDS ltcl_userfield.

*----------------------------------------------------------------------*
*       CLASS ltcl_userfield DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS ltcl_userfield DEFINITION FOR TESTING
    DURATION SHORT
    RISK LEVEL HARMLESS
    FINAL.

  PRIVATE SECTION.
    DATA: mo_commit TYPE REF TO zcl_ags_obj_commit.

    METHODS:
      setup
        RAISING zcx_ags_error,
      parse_userfield1 FOR TESTING
        RAISING zcx_ags_error,
      parse_userfield2 FOR TESTING
        RAISING zcx_ags_error,
      parse_userfield3 FOR TESTING
        RAISING zcx_ags_error,
      error FOR TESTING.

ENDCLASS.       "ltcl_Userfield

*----------------------------------------------------------------------*
*       CLASS ltcl_userfield IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS ltcl_userfield IMPLEMENTATION.

  METHOD setup.
    CREATE OBJECT mo_commit.
  ENDMETHOD.                    "setup

  METHOD parse_userfield1.

    CONSTANTS: lc_field TYPE string
      VALUE 'Foobar <foo@bar.com> 1466596513 +0000' ##no_text.

    DATA: ls_field TYPE zcl_ags_obj_commit=>ty_userfield.


    ls_field = mo_commit->parse_userfield( lc_field ).

    cl_abap_unit_assert=>assert_not_initial( ls_field ).

    cl_abap_unit_assert=>assert_equals(
        act = ls_field-email
        exp = 'foo@bar.com' ).

    cl_abap_unit_assert=>assert_equals(
        act = ls_field-name
        exp = 'Foobar' ).

  ENDMETHOD.                    "parse_userfield

  METHOD parse_userfield2.

    CONSTANTS: lc_field TYPE string
      VALUE 'SAP* <SAP*@localhost> 1482397806 +0100' ##no_text.

    DATA: ls_field TYPE zcl_ags_obj_commit=>ty_userfield.


    ls_field = mo_commit->parse_userfield( lc_field ).

    cl_abap_unit_assert=>assert_not_initial( ls_field ).

    cl_abap_unit_assert=>assert_equals(
        act = ls_field-email
        exp = 'SAP*@localhost' ).

    cl_abap_unit_assert=>assert_equals(
        act = ls_field-name
        exp = 'SAP*' ).

  ENDMETHOD.

  METHOD parse_userfield3.

* this is actually the UNAM saved in REPOSRC, bug in SAP standard?
    CONSTANTS: lc_field TYPE string
      VALUE 'SAP*........ <SAP*........@localhost> 1484987134 +0100' ##no_text.

    DATA: ls_field TYPE zcl_ags_obj_commit=>ty_userfield.


    ls_field = mo_commit->parse_userfield( lc_field ).

    cl_abap_unit_assert=>assert_not_initial( ls_field ).

    cl_abap_unit_assert=>assert_equals(
        act = ls_field-name
        exp = 'SAP*........' ).

  ENDMETHOD.

  METHOD error.

    TRY.
        mo_commit->parse_userfield( 'something' ).
        cl_abap_unit_assert=>fail( ).
      CATCH zcx_ags_error ##no_handler.
    ENDTRY.

  ENDMETHOD.                    "error

ENDCLASS.                    "ltcl_userfield IMPLEMENTATION

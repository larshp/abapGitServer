
CLASS ltcl_test DEFINITION FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS
  FINAL.

  PRIVATE SECTION.
    CONSTANTS: c_sha1 TYPE zags_sha1 VALUE '5F46CB3C4B7F0B3600B64F744CDE614A283A88DC'.

    METHODS:
      serialize FOR TESTING
        RAISING zcx_ags_error.

ENDCLASS.       "ltcl_Test


CLASS ltcl_test IMPLEMENTATION.

  METHOD serialize.

    DATA: lo_old  TYPE REF TO zcl_ags_obj_commit,
          lo_new  TYPE REF TO zcl_ags_obj_commit,
          lv_xstr TYPE xstring.


    CREATE OBJECT lo_old.
    lo_old->set_author( 'author' ).
    lo_old->set_body( 'body' ).
    lo_old->set_committer( 'committer' ).
    lo_old->set_parent( c_sha1 ).
    lo_old->set_tree( c_sha1 ).
    lv_xstr = lo_old->zif_ags_object~serialize( ).

    CREATE OBJECT lo_new.
    lo_new->zif_ags_object~deserialize( lv_xstr ).

    cl_abap_unit_assert=>assert_equals(
        act = lo_new->get_author( )
        exp = lo_old->get_author( ) ).

    cl_abap_unit_assert=>assert_equals(
        act = lo_new->get_body( )
        exp = lo_old->get_body( ) ).

    cl_abap_unit_assert=>assert_equals(
        act = lo_new->get_committer( )
        exp = lo_old->get_committer( ) ).

    cl_abap_unit_assert=>assert_equals(
        act = lo_new->get_parent( )
        exp = lo_old->get_parent( ) ).

    cl_abap_unit_assert=>assert_equals(
        act = lo_new->get_tree( )
        exp = lo_old->get_tree( ) ).

  ENDMETHOD.

ENDCLASS.
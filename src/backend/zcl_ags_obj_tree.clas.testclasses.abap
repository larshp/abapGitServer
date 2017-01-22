
CLASS ltcl_test DEFINITION FOR TESTING
    DURATION SHORT
    RISK LEVEL HARMLESS
    FINAL.

  PRIVATE SECTION.
    CONSTANTS: c_sha1 TYPE zags_sha1 VALUE '5f46cb3c4b7f0b3600b64f744cde614a283a88dc'.

    METHODS: serialize FOR TESTING
      RAISING zcx_ags_error.
ENDCLASS.       "ltcl_Test


CLASS ltcl_test IMPLEMENTATION.

  METHOD serialize.

    DATA: lo_old  TYPE REF TO zcl_ags_obj_tree,
          lo_new  TYPE REF TO zcl_ags_obj_tree,
          lv_xstr TYPE xstring.


    CREATE OBJECT lo_old.
    lo_old->add_file(
      iv_chmod = zcl_ags_obj_tree=>c_chmod-file
      iv_name  = 'foobar.txt'
      iv_sha1  = c_sha1 ).

    lv_xstr = lo_old->zif_ags_object~serialize( ).

    CREATE OBJECT lo_new.
    lo_new->zif_ags_object~deserialize( lv_xstr ).

    cl_abap_unit_assert=>assert_equals(
      act = lines( lo_new->get_files( ) )
      exp = 1 ).

  ENDMETHOD.

ENDCLASS.

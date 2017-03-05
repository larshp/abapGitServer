
CLASS ltcl_test DEFINITION FOR TESTING DURATION SHORT RISK LEVEL HARMLESS FINAL.

  PRIVATE SECTION.
    DATA: mo_repo TYPE REF TO zcl_ags_repo.

    METHODS:
      setup,
      add FOR TESTING,
      delete FOR TESTING,
      modify FOR TESTING.

ENDCLASS.       "ltcl_Test

CLASS ltcl_test IMPLEMENTATION.

  METHOD setup.
    zcl_ags_db=>set_fake( ).

    mo_repo = zcl_ags_repo=>create(
      iv_name        = 'UNITTEST'
      iv_description = 'FOOBAR' ).
  ENDMETHOD.

  METHOD add.
* todo
  ENDMETHOD.

  METHOD delete.
* todo
  ENDMETHOD.

  METHOD modify.
* todo
  ENDMETHOD.

ENDCLASS.

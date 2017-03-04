
CLASS ltcl_test DEFINITION FOR TESTING DURATION SHORT RISK LEVEL HARMLESS FINAL.

  PRIVATE SECTION.
    METHODS:
      setup,
      simple FOR TESTING
        RAISING zcx_ags_error.

ENDCLASS.       "ltcl_Test

CLASS ltcl_test IMPLEMENTATION.

  METHOD setup.

    zcl_ags_db=>set_fake( ).

  ENDMETHOD.

  METHOD simple.

    zcl_ags_repo=>create(
      iv_name        = 'test'
      iv_description = 'test' ).



  ENDMETHOD.

ENDCLASS.

REPORT zabapgitserver.

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-001.
PARAMETERS: p_crepo TYPE c RADIOBUTTON GROUP g1,
            p_rname TYPE zags_repos-name,
            p_cbra  TYPE c RADIOBUTTON GROUP g1,
            p_bname TYPE zags_repos-name.
SELECTION-SCREEN END OF BLOCK b1.

CLASS lcl_app DEFINITION FINAL.

  PUBLIC SECTION.
    CLASS-METHODS: run.

  PRIVATE SECTION.
    CLASS-METHODS:
      create_repository,
      create_branch.

ENDCLASS.

CLASS lcl_app IMPLEMENTATION.

  METHOD run.

    IF p_crepo = abap_true.
      create_repository( ).
    ELSEIF p_cbra = abap_true.
      create_branch( ).
    ELSE.
      ASSERT 1 = 2.
    ENDIF.

  ENDMETHOD.

  METHOD create_repository.

    BREAK-POINT.

  ENDMETHOD.

  METHOD create_branch.

    BREAK-POINT.

  ENDMETHOD.

ENDCLASS.

START-OF-SELECTION.
  lcl_app=>run( ).
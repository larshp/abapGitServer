REPORT zags_delete_repo.

PARAMETERS: p_name TYPE zags_repos-name OBLIGATORY.

START-OF-SELECTION.
  PERFORM run.

FORM run RAISING zcx_ags_error.

  DATA: lo_repo TYPE REF TO zcl_ags_repo.

  CREATE OBJECT lo_repo
    EXPORTING
      iv_name = p_name.

  lo_repo->delete( ).

  WRITE: / 'Done'(001).

ENDFORM.
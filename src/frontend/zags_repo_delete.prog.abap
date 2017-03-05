REPORT zags_repo_delete.

PARAMETERS: p_name TYPE zags_repos-name OBLIGATORY.

START-OF-SELECTION.
  PERFORM run.

FORM run RAISING zcx_ags_error.

  zcl_ags_repo=>get_instance( p_name )->delete( ).

  WRITE: / 'Done'(001).

ENDFORM.

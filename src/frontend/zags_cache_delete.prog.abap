REPORT zags_cache_delete.

START-OF-SELECTION.
  PERFORM run.

FORM run.

  zcl_ags_db=>get_tree_cache( )->delete_all( ).

  WRITE: / 'Done'(001).

ENDFORM.

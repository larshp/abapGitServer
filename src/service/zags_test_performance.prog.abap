REPORT zags_test_performance.
* enables easy performance trace via SE30

PARAMETERS: p_name   TYPE zags_repo_name OBLIGATORY,
            p_branch TYPE zags_branch_name OBLIGATORY DEFAULT 'master'.

PARAMETERS: p_pack TYPE c RADIOBUTTON GROUP g1 DEFAULT 'X',
            p_foo  TYPE c RADIOBUTTON GROUP g1.

START-OF-SELECTION.
  PERFORM run.

FORM run RAISING zcx_ags_error.
  CASE abap_true.
    WHEN p_pack.
      PERFORM pack.
    WHEN p_foo.
      WRITE: 'todo'.
    WHEN OTHERS.
      ASSERT 0 = 1.
  ENDCASE.
ENDFORM.

FORM pack RAISING zcx_ags_error.

  DATA: lo_commit  TYPE REF TO zcl_ags_obj_commit,
        lo_repo    TYPE REF TO zcl_ags_repo,
        lt_objects TYPE zcl_ags_pack=>ty_objects_tt,
        lv_branch  TYPE zags_sha1,
        lv_repo    TYPE zags_repos-repo.

  lo_repo = zcl_ags_repo=>get_instance( p_name ).
  lv_repo = lo_repo->get_data( )-repo.
  lv_branch = lo_repo->get_branch( p_branch )->get_data( )-sha1.

  CREATE OBJECT lo_commit
    EXPORTING
      iv_repo = lv_repo
      iv_sha1 = lv_branch.

  APPEND LINES OF zcl_ags_pack=>explode(
    iv_repo = lv_repo
    ii_object = lo_commit
    iv_deepen = 0 ) TO lt_objects.

  zcl_ags_pack=>encode( lt_objects ).

ENDFORM.

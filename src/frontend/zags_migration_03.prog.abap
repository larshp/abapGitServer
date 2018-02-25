REPORT zags_migration_03.

START-OF-SELECTION.
  PERFORM run.

FORM run.

  DATA: lt_branches TYPE STANDARD TABLE OF zags_branches WITH DEFAULT KEY,
        ls_branch   LIKE LINE OF lt_branches,
        lt_repos    TYPE STANDARD TABLE OF zags_repos WITH DEFAULT KEY,
        ls_repo     LIKE LINE OF lt_repos,
        lv_name     TYPE zags_branches-name.


  SELECT * FROM zags_branches INTO TABLE lt_branches.
  LOOP AT lt_branches INTO ls_branch WHERE NOT name CA '/'.
    CONCATENATE 'refs/heads/' ls_branch-name INTO lv_name.
    UPDATE zags_branches SET name = lv_name
      WHERE repo = ls_branch-repo
      AND branch = ls_branch-branch.
    ASSERT sy-subrc = 0.
  ENDLOOP.

  SELECT * FROM zags_repos INTO TABLE lt_repos.
  LOOP AT lt_repos INTO ls_repo WHERE NOT head CA '/'.
    CONCATENATE 'refs/heads/' ls_repo-head INTO lv_name.
    UPDATE zags_repos SET head = lv_name
      WHERE repo = ls_repo-repo.
    ASSERT sy-subrc = 0.
  ENDLOOP.

  WRITE: / 'Done'.

ENDFORM.

class ZCL_AGS_REPO definition
  public
  create public .

public section.
protected section.
private section.

  methods CREATE
    importing
      !IV_NAME type ZAGS_REPOS-NAME .
ENDCLASS.



CLASS ZCL_AGS_REPO IMPLEMENTATION.


METHOD create.

  DATA: ls_repo TYPE zags_repos.

* todo

ENDMETHOD.
ENDCLASS.
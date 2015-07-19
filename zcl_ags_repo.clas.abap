class ZCL_AGS_REPO definition
  public
  create public .

public section.

  class-methods CREATE
    importing
      !IV_NAME type ZAGS_REPOS-NAME .
protected section.
private section.
ENDCLASS.



CLASS ZCL_AGS_REPO IMPLEMENTATION.


METHOD create.

  DATA: ls_repo TYPE zags_repos.


* todo, error if name already exists

  ls_repo-id   = cl_system_uuid=>if_system_uuid_static~create_uuid_c22( ).
  ls_repo-name = 'foobar'.
  ls_repo-sha1 = '44CDE614A283A88DC5F46CB3C4B7F0B3600B64F7'.

  INSERT zags_repos FROM ls_repo.
  IF sy-subrc <> 0.
    BREAK-POINT.
  ENDIF.

* todo: initialize tree and commit

ENDMETHOD.
ENDCLASS.
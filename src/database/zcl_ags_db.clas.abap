class ZCL_AGS_DB definition
  public
  create public .

public section.

  class-methods GET_BRANCHES
    returning
      value(RO_BRANCHES) type ref to ZCL_AGS_DB_BRANCHES .
  class-methods GET_OBJECTS
    returning
      value(RO_OBJECTS) type ref to ZCL_AGS_DB_OBJECTS .
  class-methods GET_REPOS
    returning
      value(RO_REPOS) type ref to ZCL_AGS_DB_REPOS .
  class-methods SET_FAKE .
protected section.
private section.

  class-data GO_BRANCHES type ref to ZCL_AGS_DB_BRANCHES .
  class-data GO_OBJECTS type ref to ZCL_AGS_DB_OBJECTS .
  class-data GO_REPOS type ref to ZCL_AGS_DB_REPOS .
  class-data GV_FAKE type ABAP_BOOL .
ENDCLASS.



CLASS ZCL_AGS_DB IMPLEMENTATION.


  METHOD get_branches.

    IF go_branches IS INITIAL.
      CREATE OBJECT go_branches.
      IF gv_fake = abap_true.
        go_branches->set_fake( ).
      ENDIF.
    ENDIF.

    ro_branches = go_branches.

  ENDMETHOD.


  METHOD get_objects.

    IF go_objects IS INITIAL.
      CREATE OBJECT go_objects.
      IF gv_fake = abap_true.
        go_objects->set_fake( ).
      ENDIF.
    ENDIF.

    ro_objects = go_objects.

  ENDMETHOD.


  METHOD get_repos.

    IF go_repos IS INITIAL.
      CREATE OBJECT go_repos.
      IF gv_fake = abap_true.
        go_repos->set_fake( ).
      ENDIF.
    ENDIF.

    ro_repos = go_repos.

  ENDMETHOD.


  METHOD set_fake.

    CLEAR go_repos.
    CLEAR go_branches.
    CLEAR go_objects.

    gv_fake = abap_true.

  ENDMETHOD.
ENDCLASS.

CLASS zcl_ags_db DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.

    CLASS-METHODS get_branches
      RETURNING
        VALUE(ro_branches) TYPE REF TO zcl_ags_db_branches .
    CLASS-METHODS get_objects
      RETURNING
        VALUE(ro_objects) TYPE REF TO zcl_ags_db_objects .
    CLASS-METHODS get_repos
      RETURNING
        VALUE(ro_repos) TYPE REF TO zcl_ags_db_repos .
    CLASS-METHODS get_tree_cache
      RETURNING
        VALUE(ro_cache) TYPE REF TO zcl_ags_db_tree_cache .
    CLASS-METHODS set_fake .
  PROTECTED SECTION.
  PRIVATE SECTION.

    CLASS-DATA go_branches TYPE REF TO zcl_ags_db_branches .
    CLASS-DATA go_objects TYPE REF TO zcl_ags_db_objects .
    CLASS-DATA go_repos TYPE REF TO zcl_ags_db_repos .
    CLASS-DATA go_tree_cache TYPE REF TO zcl_ags_db_tree_cache .
    CLASS-DATA gv_fake TYPE abap_bool .
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


  METHOD get_tree_cache.

    IF go_tree_cache IS INITIAL.
      CREATE OBJECT go_tree_cache.
      IF gv_fake = abap_true.
        go_tree_cache->set_fake( ).
      ENDIF.
    ENDIF.

    ro_cache = go_tree_cache.

  ENDMETHOD.


  METHOD set_fake.

    CLEAR go_repos.
    CLEAR go_branches.
    CLEAR go_objects.

    gv_fake = abap_true.

  ENDMETHOD.
ENDCLASS.

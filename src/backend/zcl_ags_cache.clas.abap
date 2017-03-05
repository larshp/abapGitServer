class ZCL_AGS_CACHE definition
  public
  create public .

public section.

  types:
    BEGIN OF ty_file,
        filename    TYPE string,
        sha1        TYPE zags_sha1,
        comment     TYPE string,
        commit_sha1 TYPE zags_sha1,
        time        TYPE zags_unix_time,
      END OF ty_file .
  types:
    ty_files_tt TYPE STANDARD TABLE OF ty_file WITH DEFAULT KEY .

  methods CONSTRUCTOR
    importing
      !IV_REPO type ZAGS_REPO
      !IV_COMMIT type ZAGS_SHA1 .
  methods LIST_COMMITS
    returning
      value(RT_COMMITS) type ZCL_AGS_OBJ_COMMIT=>TY_PRETTY_TT
    raising
      ZCX_AGS_ERROR .
  methods LIST_COMMITS_BY_FILE .
  methods LIST_COMMITS_BY_USER .
  methods LIST_FILES_BY_PATH
    importing
      !IV_PATH type STRING
    returning
      value(RT_FILES) type TY_FILES_TT
    raising
      ZCX_AGS_ERROR .
  methods LIST_FILES_SIMPLE
    returning
      value(RT_FILES) type ZCL_AGS_CACHE=>TY_FILES_TT
    raising
      ZCX_AGS_ERROR .
protected section.
private section.

  data MV_REPO type ZAGS_REPO .
  data MV_COMMIT type ZAGS_SHA1 .
ENDCLASS.



CLASS ZCL_AGS_CACHE IMPLEMENTATION.


  METHOD constructor.
* todo, not sure about the input fields for this constructor
    mv_repo   = iv_repo.
    mv_commit = iv_commit.
  ENDMETHOD.


  METHOD list_commits.

    DATA: lt_visit  TYPE STANDARD TABLE OF zags_sha1,
          ls_data   TYPE zcl_ags_obj_commit=>ty_pretty,
          lv_commit LIKE LINE OF lt_visit.


    APPEND mv_commit TO lt_visit.

    LOOP AT lt_visit INTO lv_commit.
      ls_data = zcl_ags_obj_commit=>get_instance( lv_commit )->get_pretty( ).

      APPEND ls_data TO rt_commits.

      IF NOT ls_data-parent IS INITIAL.
        READ TABLE lt_visit FROM ls_data-parent TRANSPORTING NO FIELDS.
        IF sy-subrc <> 0.
          APPEND ls_data-parent TO lt_visit.
        ENDIF.
      ENDIF.
      IF NOT ls_data-parent2 IS INITIAL.
        READ TABLE lt_visit FROM ls_data-parent2 TRANSPORTING NO FIELDS.
        IF sy-subrc <> 0.
          APPEND ls_data-parent2 TO lt_visit.
        ENDIF.
      ENDIF.

    ENDLOOP.

    SORT rt_commits BY author-time DESCENDING.

  ENDMETHOD.


  METHOD list_commits_by_file.

* todo, return list of commits
* inputs: filename/+path
* https://github.com/larshp/abapGitServer/issues/23
    RETURN.

  ENDMETHOD.


  METHOD list_commits_by_user.

* todo, return list of commits
* input: user
* https://github.com/larshp/abapGitServer/issues/20
    RETURN.

  ENDMETHOD.


  METHOD list_files_by_path.

    DATA: lt_commits TYPE zcl_ags_obj_commit=>ty_pretty_tt,
          lt_current TYPE ty_files_tt,
          lv_changed TYPE abap_bool,
          lo_cache   TYPE REF TO zcl_ags_cache,
          lt_prev    TYPE ty_files_tt.

    FIELD-SYMBOLS: <ls_current> LIKE LINE OF lt_current,
                   <ls_prev>    LIKE LINE OF lt_prev,
                   <ls_output>  LIKE LINE OF rt_files,
                   <ls_commit>  LIKE LINE OF lt_commits.

* todo, implement path
    ASSERT iv_path = '/'.

    lt_commits = list_commits( ).

    rt_files = list_files_simple( ).

    SORT lt_commits BY committer-time ASCENDING.

    LOOP AT lt_commits ASSIGNING <ls_commit>.

      CREATE OBJECT lo_cache
        EXPORTING
          iv_repo   = mv_repo
          iv_commit = <ls_commit>-sha1.
      lt_current = lo_cache->list_files_simple( ).

      LOOP AT lt_current ASSIGNING <ls_current>.
        lv_changed = abap_false.
        READ TABLE lt_prev ASSIGNING <ls_prev>
          WITH KEY filename = <ls_current>-filename.
        IF sy-subrc <> 0
            OR <ls_prev>-sha1 <> <ls_current>-sha1.
          lv_changed = abap_true.
        ENDIF.

        IF lv_changed = abap_true.
          READ TABLE rt_files ASSIGNING <ls_output>
            WITH KEY filename = <ls_current>-filename.
          IF sy-subrc = 0.
            <ls_output>-comment     = <ls_commit>-text.
            <ls_output>-commit_sha1 = <ls_commit>-sha1.
            <ls_output>-time        = <ls_commit>-committer-time.
          ENDIF.
        ENDIF.
      ENDLOOP.

      lt_prev = lt_current.
    ENDLOOP.

  ENDMETHOD.


  METHOD list_files_simple.

    TYPES: BEGIN OF ty_tree,
             sha1 TYPE zags_sha1,
             base TYPE string,
           END OF ty_tree.

    DATA: lo_tree  TYPE REF TO zcl_ags_obj_tree,
          lt_files TYPE zcl_ags_obj_tree=>ty_tree_tt,
          lt_trees TYPE STANDARD TABLE OF ty_tree WITH DEFAULT KEY.

    FIELD-SYMBOLS:
      <ls_input_tree> LIKE LINE OF lt_trees,
      <ls_input_file> LIKE LINE OF lt_files,
      <ls_file>       LIKE LINE OF rt_files,
      <ls_tree>       LIKE LINE OF lt_trees.


    APPEND INITIAL LINE TO lt_trees ASSIGNING <ls_tree>.
    <ls_tree>-sha1 = zcl_ags_obj_commit=>get_instance( mv_commit )->get( )-tree.
    <ls_tree>-base = '/'.

    LOOP AT lt_trees ASSIGNING <ls_input_tree>.
      CREATE OBJECT lo_tree
        EXPORTING
          iv_sha1 = <ls_input_tree>-sha1.
      lt_files = lo_tree->get_files( ).
      LOOP AT lt_files ASSIGNING <ls_input_file>.
        CASE <ls_input_file>-chmod.
          WHEN zcl_ags_obj_tree=>c_chmod-dir.
            APPEND INITIAL LINE TO lt_trees ASSIGNING <ls_tree>.
            <ls_tree>-sha1 = <ls_input_file>-sha1.
            <ls_tree>-base = <ls_input_tree>-base && <ls_input_file>-name && '/'.
          WHEN OTHERS.
            APPEND INITIAL LINE TO rt_files ASSIGNING <ls_file>.
            <ls_file>-filename = <ls_input_tree>-base && <ls_input_file>-name.
            <ls_file>-sha1 = <ls_input_file>-sha1.
        ENDCASE.
      ENDLOOP.
    ENDLOOP.

  ENDMETHOD.
ENDCLASS.

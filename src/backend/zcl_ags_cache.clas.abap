CLASS zcl_ags_cache DEFINITION
  PUBLIC
  CREATE PUBLIC.

  PUBLIC SECTION.

    TYPES:
      BEGIN OF ty_file.
            INCLUDE TYPE zags_tree_cache_data.
    TYPES: comment TYPE string,
           time    TYPE zags_unix_time,
           END OF ty_file.
    TYPES:
      ty_files_tt TYPE STANDARD TABLE OF ty_file WITH DEFAULT KEY .
    TYPES:
      BEGIN OF ty_file_simple,
        filename  TYPE string,
        path      TYPE string,
        blob_sha1 TYPE zags_sha1,
      END OF ty_file_simple.
    TYPES:
      ty_files_simple_tt TYPE STANDARD TABLE OF ty_file_simple WITH DEFAULT KEY .

    METHODS constructor
      IMPORTING
        !iv_repo   TYPE zags_repo
        !iv_commit TYPE zags_sha1 .
    METHODS list_commits
      RETURNING
        VALUE(rt_commits) TYPE zcl_ags_obj_commit=>ty_pretty_tt
      RAISING
        zcx_ags_error .
    METHODS list_commits_by_file .
    METHODS list_commits_by_user .
    METHODS list_files_by_path
      IMPORTING
        !iv_path        TYPE string
      RETURNING
        VALUE(rt_files) TYPE ty_files_tt
      RAISING
        zcx_ags_error .
    METHODS list_files_simple
      RETURNING
        VALUE(rt_files) TYPE ty_files_simple_tt
      RAISING
        zcx_ags_error .
protected section.
private section.

  data MV_REPO type ZAGS_REPO .
  data MV_COMMIT type ZAGS_SHA1 .

  methods READ_TREE_CACHE
    importing
      !IV_PATH type STRING
      !IV_COMMIT type ZAGS_SHA1
    returning
      value(RT_FILES) type TY_FILES_TT
    raising
      ZCX_AGS_ERROR .
  methods SAVE_TREE_CACHE
    importing
      !IT_FILES type TY_FILES_TT
    raising
      ZCX_AGS_ERROR .
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
* inputs: path + filename
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
          lv_changed TYPE abap_bool,
          lo_cache   TYPE REF TO zcl_ags_cache,
          lt_current TYPE ty_files_simple_tt,
          lt_files   TYPE ty_files_simple_tt,
          lt_prev    TYPE ty_files_simple_tt.

    FIELD-SYMBOLS: <ls_current> LIKE LINE OF lt_current,
                   <ls_prev>    LIKE LINE OF lt_prev,
                   <ls_output>  LIKE LINE OF rt_files,
                   <ls_simple>  LIKE LINE OF lt_files,
                   <ls_file>    LIKE LINE OF rt_files,
                   <ls_commit>  LIKE LINE OF lt_commits.


* todo, implement path handling, backend + frontend
    ASSERT iv_path = '/'.

    rt_files = read_tree_cache(
      iv_path   = iv_path
      iv_commit = mv_commit ).
    IF lines( rt_files ) > 0.
      RETURN.
    ENDIF.

* todo, optimize, the next tree can be calculated from exising tree_cache

    lt_commits = list_commits( ).
    lt_files = list_files_simple( ).
    LOOP AT lt_files ASSIGNING <ls_simple>.
      APPEND INITIAL LINE TO rt_files ASSIGNING <ls_file>.
      MOVE-CORRESPONDING <ls_simple> TO <ls_file>.
    ENDLOOP.

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
          WITH KEY filename = <ls_current>-filename
          path = <ls_current>-path.
        IF sy-subrc <> 0 OR <ls_prev>-blob_sha1 <> <ls_current>-blob_sha1.
          lv_changed = abap_true.
        ENDIF.

        IF lv_changed = abap_true.
          READ TABLE rt_files ASSIGNING <ls_output>
            WITH KEY filename = <ls_current>-filename
            path = <ls_current>-path.
          IF sy-subrc = 0.
            <ls_output>-last_commit_sha1 = <ls_commit>-sha1.
            <ls_output>-comment     = <ls_commit>-text.
            <ls_output>-time        = <ls_commit>-committer-time.
          ENDIF.
        ENDIF.
      ENDLOOP.

      lt_prev = lt_current.
    ENDLOOP.

    save_tree_cache( rt_files ).

  ENDMETHOD.


  METHOD list_files_simple.

    TYPES: BEGIN OF ty_tree,
             sha1 TYPE zags_sha1,
             path TYPE string,
           END OF ty_tree.

    DATA: lt_files TYPE zcl_ags_obj_tree=>ty_tree_tt,
          lt_trees TYPE STANDARD TABLE OF ty_tree WITH DEFAULT KEY.

    FIELD-SYMBOLS: <ls_input_tree> LIKE LINE OF lt_trees,
                   <ls_input_file> LIKE LINE OF lt_files,
                   <ls_file>       LIKE LINE OF rt_files,
                   <ls_tree>       LIKE LINE OF lt_trees.


    APPEND INITIAL LINE TO lt_trees ASSIGNING <ls_tree>.
    <ls_tree>-sha1 = zcl_ags_obj_commit=>get_instance( mv_commit )->get( )-tree.
    <ls_tree>-path = '/'.

    LOOP AT lt_trees ASSIGNING <ls_input_tree>.
      lt_files = zcl_ags_obj_tree=>get_instance( <ls_input_tree>-sha1 )->get_files( ).
      LOOP AT lt_files ASSIGNING <ls_input_file>.
        CASE <ls_input_file>-chmod.
          WHEN zcl_ags_obj_tree=>c_chmod-dir.
            APPEND INITIAL LINE TO lt_trees ASSIGNING <ls_tree>.
            <ls_tree>-sha1 = <ls_input_file>-sha1.
            <ls_tree>-path = <ls_input_tree>-path && <ls_input_file>-name && '/'.
          WHEN OTHERS.
            APPEND INITIAL LINE TO rt_files ASSIGNING <ls_file>.
            <ls_file>-filename  = <ls_input_file>-name.
            <ls_file>-path      = <ls_input_tree>-path.
            <ls_file>-blob_sha1 = <ls_input_file>-sha1.
        ENDCASE.
      ENDLOOP.
    ENDLOOP.

  ENDMETHOD.


  METHOD read_tree_cache.

    DATA: lt_cache  TYPE zags_tree_cache_tt,
          ls_commit TYPE zcl_ags_obj_commit=>ty_pretty.

    FIELD-SYMBOLS: <ls_cache> LIKE LINE OF lt_cache,
                   <ls_file1> LIKE LINE OF rt_files,
                   <ls_file2> LIKE LINE OF rt_files.

* todo, iv_path

    lt_cache = zcl_ags_db=>get_tree_cache( )->select(
      iv_repo        = mv_repo
      iv_commit_sha1 = iv_commit ).

    LOOP AT lt_cache ASSIGNING <ls_cache>.
      APPEND INITIAL LINE TO rt_files ASSIGNING <ls_file1>.
      MOVE-CORRESPONDING <ls_cache> TO <ls_file1>.
    ENDLOOP.

    LOOP AT rt_files ASSIGNING <ls_file1> WHERE time IS INITIAL.
      ls_commit = zcl_ags_obj_commit=>get_instance( <ls_file1>-last_commit_sha1 )->get_pretty( ).

      LOOP AT rt_files ASSIGNING <ls_file2> WHERE last_commit_sha1 = <ls_file1>-last_commit_sha1.
        <ls_file2>-comment = ls_commit-text.
        <ls_file2>-time = ls_commit-author-time.
      ENDLOOP.
    ENDLOOP.

  ENDMETHOD.


  METHOD save_tree_cache.

    DATA: lt_cache   TYPE zags_tree_cache_tt,
          lv_counter TYPE zags_tree_cache-counter,
          ls_cache   LIKE LINE OF lt_cache.

    FIELD-SYMBOLS: <ls_file>  LIKE LINE OF it_files.


    lv_counter = 1.

    LOOP AT it_files ASSIGNING <ls_file>.

      CLEAR ls_cache.
      ls_cache-repo             = mv_repo.
      ls_cache-commit_sha1      = mv_commit.
      ls_cache-counter          = lv_counter.
      ls_cache-filename         = <ls_file>-filename.
      ls_cache-path             = <ls_file>-path.
      ls_cache-blob_sha1        = <ls_file>-blob_sha1.
      ls_cache-last_commit_sha1 = <ls_file>-last_commit_sha1.

      INSERT ls_cache INTO TABLE lt_cache.

      lv_counter = lv_counter + 1.
    ENDLOOP.

    zcl_ags_db=>get_tree_cache( )->insert( lt_cache ).

  ENDMETHOD.
ENDCLASS.

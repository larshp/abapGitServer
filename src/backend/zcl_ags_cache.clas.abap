CLASS zcl_ags_cache DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.

    TYPES:
      BEGIN OF ty_file.
            INCLUDE TYPE zags_tree_cache_data.
    TYPES: comment TYPE string,
           time    TYPE zags_unix_time,
           END OF ty_file .
    TYPES:
      ty_files_tt TYPE STANDARD TABLE OF ty_file WITH DEFAULT KEY .
    TYPES:
      BEGIN OF ty_file_simple,
        filename  TYPE string,
        path      TYPE string,
        blob_sha1 TYPE zags_sha1,
        chmod     TYPE zags_chmod,
      END OF ty_file_simple .
    TYPES:
      ty_files_simple_tt TYPE STANDARD TABLE OF ty_file_simple WITH DEFAULT KEY .

    METHODS build
      IMPORTING
        !iv_show_progress TYPE abap_bool DEFAULT abap_false
      RAISING
        zcx_ags_error .
    METHODS constructor
      IMPORTING
        !iv_repo   TYPE zags_repo
        !iv_commit TYPE zags_sha1 .
    METHODS list_commits
      RETURNING
        VALUE(rt_commits) TYPE zcl_ags_obj_commit=>ty_pretty_tt
      RAISING
        zcx_ags_error .
    METHODS list_commits_by_file
      IMPORTING
        !iv_filename      TYPE string
        !iv_path          TYPE string
      RETURNING
        VALUE(rt_commits) TYPE zcl_ags_obj_commit=>ty_pretty_tt
      RAISING
        zcx_ags_error .
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
  PROTECTED SECTION.
  PRIVATE SECTION.

    DATA mv_repo TYPE zags_repo .
    DATA mv_commit TYPE zags_sha1 .

    METHODS bubble_dir
      IMPORTING
        !iv_commit TYPE zags_sha1
        !is_file   TYPE zags_tree_cache_data
      CHANGING
        !ct_files  TYPE zags_tree_cache_data_tt .
    METHODS find_missing
      RETURNING
        VALUE(rt_missing) TYPE zags_sha1_tt
      RAISING
        zcx_ags_error .
    METHODS build_tree_cache
      IMPORTING
        !iv_commit TYPE zags_sha1
      RAISING
        zcx_ags_error .
    METHODS read_tree_cache
      IMPORTING
        !iv_path        TYPE string
        !iv_commit      TYPE zags_sha1
      RETURNING
        VALUE(rt_files) TYPE ty_files_tt
      RAISING
        zcx_ags_error .
    METHODS save_tree_cache
      IMPORTING
        !iv_commit TYPE zags_sha1
        !it_data   TYPE zags_tree_cache_data_tt
      RAISING
        zcx_ags_error .
ENDCLASS.



CLASS ZCL_AGS_CACHE IMPLEMENTATION.


  METHOD bubble_dir.

    DATA: lt_split TYPE STANDARD TABLE OF string WITH DEFAULT KEY,
          lv_name  LIKE LINE OF lt_split,
          lv_path  TYPE string,
          lv_index TYPE i.

    FIELD-SYMBOLS: <ls_file> LIKE LINE OF ct_files.


    IF is_file-path = '/'.
      RETURN.
    ENDIF.

    SPLIT is_file-path AT '/' INTO TABLE lt_split.
    DELETE lt_split WHERE table_line = ''.

    lv_index = lines( lt_split ).
    DO lines( lt_split ) TIMES.
      READ TABLE lt_split INDEX lv_index INTO lv_name.
      ASSERT sy-subrc = 0.
      DELETE lt_split INDEX lv_index.

      lv_path = concat_lines_of( table = lt_split
                                 sep   = '/').
      IF lv_path IS INITIAL.
        lv_path = '/'.
      ELSE.
        CONCATENATE '/' lv_path '/' INTO lv_path.
      ENDIF.

      READ TABLE ct_files ASSIGNING <ls_file> WITH KEY
        filename = lv_name
        path = lv_path
        chmod = zcl_ags_obj_tree=>c_chmod-dir
        BINARY SEARCH.
      ASSERT sy-subrc = 0.
      <ls_file>-last_commit_sha1 = iv_commit.

      lv_index = lv_index - 1.
    ENDDO.

  ENDMETHOD.


  METHOD build.

    DATA: lt_cache   TYPE zags_tree_cache_tt,
          lv_index   TYPE i,
          lt_missing TYPE zags_sha1_tt,
          lv_commit  LIKE LINE OF lt_missing.


    lt_cache = zcl_ags_db=>get_tree_cache( )->select(
      iv_repo        = mv_repo
      iv_commit_sha1 = mv_commit
      iv_max         = 1 ).
    IF lines( lt_cache ) > 0.
      RETURN.
    ENDIF.

    lt_missing = find_missing( ).

    lv_index = lines( lt_missing ).
    DO lines( lt_missing ) TIMES.
      IF iv_show_progress = abap_true.
        cl_progress_indicator=>progress_indicate(
          i_text               = |Building { mv_repo }|
          i_processed          = sy-index
          i_total              = lines( lt_missing )
          i_output_immediately = abap_true ).
      ENDIF.

* start with the oldest
      READ TABLE lt_missing INDEX lv_index INTO lv_commit.
      ASSERT sy-subrc = 0.
      build_tree_cache( lv_commit ).
      lv_index = lv_index - 1.
    ENDDO.

  ENDMETHOD.


  METHOD build_tree_cache.

    DATA: lv_parent  TYPE zags_sha1,
          lt_current TYPE ty_files_simple_tt,
          lt_prev    TYPE TABLE OF zags_tree_cache,
          lt_files   TYPE zags_tree_cache_data_tt,
          ls_file    LIKE LINE OF lt_files,
          lo_cache   TYPE REF TO zcl_ags_cache.

    FIELD-SYMBOLS: <ls_current> LIKE LINE OF lt_current,
                   <ls_output>  LIKE LINE OF lt_files,
                   <ls_prev>    LIKE LINE OF lt_prev.

* todo, handle multiple parents(merge commits)
    lv_parent = zcl_ags_obj_commit=>load(
      iv_repo = mv_repo
      iv_sha1 = iv_commit )->get( )-parent.

    CREATE OBJECT lo_cache
      EXPORTING
        iv_repo   = mv_repo
        iv_commit = iv_commit.
    lt_current = lo_cache->list_files_simple( ).
    LOOP AT lt_current ASSIGNING <ls_current>.
      CLEAR ls_file.
      MOVE-CORRESPONDING <ls_current> TO ls_file.
      IF lv_parent IS INITIAL.
        ls_file-last_commit_sha1 = iv_commit.
      ENDIF.
      INSERT ls_file INTO TABLE lt_files.
    ENDLOOP.
    CLEAR lt_current.

    IF NOT lv_parent IS INITIAL.
      lt_prev = zcl_ags_db=>get_tree_cache( )->select(
        iv_repo        = mv_repo
        iv_commit_sha1 = lv_parent ).
      ASSERT lines( lt_prev ) > 0.
      SORT lt_prev BY filename chmod path.
    ENDIF.

    LOOP AT lt_files ASSIGNING <ls_output>.
      READ TABLE lt_prev ASSIGNING <ls_prev>
        WITH KEY filename = <ls_output>-filename
        path = <ls_output>-path
        chmod = <ls_output>-chmod
        BINARY SEARCH.
      IF sy-subrc = 0
          AND <ls_output>-chmod = zcl_ags_obj_tree=>c_chmod-file
          AND <ls_prev>-blob_sha1 <> <ls_output>-blob_sha1.
* file changed
        <ls_output>-last_commit_sha1 = iv_commit.
        bubble_dir( EXPORTING iv_commit = iv_commit
                              is_file = <ls_output>
                    CHANGING ct_files = lt_files ).
      ELSEIF sy-subrc <> 0
          AND <ls_output>-chmod = zcl_ags_obj_tree=>c_chmod-file.
* new file
        <ls_output>-last_commit_sha1 = iv_commit.
        bubble_dir( EXPORTING iv_commit = iv_commit
                              is_file = <ls_output>
                    CHANGING ct_files = lt_files ).
      ELSEIF sy-subrc = 0.
* not changed, use old commit SHA1
        <ls_output>-last_commit_sha1 = <ls_prev>-last_commit_sha1.
      ELSE.
        <ls_output>-last_commit_sha1 = iv_commit.
      ENDIF.

      ASSERT NOT <ls_output>-last_commit_sha1 IS INITIAL.
    ENDLOOP.

    ASSERT lines( lt_files ) > 0.
    save_tree_cache( iv_commit = iv_commit
                     it_data   = lt_files ).

  ENDMETHOD.


  METHOD constructor.
* todo, not sure about the input fields for this constructor
    mv_repo   = iv_repo.
    mv_commit = iv_commit.
  ENDMETHOD.


  METHOD find_missing.

    DATA: lt_commits TYPE zcl_ags_obj_commit=>ty_pretty_tt,
          lt_cache   TYPE zags_tree_cache_tt.

    FIELD-SYMBOLS: <ls_commit> LIKE LINE OF lt_commits.


* todo, this will break if the git history is rewritten?
    lt_commits = list_commits( ).

    LOOP AT lt_commits ASSIGNING <ls_commit>.
      lt_cache = zcl_ags_db=>get_tree_cache( )->select(
        iv_repo        = mv_repo
        iv_commit_sha1 = <ls_commit>-sha1
        iv_max         = 1 ).
      IF lines( lt_cache ) > 0.
        EXIT. " current loop
      ENDIF.

      APPEND <ls_commit>-sha1 TO rt_missing.
    ENDLOOP.

  ENDMETHOD.


  METHOD list_commits.

    DATA: lt_visit   TYPE STANDARD TABLE OF zags_sha1,
          ls_data    TYPE zcl_ags_obj_commit=>ty_pretty,
          lt_objects TYPE zags_objects_tt,
          lo_commit  TYPE REF TO zcl_ags_obj_commit,
          lv_commit  LIKE LINE OF lt_visit.

    FIELD-SYMBOLS: <ls_object> LIKE LINE OF lt_objects.


* load all commits from repository, this should scale better
* than doing SELECT SINGLE
    lt_objects = zcl_ags_db=>get_objects( )->list_by_type(
      iv_repo = mv_repo
      iv_type = zif_ags_constants=>c_type-commit ).

    APPEND mv_commit TO lt_visit.

    LOOP AT lt_visit INTO lv_commit.
      READ TABLE lt_objects ASSIGNING <ls_object>
        WITH KEY repo = mv_repo sha1 = lv_commit.
      ASSERT sy-subrc = 0.

      lo_commit = zcl_ags_obj_commit=>new( mv_repo ).
      lo_commit->deserialize( <ls_object>-data_raw ).
      ls_data = lo_commit->get_pretty( ).
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

* todo, not needed? or handle in a different way?
    SORT rt_commits STABLE BY author-time DESCENDING.

  ENDMETHOD.


  METHOD list_commits_by_file.

    DATA: lt_cache  TYPE STANDARD TABLE OF zags_tree_cache WITH DEFAULT KEY,
          ls_commit TYPE zcl_ags_obj_commit=>ty_pretty.

    FIELD-SYMBOLS: <ls_cache> LIKE LINE OF lt_cache.


    build( ).

    lt_cache = zcl_ags_db=>get_tree_cache( )->select_by_file(
      iv_repo     = mv_repo
      iv_filename = iv_filename
      iv_path     = iv_path ).

    SORT lt_cache BY last_commit_sha1 ASCENDING.
    DELETE ADJACENT DUPLICATES FROM lt_cache COMPARING last_commit_sha1.

    LOOP AT lt_cache ASSIGNING <ls_cache>.
      ls_commit = zcl_ags_obj_commit=>load(
        iv_repo = mv_repo
        iv_sha1 = <ls_cache>-last_commit_sha1
        )->get_pretty( ).
      APPEND ls_commit TO rt_commits.
    ENDLOOP.

* todo, this is not entirely correct
    SORT rt_commits STABLE BY author-time DESCENDING.

  ENDMETHOD.


  METHOD list_commits_by_user.

* todo, return list of commits
* input: user
* https://github.com/larshp/abapGitServer/issues/20
    RETURN.

  ENDMETHOD.


  METHOD list_files_by_path.

    build( ).

    rt_files = read_tree_cache(
      iv_path   = iv_path
      iv_commit = mv_commit ).

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
    <ls_tree>-sha1 = zcl_ags_obj_commit=>load(
      iv_repo = mv_repo
      iv_sha1 = mv_commit )->get( )-tree.
    <ls_tree>-path = '/'.

    LOOP AT lt_trees ASSIGNING <ls_input_tree>.
      lt_files = zcl_ags_obj_tree=>load(
        iv_repo = mv_repo
        iv_sha1 = <ls_input_tree>-sha1 )->get_files( ).
      LOOP AT lt_files ASSIGNING <ls_input_file>.
        CASE <ls_input_file>-chmod.
          WHEN zcl_ags_obj_tree=>c_chmod-dir.
            APPEND INITIAL LINE TO rt_files ASSIGNING <ls_file>.
            <ls_file>-filename  = <ls_input_file>-name.
            <ls_file>-path      = <ls_input_tree>-path.
            <ls_file>-chmod     = <ls_input_file>-chmod.

            APPEND INITIAL LINE TO lt_trees ASSIGNING <ls_tree>.
            <ls_tree>-sha1 = <ls_input_file>-sha1.
            <ls_tree>-path = <ls_input_tree>-path && <ls_input_file>-name && '/'.
          WHEN OTHERS.
            APPEND INITIAL LINE TO rt_files ASSIGNING <ls_file>.
            <ls_file>-filename  = <ls_input_file>-name.
            <ls_file>-path      = <ls_input_tree>-path.
            <ls_file>-blob_sha1 = <ls_input_file>-sha1.
            <ls_file>-chmod     = <ls_input_file>-chmod.
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


    lt_cache = zcl_ags_db=>get_tree_cache( )->select_by_path(
      iv_repo        = mv_repo
      iv_path        = iv_path
      iv_commit_sha1 = iv_commit ).

    LOOP AT lt_cache ASSIGNING <ls_cache>.
      APPEND INITIAL LINE TO rt_files ASSIGNING <ls_file1>.
      MOVE-CORRESPONDING <ls_cache> TO <ls_file1>.
    ENDLOOP.

    LOOP AT rt_files ASSIGNING <ls_file1> WHERE time IS INITIAL.
      ls_commit = zcl_ags_obj_commit=>load(
        iv_repo = mv_repo
        iv_sha1 = <ls_file1>-last_commit_sha1
        )->get_pretty( ).

      LOOP AT rt_files ASSIGNING <ls_file2>
          WHERE last_commit_sha1 = <ls_file1>-last_commit_sha1.
        <ls_file2>-comment = ls_commit-text.
        <ls_file2>-time = ls_commit-author-time.
      ENDLOOP.
    ENDLOOP.

    SORT rt_files BY chmod DESCENDING filename ASCENDING.

  ENDMETHOD.


  METHOD save_tree_cache.

    DATA: lt_cache   TYPE zags_tree_cache_tt,
          lv_counter TYPE zags_tree_cache-counter,
          ls_cache   LIKE LINE OF lt_cache.

    FIELD-SYMBOLS: <ls_data> LIKE LINE OF it_data.


    lv_counter = 1.
    LOOP AT it_data ASSIGNING <ls_data>.
      CLEAR ls_cache.
      MOVE-CORRESPONDING <ls_data> TO ls_cache.
      ASSERT NOT ls_cache-last_commit_sha1 IS INITIAL.
      ls_cache-repo             = mv_repo.
      ls_cache-commit_sha1      = iv_commit.
      ls_cache-counter          = lv_counter.
      INSERT ls_cache INTO TABLE lt_cache.
      lv_counter = lv_counter + 1.
    ENDLOOP.

    zcl_ags_db=>get_tree_cache( )->insert( lt_cache ).

  ENDMETHOD.
ENDCLASS.

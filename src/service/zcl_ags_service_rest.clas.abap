CLASS zcl_ags_service_rest DEFINITION
  PUBLIC
  CREATE PUBLIC.

  PUBLIC SECTION.

    INTERFACES zif_ags_service.
    INTERFACES zif_swag_handler.

    TYPES:
      BEGIN OF ty_create,
        name        TYPE zags_repos-name,
        description TYPE zags_repos-description,
      END OF ty_create.
    TYPES:
      BEGIN OF ty_branch,
        name   TYPE zags_branch_name,
        time   TYPE zags_unix_time,
        commit TYPE zags_sha1,
        head   TYPE abap_bool,
      END OF ty_branch.
    TYPES:
      BEGIN OF ty_changed_file,
        filename TYPE string,
        old_blob TYPE zags_sha1,
        new_blob TYPE zags_sha1,
      END OF ty_changed_file.
    TYPES:
      ty_changed_files_tt TYPE STANDARD TABLE OF ty_changed_file WITH DEFAULT KEY.
    TYPES:
      BEGIN OF ty_commit.
        INCLUDE TYPE zcl_ags_obj_commit=>ty_pretty.
    TYPES: files TYPE ty_changed_files_tt,
           END OF ty_commit.
    TYPES:
      BEGIN OF ty_file,
        filename    TYPE string,
        sha1        TYPE zags_sha1,
        comment     TYPE string,
        commit_sha1 TYPE zags_sha1,
        time        TYPE zags_unix_time,
      END OF ty_file.
    TYPES:
      ty_branches_tt TYPE STANDARD TABLE OF ty_branch WITH DEFAULT KEY.
    TYPES:
      ty_files_tt TYPE STANDARD TABLE OF ty_file WITH DEFAULT KEY.

    METHODS create_repo
      IMPORTING
        !is_data TYPE ty_create
      RAISING
        zcx_ags_error.
    METHODS edit_repo
      IMPORTING
        !is_data TYPE ty_create
      RAISING
        zcx_ags_error.
    METHODS list_branches
      IMPORTING
        !iv_repo           TYPE zags_repo_name
      RETURNING
        VALUE(rt_branches) TYPE ty_branches_tt
      RAISING
        zcx_ags_error.
    METHODS list_commits
      IMPORTING
        !iv_repo          TYPE zags_repo_name
        !iv_branch        TYPE zags_branch_name
      RETURNING
        VALUE(rt_commits) TYPE zcl_ags_obj_commit=>ty_pretty_tt
      RAISING
        zcx_ags_error.
    METHODS list_files
      IMPORTING
        !iv_repo        TYPE zags_repo_name
        !iv_branch      TYPE zags_branch_name
      RETURNING
        VALUE(rt_files) TYPE ty_files_tt
      RAISING
        zcx_ags_error.
    METHODS list_repos
      RETURNING
        VALUE(rt_list) TYPE zcl_ags_repo=>ty_repos_tt
      RAISING
        zcx_ags_error.
    METHODS read_blob
      IMPORTING
        !iv_repo           TYPE zags_repo_name
        !iv_branch         TYPE string
        !iv_filename       TYPE string
      RETURNING
        VALUE(rv_contents) TYPE xstring
      RAISING
        zcx_ags_error.
    METHODS read_blob_sha1
      IMPORTING
        !iv_sha1           TYPE zags_sha1
      RETURNING
        VALUE(rv_contents) TYPE xstring
      RAISING
        zcx_ags_error.
    METHODS read_commit
      IMPORTING
        !iv_commit     TYPE zags_sha1
      RETURNING
        VALUE(rs_data) TYPE ty_commit
      RAISING
        zcx_ags_error.
  PROTECTED SECTION.
  PRIVATE SECTION.

    CONSTANTS c_base TYPE string VALUE '/sap/zgit/rest' ##NO_TEXT.

    METHODS list_changes
      IMPORTING
        !iv_new         TYPE zags_sha1
        !iv_old         TYPE zags_sha1
      RETURNING
        VALUE(rt_files) TYPE ty_changed_files_tt
      RAISING
        zcx_ags_error.
    METHODS list_files_simple
      IMPORTING
        !iv_tree        TYPE zags_sha1
      RETURNING
        VALUE(rt_files) TYPE ty_files_tt
      RAISING
        zcx_ags_error.
ENDCLASS.



CLASS ZCL_AGS_SERVICE_REST IMPLEMENTATION.


  METHOD create_repo.

    zcl_ags_repo=>create(
      iv_name        = is_data-name
      iv_description = is_data-description ).

  ENDMETHOD.


  METHOD edit_repo.

    DATA: lo_repo TYPE REF TO zcl_ags_repo.


    CREATE OBJECT lo_repo
      EXPORTING
        iv_name = is_data-name.

    lo_repo->set_description( is_data-description ).

  ENDMETHOD.


  METHOD list_branches.

    DATA: lv_head     TYPE zags_sha1,
          lt_branches TYPE zcl_ags_repo=>ty_branches_tt,
          lo_commit   TYPE REF TO zcl_ags_obj_commit,
          lo_repo     TYPE REF TO zcl_ags_repo.

    FIELD-SYMBOLS: <ls_output> LIKE LINE OF rt_branches,
                   <lo_branch> LIKE LINE OF lt_branches.


    CREATE OBJECT lo_repo
      EXPORTING
        iv_name = iv_repo.

    lv_head = lo_repo->get_data( )-head.
    lt_branches = lo_repo->list_branches( ).

    LOOP AT lt_branches ASSIGNING <lo_branch>.
      APPEND INITIAL LINE TO rt_branches ASSIGNING <ls_output>.
      <ls_output>-name = <lo_branch>->get_data( )-name.

      CREATE OBJECT lo_commit
        EXPORTING
          iv_sha1 = <lo_branch>->get_data( )-sha1.

      <ls_output>-time = lo_commit->get_pretty( )-committer-time.
      <ls_output>-commit = lo_commit->sha1( ).
      IF <ls_output>-name = lv_head.
        <ls_output>-head = abap_true.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.


  METHOD list_changes.

    DATA: lt_new    TYPE ty_files_tt,
          lt_old    TYPE ty_files_tt,
          lo_commit TYPE REF TO zcl_ags_obj_commit,
          ls_new    LIKE LINE OF lt_new,
          ls_old    LIKE LINE OF lt_old.

    FIELD-SYMBOLS: <ls_file> LIKE LINE OF rt_files.


    CREATE OBJECT lo_commit
      EXPORTING
        iv_sha1 = iv_new.
    lt_new = list_files_simple( lo_commit->get( )-tree ).
    IF NOT iv_old IS INITIAL AND NOT iv_old CO '0'.
      CREATE OBJECT lo_commit
        EXPORTING
          iv_sha1 = iv_old.
      lt_old = list_files_simple( lo_commit->get( )-tree ).
    ENDIF.

    LOOP AT lt_new INTO ls_new.
* remove unchanged
      DELETE lt_old WHERE filename = ls_new-filename AND sha1 = ls_new-sha1.
      IF sy-subrc = 0.
        DELETE lt_new WHERE filename = ls_new-filename AND sha1 = ls_new-sha1.
        ASSERT sy-subrc = 0.
        CONTINUE.
      ENDIF.

* find changed
      READ TABLE lt_old INTO ls_old WITH KEY filename = ls_new-filename.
      IF sy-subrc = 0.
        DELETE lt_new WHERE filename = ls_new-filename.
        ASSERT sy-subrc = 0.
        DELETE lt_old WHERE filename = ls_new-filename.
        ASSERT sy-subrc = 0.

        APPEND INITIAL LINE TO rt_files ASSIGNING <ls_file>.
        <ls_file>-filename = ls_new-filename.
        <ls_file>-old_blob = ls_old-sha1.
        <ls_file>-new_blob = ls_new-sha1.
        CONTINUE.
      ENDIF.

* find new
      APPEND INITIAL LINE TO rt_files ASSIGNING <ls_file>.
      <ls_file>-filename = ls_new-filename.
      <ls_file>-new_blob = ls_new-sha1.
    ENDLOOP.

* find deleted
    LOOP AT lt_old INTO ls_old.
      APPEND INITIAL LINE TO rt_files ASSIGNING <ls_file>.
      <ls_file>-filename = ls_old-filename.
      <ls_file>-old_blob = ls_old-sha1.
    ENDLOOP.

  ENDMETHOD.


  METHOD list_commits.

    DATA: lt_visit  TYPE STANDARD TABLE OF zags_sha1,
          lo_commit TYPE REF TO zcl_ags_obj_commit,
          ls_data   TYPE zcl_ags_obj_commit=>ty_pretty,
          lo_repo   TYPE REF TO zcl_ags_repo,
          lo_branch TYPE REF TO zcl_ags_branch,
          lv_commit LIKE LINE OF lt_visit.


    CREATE OBJECT lo_repo
      EXPORTING
        iv_name = iv_repo.

    lo_branch = lo_repo->get_branch( iv_branch ).

    APPEND lo_branch->get_data( )-sha1 TO lt_visit.

    LOOP AT lt_visit INTO lv_commit.

      CREATE OBJECT lo_commit
        EXPORTING
          iv_sha1 = lv_commit.
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

    SORT rt_commits BY author-time DESCENDING.

  ENDMETHOD.


  METHOD list_files.

    DATA: lt_commits TYPE zcl_ags_obj_commit=>ty_pretty_tt,
          lt_current TYPE ty_files_tt,
          lv_changed TYPE abap_bool,
          lt_prev    TYPE ty_files_tt.

    FIELD-SYMBOLS: <ls_current> LIKE LINE OF lt_current,
                   <ls_prev>    LIKE LINE OF lt_prev,
                   <ls_output>  LIKE LINE OF rt_files,
                   <ls_commit>  LIKE LINE OF lt_commits.


    lt_commits = list_commits(
      iv_repo    = iv_repo
      iv_branch  = iv_branch ).

    READ TABLE lt_commits INDEX 1 ASSIGNING <ls_commit>.
    IF sy-subrc = 0.
      rt_files = list_files_simple( <ls_commit>-tree ).
    ENDIF.

    SORT lt_commits BY committer-time ASCENDING.

    LOOP AT lt_commits ASSIGNING <ls_commit>.

      lt_current = list_files_simple( <ls_commit>-tree ).

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
    <ls_tree>-sha1 = iv_tree.
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


  METHOD list_repos.

    rt_list = zcl_ags_repo=>list( ).

  ENDMETHOD.


  METHOD read_blob.

    DATA: lv_commit TYPE zags_sha1,
          lo_blob   TYPE REF TO zcl_ags_obj_blob,
          lt_files  TYPE ty_files_tt,
          lo_commit TYPE REF TO zcl_ags_obj_commit,
          lo_repo   TYPE REF TO zcl_ags_repo.

    FIELD-SYMBOLS: <ls_file> LIKE LINE OF lt_files.


    CREATE OBJECT lo_repo
      EXPORTING
        iv_name = iv_repo.

    lv_commit = lo_repo->get_branch( CONV #( iv_branch ) )->get_data( )-sha1.

    CREATE OBJECT lo_commit
      EXPORTING
        iv_sha1 = lv_commit.

    lt_files = list_files_simple( lo_commit->get( )-tree ).

    READ TABLE lt_files ASSIGNING <ls_file>
      WITH KEY filename = '/' && iv_filename.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_ags_error
        EXPORTING
          textid = zcx_ags_error=>m011.
    ENDIF.

    CREATE OBJECT lo_blob
      EXPORTING
        iv_sha1 = <ls_file>-sha1.
    rv_contents = lo_blob->get_data( ).

  ENDMETHOD.


  METHOD read_blob_sha1.

    DATA: lo_blob TYPE REF TO zcl_ags_obj_blob.


    CREATE OBJECT lo_blob
      EXPORTING
        iv_sha1 = iv_sha1.

    rv_contents = lo_blob->get_data( ).

  ENDMETHOD.


  METHOD read_commit.

    DATA: lo_commit TYPE REF TO zcl_ags_obj_commit.


    CREATE OBJECT lo_commit
      EXPORTING
        iv_sha1 = iv_commit.

    MOVE-CORRESPONDING lo_commit->get_pretty( ) TO rs_data.

    rs_data-files = list_changes(
      iv_new = iv_commit
      iv_old = rs_data-parent ).

  ENDMETHOD.


  METHOD zif_ags_service~run.

    DATA: lv_path     TYPE string,
          lv_json_url TYPE string,
          lo_swag     TYPE REF TO zcl_swag.


    CREATE OBJECT lo_swag
      EXPORTING
        ii_server = ii_server
        iv_base   = c_base.
    lo_swag->register( me ).

    lv_json_url = c_base && '/swagger.json'.

    lv_path = ii_server->request->get_header_field( '~path' ).
    IF lv_path = c_base && '/swagger.html'.
      lo_swag->generate_ui(
        iv_json_url = lv_json_url
        iv_title    = 'abapGitServer - Swagger'(008) ).
    ELSEIF lv_path = lv_json_url.
      lo_swag->generate_spec(
        iv_title       = 'abapGitServer'(010)
        iv_description = 'abapGitServer REST functions'(009) ).
    ELSE.
      lo_swag->run( ).
    ENDIF.

  ENDMETHOD.


  METHOD zif_swag_handler~meta.

    FIELD-SYMBOLS: <ls_meta> LIKE LINE OF rt_meta.

    APPEND INITIAL LINE TO rt_meta ASSIGNING <ls_meta>.
    <ls_meta>-summary   = 'List Repositories'(001).
    <ls_meta>-url-regex = '/list$'.
    <ls_meta>-method    = zcl_swag=>c_method-get.
    <ls_meta>-handler   = 'LIST_REPOS'.

    APPEND INITIAL LINE TO rt_meta ASSIGNING <ls_meta>.
    <ls_meta>-summary   = 'Create repository'(003).
    <ls_meta>-url-regex = '/create$'.
    <ls_meta>-method    = zcl_swag=>c_method-post.
    <ls_meta>-handler   = 'CREATE_REPO'.

    APPEND INITIAL LINE TO rt_meta ASSIGNING <ls_meta>.
    <ls_meta>-summary   = 'Edit repository description'(011).
    <ls_meta>-url-regex = '/edit$'.
    <ls_meta>-method    = zcl_swag=>c_method-put.
    <ls_meta>-handler   = 'EDIT_REPO'.

    APPEND INITIAL LINE TO rt_meta ASSIGNING <ls_meta>.
    <ls_meta>-summary   = 'List Branches'(002).
    <ls_meta>-url-regex = '/branches/(\w*)$'.
    APPEND 'IV_REPO' TO <ls_meta>-url-group_names.
    <ls_meta>-method    = zcl_swag=>c_method-get.
    <ls_meta>-handler   = 'LIST_BRANCHES'.

    APPEND INITIAL LINE TO rt_meta ASSIGNING <ls_meta>.
    <ls_meta>-summary   = 'List files'(004).
    <ls_meta>-url-regex = '/tree/(\w*)/(\w*)$'.
    APPEND 'IV_REPO' TO <ls_meta>-url-group_names.
    APPEND 'IV_BRANCH' TO <ls_meta>-url-group_names.
    <ls_meta>-method    = zcl_swag=>c_method-get.
    <ls_meta>-handler   = 'LIST_FILES'.

    APPEND INITIAL LINE TO rt_meta ASSIGNING <ls_meta>.
    <ls_meta>-summary   = 'Read blob'(005).
    <ls_meta>-url-regex = '/blob/(\w*)/(\w+)/(.*)$'.
    APPEND 'IV_REPO' TO <ls_meta>-url-group_names.
    APPEND 'IV_BRANCH' TO <ls_meta>-url-group_names.
    APPEND 'IV_FILENAME' TO <ls_meta>-url-group_names.
    <ls_meta>-method    = zcl_swag=>c_method-get.
    <ls_meta>-handler   = 'READ_BLOB'.

    APPEND INITIAL LINE TO rt_meta ASSIGNING <ls_meta>.
    <ls_meta>-summary   = 'Read blob via SHA1'(012).
    <ls_meta>-url-regex = '/blob/(\w+)$'.
    APPEND 'IV_SHA1' TO <ls_meta>-url-group_names.
    <ls_meta>-method    = zcl_swag=>c_method-get.
    <ls_meta>-handler   = 'READ_BLOB_SHA1'.

    APPEND INITIAL LINE TO rt_meta ASSIGNING <ls_meta>.
    <ls_meta>-summary   = 'Read commit'(006).
    <ls_meta>-url-regex = '/commit/(\w+)$'.
    APPEND 'IV_COMMIT' TO <ls_meta>-url-group_names.
    <ls_meta>-method    = zcl_swag=>c_method-get.
    <ls_meta>-handler   = 'READ_COMMIT'.

    APPEND INITIAL LINE TO rt_meta ASSIGNING <ls_meta>.
    <ls_meta>-summary   = 'List commits'(007).
    <ls_meta>-url-regex = '/commits/(\w+)/(\w+)$'.
    APPEND 'IV_REPO' TO <ls_meta>-url-group_names.
    APPEND 'IV_BRANCH' TO <ls_meta>-url-group_names.
    <ls_meta>-method    = zcl_swag=>c_method-get.
    <ls_meta>-handler   = 'LIST_COMMITS'.

  ENDMETHOD.
ENDCLASS.
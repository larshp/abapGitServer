CLASS zcl_ags_service_rest DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES zif_ags_service .
    INTERFACES zif_swag_handler .

    TYPES:
      BEGIN OF ty_repo_branch,
        branch TYPE zags_branches,
        latest TYPE zcl_ags_obj_commit=>ty_pretty,
      END OF ty_repo_branch .
    TYPES:
      ty_repo_branches_tt TYPE STANDARD TABLE OF ty_repo_branch WITH DEFAULT KEY .
    TYPES:
      BEGIN OF ty_repo,
        repo     TYPE zags_repos,
        branches TYPE ty_repo_branches_tt,
      END OF ty_repo .
    TYPES:
      ty_repos_tt TYPE STANDARD TABLE OF ty_repo WITH DEFAULT KEY .
    TYPES:
      BEGIN OF ty_branch,
        name   TYPE zags_branch_name,
        time   TYPE zags_unix_time,
        commit TYPE zags_sha1,
        head   TYPE abap_bool,
      END OF ty_branch .
    TYPES:
      ty_branches_tt TYPE STANDARD TABLE OF ty_branch WITH DEFAULT KEY .
    TYPES:
      BEGIN OF ty_create,
        name        TYPE zags_repos-name,
        description TYPE zags_repos-description,
      END OF ty_create .
    TYPES:
      BEGIN OF ty_filename_and_path,
        filename TYPE string,
        path     TYPE string,
      END OF ty_filename_and_path .
    TYPES:
      BEGIN OF ty_changed_file,
        filename TYPE string,
        path     TYPE string,
        old_blob TYPE zags_sha1,
        new_blob TYPE zags_sha1,
      END OF ty_changed_file .
    TYPES:
      ty_changed_files_tt TYPE STANDARD TABLE OF ty_changed_file WITH DEFAULT KEY .
    TYPES:
      BEGIN OF ty_commit.
        INCLUDE TYPE zcl_ags_obj_commit=>ty_pretty.
    TYPES: files TYPE ty_changed_files_tt,
           END OF ty_commit .
    TYPES:
      BEGIN OF ty_merge_request_commits,
        anchestor TYPE zags_sha1,
        source TYPE zags_sha1,
        target TYPE zags_sha1,
        source_branch_name TYPE zags_branch_name,
        target_branch_name TYPE zags_branch_name,
        changed_files TYPE HASHED TABLE OF ty_changed_file WITH UNIQUE KEY filename path,
      END OF ty_merge_request_commits.
    TYPES:
      BEGIN OF ty_user,
        name TYPE string,
        email TYPE string,
      END OF ty_user.
    TYPES BEGIN OF ty_merge_request.
      INCLUDE TYPE ty_merge_request_commits AS ap.
      INCLUDE TYPE zags_merge_req AS db.
      INCLUDE TYPE ty_user AS us.
    TYPES END OF ty_merge_request.
    TYPES:
      BEGIN OF ty_create_merge_req,
        reponame TYPE zags_repo_name,
        title TYPE zags_merge_request_title,
        sourcebranch TYPE zags_branch_name,
        targetbranch TYPE zags_branch_name,
      END OF ty_create_merge_req.

    METHODS create_repo
      IMPORTING
        is_data TYPE ty_create
      RAISING
        zcx_ags_error .
    METHODS edit_repo
      IMPORTING
        is_data TYPE ty_create
      RAISING
        zcx_ags_error .
    METHODS list_branches
      IMPORTING
        iv_repo           TYPE zags_repo_name
      RETURNING
        VALUE(rt_branches) TYPE ty_branches_tt
      RAISING
        zcx_ags_error .
    METHODS list_commits
      IMPORTING
        iv_repo          TYPE zags_repo_name
        iv_branch        TYPE zags_branch_name
      RETURNING
        VALUE(rt_commits) TYPE zcl_ags_obj_commit=>ty_pretty_tt
      RAISING
        zcx_ags_error .
    METHODS list_files
      IMPORTING
        iv_repo        TYPE zags_repo_name
        iv_branch      TYPE zags_branch_name
        iv_path        TYPE string
      RETURNING
        VALUE(rt_files) TYPE zcl_ags_cache=>ty_files_tt
      RAISING
        zcx_ags_error .
    METHODS list_repos
      RETURNING
        VALUE(rt_list) TYPE ty_repos_tt
      RAISING
        zcx_ags_error .
    METHODS read_blob
      IMPORTING
        iv_repo           TYPE zags_repo_name
        iv_branch         TYPE zags_branch_name
        iv_filename       TYPE string
      RETURNING
        VALUE(rv_contents) TYPE xstring
      RAISING
        zcx_ags_error .
    METHODS read_blob_sha1
      IMPORTING
        iv_repo           TYPE zags_repos-name
        iv_sha1           TYPE zags_objects-sha1
      RETURNING
        VALUE(rv_contents) TYPE xstring
      RAISING
        zcx_ags_error .
    METHODS read_commit
      IMPORTING
        iv_repo       TYPE zags_repos-name
        iv_commit     TYPE zags_sha1
      RETURNING
        VALUE(rs_data) TYPE ty_commit
      RAISING
        zcx_ags_error .
    METHODS read_history
      IMPORTING
        iv_repo          TYPE zags_repo_name
        iv_branch        TYPE zags_branch_name
        iv_filename      TYPE string
      RETURNING
        VALUE(rt_commits) TYPE zcl_ags_obj_commit=>ty_pretty_tt
      RAISING
        zcx_ags_error .
    METHODS create_merge_request
      IMPORTING
        iv_data TYPE ty_create_merge_req
      RETURNING VALUE(rs_merge_request) TYPE ty_merge_request
      RAISING
        zcx_ags_error .
    METHODS get_anchestor_merge_request
      IMPORTING
        iv_repo           TYPE zags_repo_name
        iv_target_branch  TYPE zags_branch_name
        iv_source_branch  TYPE zags_branch_name
      RETURNING VALUE(rs_commits) TYPE ty_merge_request_commits
      RAISING
        zcx_ags_error .
    METHODS get_merge_request
      IMPORTING
        iv_repo          TYPE zags_repo_name
        iv_id            TYPE zags_merge_request_id
      RETURNING VALUE(rs_merge_request) TYPE ty_merge_request
      RAISING
        zcx_ags_error.
    METHODS list_open_merge_requests
      IMPORTING
        iv_repo          TYPE zags_repo_name
      RETURNING VALUE(rt_requests) TYPE zags_merge_req_tt
      RAISING
        zcx_ags_error.
  PROTECTED SECTION.
  PRIVATE SECTION.

    CONSTANTS c_base TYPE string VALUE '/sap/zabapgitserver/rest' ##NO_TEXT.

    METHODS to_filename_and_path
      IMPORTING
        !iv_filename   TYPE string
      RETURNING
        VALUE(rs_data) TYPE ty_filename_and_path .
    METHODS list_changes
      IMPORTING
        !iv_repo        TYPE zags_repo
        !iv_new         TYPE zags_sha1
        !iv_old         TYPE zags_sha1
      RETURNING
        VALUE(rt_files) TYPE ty_changed_files_tt
      RAISING
        zcx_ags_error .
    METHODS get_user
      RETURNING VALUE(rs_result) TYPE ty_user.
ENDCLASS.



CLASS ZCL_AGS_SERVICE_REST IMPLEMENTATION.


  METHOD create_merge_request.

    DATA(ls_merge_request) = zcl_ags_db_merge_requests=>create(
      iv_repo_name = iv_data-reponame iv_target_branch = iv_data-targetbranch
      iv_source_branch = iv_data-sourcebranch
      iv_title = iv_data-title ).

    DATA(ls_commits) = get_anchestor_merge_request(
      iv_repo = iv_data-reponame
      iv_source_branch = iv_data-sourcebranch
      iv_target_branch = iv_data-targetbranch ).

    rs_merge_request-db = ls_merge_request.
    rs_merge_request-ap = ls_commits.

  ENDMETHOD.


  METHOD create_repo.

    zcl_ags_repo=>create(
      iv_name        = is_data-name
      iv_description = is_data-description ).

  ENDMETHOD.


  METHOD edit_repo.

    zcl_ags_repo=>get_instance( is_data-name )->set_description( is_data-description ).

  ENDMETHOD.


  METHOD get_anchestor_merge_request.
    DATA lt_changed_files TYPE ty_changed_files_tt.

    DATA(lo_repo) = zcl_ags_repo=>get_instance( iv_repo ).
    DATA(lt_source_branch_commits) = list_commits( iv_repo = iv_repo
      iv_branch = iv_source_branch ).
    DATA(lt_target_branch_commits) = list_commits( iv_repo = iv_repo
      iv_branch = iv_target_branch ).

    LOOP AT lt_source_branch_commits REFERENCE INTO DATA(lr_commit).

      IF line_exists( lt_target_branch_commits[ sha1 = lr_commit->*-sha1 ] ).
        rs_commits-anchestor = lr_commit->*-sha1.
        EXIT.
      ENDIF.

      lt_changed_files = list_changes(
        iv_repo = lo_repo->get_data( )-repo
        iv_new = lr_commit->*-sha1
        iv_old = lr_commit->*-parent ).
      LOOP AT lt_changed_files REFERENCE INTO DATA(lr_changed_file).
        INSERT lr_changed_file->* INTO TABLE rs_commits-changed_files.
        IF sy-subrc <> 0.
          rs_commits-changed_files[ filename = lr_changed_file->*-filename
            path = lr_changed_file->*-path ]-old_blob = lr_changed_file->*-old_blob.
        ENDIF.
      ENDLOOP.

    ENDLOOP.

    READ TABLE lt_source_branch_commits REFERENCE INTO lr_commit
      INDEX 1.
    IF sy-subrc = 0.
      rs_commits-source = lr_commit->*-sha1.
    ENDIF.
    READ TABLE lt_target_branch_commits REFERENCE INTO lr_commit
      INDEX 1.
    IF sy-subrc = 0.
      rs_commits-target = lr_commit->*-sha1.
    ENDIF.

    rs_commits-target_branch_name = iv_target_branch.
    rs_commits-source_branch_name = iv_source_branch.
    REPLACE 'refs/heads/' IN rs_commits-target_branch_name WITH ''.
    REPLACE 'refs/heads/' IN rs_commits-source_branch_name WITH ''.

  ENDMETHOD.


  METHOD get_merge_request.

    DATA(ls_merge_request) = zcl_ags_db_merge_requests=>single(
      iv_repo_name = iv_repo iv_id = iv_id ).
    MOVE-CORRESPONDING ls_merge_request TO rs_merge_request-db.
    rs_merge_request-target_branch_name = ls_merge_request-target_branch_name.
    rs_merge_request-source_branch_name = ls_merge_request-source_branch_name.
    rs_merge_request-ap = get_anchestor_merge_request(
      iv_repo = iv_repo iv_target_branch = rs_merge_request-target_branch_name
      iv_source_branch = rs_merge_request-source_branch_name ).
    rs_merge_request-us = get_user( ).

  ENDMETHOD.


  METHOD get_user.

    DATA(lo_user) = zcl_abapgit_user_master_record=>get_instance( sy-uname ).
    rs_result-name = lo_user->get_name( ).
    rs_result-email = lo_user->get_email( ).

  ENDMETHOD.


  METHOD list_branches.

    DATA: lv_head     TYPE zags_sha1,
          lt_branches TYPE zcl_ags_repo=>ty_branches_tt,
          lo_commit   TYPE REF TO zcl_ags_obj_commit,
          lo_repo     TYPE REF TO zcl_ags_repo.

    FIELD-SYMBOLS: <ls_output> LIKE LINE OF rt_branches,
                   <lo_branch> LIKE LINE OF lt_branches.


    lo_repo = zcl_ags_repo=>get_instance( iv_repo ).
    lv_head = lo_repo->get_data( )-head.
    lt_branches = lo_repo->list_branches( ).

    LOOP AT lt_branches ASSIGNING <lo_branch>.
      IF <lo_branch>->is_tag( ) = abap_true.
        CONTINUE.
      ENDIF.

      APPEND INITIAL LINE TO rt_branches ASSIGNING <ls_output>.
      <ls_output>-name = <lo_branch>->get_data( )-name+11.

* add information about last commit
      lo_commit = zcl_ags_obj_commit=>load(
        iv_repo = lo_repo->get_data( )-repo
        iv_sha1 = <lo_branch>->get_data( )-sha1 ).
      <ls_output>-time = lo_commit->get_pretty( )-committer-time.
      <ls_output>-commit = lo_commit->get_sha1( ).
      IF <ls_output>-name = lv_head.
        <ls_output>-head = abap_true.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.


  METHOD list_changes.

    DATA: lt_new   TYPE zcl_ags_cache=>ty_files_simple_tt,
          lt_old   TYPE zcl_ags_cache=>ty_files_simple_tt,
          lo_cache TYPE REF TO zcl_ags_cache,
          ls_new   LIKE LINE OF lt_new,
          lv_index TYPE i,
          ls_old   LIKE LINE OF lt_old.

    FIELD-SYMBOLS: <ls_file> LIKE LINE OF rt_files.


    CREATE OBJECT lo_cache
      EXPORTING
        iv_repo   = iv_repo
        iv_commit = iv_new.
    lt_new = lo_cache->list_files_simple( ).

    IF NOT iv_old IS INITIAL AND NOT iv_old CO '0'.
      CREATE OBJECT lo_cache
        EXPORTING
          iv_repo   = iv_repo
          iv_commit = iv_old.
      lt_old = lo_cache->list_files_simple( ).
      SORT lt_old BY
        filename ASCENDING
        path ASCENDING
        blob_sha1 ASCENDING.
    ENDIF.

    LOOP AT lt_new INTO ls_new WHERE chmod <> zcl_ags_obj_tree=>c_chmod-dir.
      lv_index = sy-tabix.
* remove unchanged
      READ TABLE lt_old TRANSPORTING NO FIELDS WITH KEY
        filename = ls_new-filename
        path = ls_new-path
        blob_sha1 = ls_new-blob_sha1 BINARY SEARCH.
      IF sy-subrc = 0.
        DELETE lt_old INDEX sy-tabix.
        DELETE lt_new INDEX lv_index.
        ASSERT sy-subrc = 0.
        CONTINUE.
      ENDIF.

* find changed
      READ TABLE lt_old INTO ls_old WITH KEY
        filename = ls_new-filename
        path = ls_new-path BINARY SEARCH.
      IF sy-subrc = 0.
        DELETE lt_old INDEX sy-tabix.
        ASSERT sy-subrc = 0.
        DELETE lt_new INDEX lv_index.
        ASSERT sy-subrc = 0.

        APPEND INITIAL LINE TO rt_files ASSIGNING <ls_file>.
        <ls_file>-filename = ls_new-filename.
        <ls_file>-path     = ls_new-path.
        <ls_file>-old_blob = ls_old-blob_sha1.
        <ls_file>-new_blob = ls_new-blob_sha1.
        CONTINUE.
      ENDIF.

* find new
      APPEND INITIAL LINE TO rt_files ASSIGNING <ls_file>.
      <ls_file>-filename = ls_new-filename.
      <ls_file>-path     = ls_new-path.
      <ls_file>-new_blob = ls_new-blob_sha1.
    ENDLOOP.

* find deleted
    LOOP AT lt_old INTO ls_old WHERE chmod <> zcl_ags_obj_tree=>c_chmod-dir.
      APPEND INITIAL LINE TO rt_files ASSIGNING <ls_file>.
      <ls_file>-filename = ls_old-filename.
      <ls_file>-path     = ls_old-path.
      <ls_file>-old_blob = ls_old-blob_sha1.
    ENDLOOP.

  ENDMETHOD.


  METHOD list_commits.

    DATA: lo_branch TYPE REF TO zcl_ags_branch.


    lo_branch = zcl_ags_repo=>get_instance( iv_repo )->get_branch( iv_branch ).

    rt_commits = lo_branch->get_cache( )->list_commits( ).

  ENDMETHOD.


  METHOD list_files.

    rt_files = zcl_ags_repo=>get_instance( iv_repo
      )->get_branch( iv_branch
      )->get_cache(
      )->list_files_by_path( '/' && iv_path ).

  ENDMETHOD.


  METHOD list_open_merge_requests.

    rt_requests = zcl_ags_db_merge_requests=>list_open( iv_repo ).

  ENDMETHOD.


  METHOD list_repos.

    DATA: lt_repos    TYPE zags_repos_tt,
          lo_commit   TYPE REF TO zcl_ags_obj_commit,
          lt_branches TYPE zcl_ags_repo=>ty_branches_tt.

    FIELD-SYMBOLS: <ls_repo>       LIKE LINE OF lt_repos,
                   <lo_branch>     LIKE LINE OF lt_branches,
                   <ls_out_repo>   LIKE LINE OF rt_list,
                   <ls_out_branch> LIKE LINE OF <ls_out_repo>-branches.


    lt_repos = zcl_ags_repo=>list( ).

    LOOP AT lt_repos ASSIGNING <ls_repo>.
      APPEND INITIAL LINE TO rt_list ASSIGNING <ls_out_repo>.
      <ls_out_repo>-repo = <ls_repo>.

      lt_branches = zcl_ags_repo=>get_instance( <ls_repo>-name )->list_branches( ).
      LOOP AT lt_branches ASSIGNING <lo_branch>.
        IF <lo_branch>->is_tag( ) = abap_true.
          CONTINUE.
        ENDIF.

        APPEND INITIAL LINE TO <ls_out_repo>-branches ASSIGNING <ls_out_branch>.
        <ls_out_branch>-branch = <lo_branch>->get_data( ).
        <ls_out_branch>-branch-name = <ls_out_branch>-branch-name+11.

        lo_commit = zcl_ags_obj_commit=>load(
          iv_repo = <ls_repo>-repo
          iv_sha1 = <ls_out_branch>-branch-sha1 ).

        <ls_out_branch>-latest = lo_commit->get_pretty( ).
      ENDLOOP.
    ENDLOOP.

    SORT rt_list BY repo-name AS TEXT ASCENDING.

  ENDMETHOD.


  METHOD read_blob.

    DATA: lt_files TYPE zcl_ags_cache=>ty_files_simple_tt,
          lo_repo  TYPE REF TO zcl_ags_repo,
          ls_fpath TYPE ty_filename_and_path.

    FIELD-SYMBOLS: <ls_file> LIKE LINE OF lt_files.


    ls_fpath = to_filename_and_path( iv_filename ).

    lo_repo = zcl_ags_repo=>get_instance( iv_repo ).

    lt_files = lo_repo->get_branch( iv_branch )->get_cache( )->list_files_simple( ).

    READ TABLE lt_files ASSIGNING <ls_file>
      WITH KEY filename = ls_fpath-filename
      path = ls_fpath-path.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_ags_error
        EXPORTING
          textid = zcx_ags_error=>m011.
    ENDIF.

    rv_contents = zcl_ags_obj_blob=>load(
      iv_repo = lo_repo->get_data( )-repo
      iv_sha1 = <ls_file>-blob_sha1 )->get_data( ).

  ENDMETHOD.


  METHOD read_blob_sha1.

    DATA: lo_repo TYPE REF TO zcl_ags_repo.


    lo_repo = zcl_ags_repo=>get_instance( iv_repo ).

    rv_contents = zcl_ags_obj_blob=>load(
      iv_repo = lo_repo->get_data( )-repo
      iv_sha1 = iv_sha1 )->get_data( ).

  ENDMETHOD.


  METHOD read_commit.

    DATA: lo_repo TYPE REF TO zcl_ags_repo.


    ASSERT NOT iv_repo IS INITIAL.
    ASSERT NOT iv_commit IS INITIAL.

    lo_repo = zcl_ags_repo=>get_instance( iv_repo ).

    MOVE-CORRESPONDING zcl_ags_obj_commit=>load(
        iv_repo = lo_repo->get_data( )-repo
        iv_sha1 = iv_commit )->get_pretty( )
      TO rs_data.

* todo, handle 2 parents, i.e. merge commit?
    rs_data-files = list_changes(
      iv_repo = lo_repo->get_data( )-repo
      iv_new  = iv_commit
      iv_old  = rs_data-parent ).

  ENDMETHOD.


  METHOD read_history.

    DATA ls_fpath TYPE ty_filename_and_path.
    DATA lo_branch TYPE REF TO zcl_ags_branch.


    ls_fpath = to_filename_and_path( iv_filename ).

    IF iv_branch IS INITIAL.
      lo_branch = zcl_ags_repo=>get_instance( iv_repo )->get_default_branch( ).
    ELSE.
      lo_branch = zcl_ags_repo=>get_instance( iv_repo )->get_branch( iv_branch ).
    ENDIF.

    rt_commits = lo_branch->get_cache(
      )->list_commits_by_file(
      iv_filename = ls_fpath-filename
      iv_path     = ls_fpath-path ).

  ENDMETHOD.


  METHOD to_filename_and_path.

    DATA: lv_tmp TYPE string.

    rs_data-filename = iv_filename.
    rs_data-path = '/'.
    WHILE rs_data-filename CA '/'.
      SPLIT rs_data-filename AT '/' INTO lv_tmp rs_data-filename.
      CONCATENATE rs_data-path lv_tmp '/' INTO rs_data-path.
    ENDWHILE.

  ENDMETHOD.


  METHOD zif_ags_service~run.

    DATA: lo_swag TYPE REF TO zcl_swag.


    CREATE OBJECT lo_swag
      EXPORTING
        ii_server = ii_server
        iv_base   = c_base
        iv_title  = 'abapGitServer'.
    lo_swag->register( me ).

    TRY.
        lo_swag->run( ).
      CATCH cx_static_check.
        ASSERT 0 = 1.
    ENDTRY.

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
    <ls_meta>-url-regex = '/branches/([\w-]+)$'.
    APPEND 'IV_REPO' TO <ls_meta>-url-group_names.
    <ls_meta>-method    = zcl_swag=>c_method-get.
    <ls_meta>-handler   = 'LIST_BRANCHES'.

    APPEND INITIAL LINE TO rt_meta ASSIGNING <ls_meta>.
    <ls_meta>-summary   = 'List files'(004).
    <ls_meta>-url-regex = '/tree/([\w-]+)/([\w-]+)/(.*)$'.
    APPEND 'IV_REPO' TO <ls_meta>-url-group_names.
    APPEND 'IV_BRANCH' TO <ls_meta>-url-group_names.
    APPEND 'IV_PATH' TO <ls_meta>-url-group_names.
    <ls_meta>-method    = zcl_swag=>c_method-get.
    <ls_meta>-handler   = 'LIST_FILES'.

    APPEND INITIAL LINE TO rt_meta ASSIGNING <ls_meta>.
    <ls_meta>-summary   = 'Read blob'(005).
    <ls_meta>-url-regex = '/blob/([\w-]+)/([\w-]+)/(.*)$'.
    APPEND 'IV_REPO' TO <ls_meta>-url-group_names.
    APPEND 'IV_BRANCH' TO <ls_meta>-url-group_names.
    APPEND 'IV_FILENAME' TO <ls_meta>-url-group_names.
    <ls_meta>-method    = zcl_swag=>c_method-get.
    <ls_meta>-handler   = 'READ_BLOB'.

    APPEND INITIAL LINE TO rt_meta ASSIGNING <ls_meta>.
    <ls_meta>-summary   = 'Read history'(013).
    <ls_meta>-url-regex = '/history/([\w-]+)/([\w-]+)/(.*)$'.
    APPEND 'IV_REPO' TO <ls_meta>-url-group_names.
    APPEND 'IV_BRANCH' TO <ls_meta>-url-group_names.
    APPEND 'IV_FILENAME' TO <ls_meta>-url-group_names.
    <ls_meta>-method    = zcl_swag=>c_method-get.
    <ls_meta>-handler   = 'READ_HISTORY'.

    APPEND INITIAL LINE TO rt_meta ASSIGNING <ls_meta>.
    <ls_meta>-summary   = 'Read blob via SHA1'(012).
    <ls_meta>-url-regex = '/blob/([\w-]+)/(\w+)$'.
    APPEND 'IV_REPO' TO <ls_meta>-url-group_names.
    APPEND 'IV_SHA1' TO <ls_meta>-url-group_names.
    <ls_meta>-method    = zcl_swag=>c_method-get.
    <ls_meta>-handler   = 'READ_BLOB_SHA1'.

    APPEND INITIAL LINE TO rt_meta ASSIGNING <ls_meta>.
    <ls_meta>-summary   = 'Read commit'(006).
    <ls_meta>-url-regex = '/commit/([\w-]+)/(\w+)$'.
    APPEND 'IV_REPO' TO <ls_meta>-url-group_names.
    APPEND 'IV_COMMIT' TO <ls_meta>-url-group_names.
    <ls_meta>-method    = zcl_swag=>c_method-get.
    <ls_meta>-handler   = 'READ_COMMIT'.

    APPEND INITIAL LINE TO rt_meta ASSIGNING <ls_meta>.
    <ls_meta>-summary   = 'List commits'(007).
    <ls_meta>-url-regex = '/commits/([\w-]+)/([\w-]+)$'.
    APPEND 'IV_REPO' TO <ls_meta>-url-group_names.
    APPEND 'IV_BRANCH' TO <ls_meta>-url-group_names.
    <ls_meta>-method    = zcl_swag=>c_method-get.
    <ls_meta>-handler   = 'LIST_COMMITS'.

    APPEND INITIAL LINE TO rt_meta ASSIGNING <ls_meta>.
    <ls_meta>-summary   = 'Get branch anchestor'.
    <ls_meta>-url-regex = '/anchestor/([\w-]+)/([\w-]+)/([\w-]+)$'.
    APPEND 'IV_REPO' TO <ls_meta>-url-group_names.
    APPEND 'IV_TARGET_BRANCH' TO <ls_meta>-url-group_names.
    APPEND 'IV_SOURCE_BRANCH' TO <ls_meta>-url-group_names.
    <ls_meta>-method    = zcl_swag=>c_method-get.
    <ls_meta>-handler   = 'GET_ANCHESTOR_MERGE_REQUEST'.

    APPEND INITIAL LINE TO rt_meta ASSIGNING <ls_meta>.
    <ls_meta>-summary   = 'Create merge request'.
    <ls_meta>-url-regex = '/create_merge_request$'.
    <ls_meta>-method    = zcl_swag=>c_method-post.
    <ls_meta>-handler   = 'CREATE_MERGE_REQUEST'.

    APPEND INITIAL LINE TO rt_meta ASSIGNING <ls_meta>.
    <ls_meta>-summary   = 'Get single merge requests'.
    <ls_meta>-url-regex = '/merge_request/([\w-]+)/([\w-]+)'.
    APPEND 'IV_REPO' TO <ls_meta>-url-group_names.
    APPEND 'IV_ID' TO <ls_meta>-url-group_names.
    <ls_meta>-method    = zcl_swag=>c_method-get.
    <ls_meta>-handler   = 'GET_MERGE_REQUEST'.

    APPEND INITIAL LINE TO rt_meta ASSIGNING <ls_meta>.
    <ls_meta>-summary   = 'List open merge requests'.
    <ls_meta>-url-regex = '/list_merge_requests/([\w-]+)'.
    APPEND 'IV_REPO' TO <ls_meta>-url-group_names.
    <ls_meta>-method    = zcl_swag=>c_method-get.
    <ls_meta>-handler   = 'LIST_OPEN_MERGE_REQUESTS'.

  ENDMETHOD.
ENDCLASS.

class ZCL_AGS_SERVICE_REST definition
  public
  create public .

public section.

  interfaces ZIF_AGS_SERVICE .
  interfaces ZIF_SWAG_HANDLER .

  types:
    BEGIN OF ty_create,
        name        TYPE zags_repos-name,
        description TYPE zags_repos-description,
      END OF ty_create .
  types:
    BEGIN OF ty_branch,
        name   TYPE zags_branch_name,
        time   TYPE zags_unix_time,
        commit TYPE zags_sha1,
        head   TYPE abap_bool,
      END OF ty_branch .
  types:
    BEGIN OF ty_changed_file,
        filename TYPE string,
        old_blob TYPE zags_sha1,
        new_blob TYPE zags_sha1,
      END OF ty_changed_file .
  types:
    ty_changed_files_tt TYPE STANDARD TABLE OF ty_changed_file WITH DEFAULT KEY .
  types:
    BEGIN OF ty_commit.
        INCLUDE TYPE zcl_ags_obj_commit=>ty_pretty.
    TYPES: files TYPE ty_changed_files_tt,
           END OF ty_commit .
  types:
    ty_branches_tt TYPE STANDARD TABLE OF ty_branch WITH DEFAULT KEY .

  methods CREATE_REPO
    importing
      !IS_DATA type TY_CREATE
    raising
      ZCX_AGS_ERROR .
  methods EDIT_REPO
    importing
      !IS_DATA type TY_CREATE
    raising
      ZCX_AGS_ERROR .
  methods LIST_BRANCHES
    importing
      !IV_REPO type ZAGS_REPO_NAME
    returning
      value(RT_BRANCHES) type TY_BRANCHES_TT
    raising
      ZCX_AGS_ERROR .
  methods LIST_COMMITS
    importing
      !IV_REPO type ZAGS_REPO_NAME
      !IV_BRANCH type ZAGS_BRANCH_NAME
    returning
      value(RT_COMMITS) type ZCL_AGS_OBJ_COMMIT=>TY_PRETTY_TT
    raising
      ZCX_AGS_ERROR .
  methods LIST_FILES
    importing
      !IV_REPO type ZAGS_REPO_NAME
      !IV_BRANCH type ZAGS_BRANCH_NAME
    returning
      value(RT_FILES) type ZCL_AGS_CACHE=>TY_FILES_TT
    raising
      ZCX_AGS_ERROR .
  methods LIST_REPOS
    returning
      value(RT_LIST) type ZAGS_REPOS_TT
    raising
      ZCX_AGS_ERROR .
  methods READ_BLOB
    importing
      !IV_REPO type ZAGS_REPO_NAME
      !IV_BRANCH type ZAGS_BRANCH_NAME
      !IV_FILENAME type STRING
    returning
      value(RV_CONTENTS) type XSTRING
    raising
      ZCX_AGS_ERROR .
  methods READ_BLOB_SHA1
    importing
      !IV_SHA1 type ZAGS_SHA1
    returning
      value(RV_CONTENTS) type XSTRING
    raising
      ZCX_AGS_ERROR .
  methods READ_COMMIT
    importing
      !IV_REPO type ZAGS_REPO
      !IV_COMMIT type ZAGS_SHA1
    returning
      value(RS_DATA) type TY_COMMIT
    raising
      ZCX_AGS_ERROR .
  PROTECTED SECTION.
private section.

  constants C_BASE type STRING value '/sap/zabapgitserver/rest' ##NO_TEXT.

  methods LIST_CHANGES
    importing
      !IV_REPO type ZAGS_REPO
      !IV_NEW type ZAGS_SHA1
      !IV_OLD type ZAGS_SHA1
    returning
      value(RT_FILES) type TY_CHANGED_FILES_TT
    raising
      ZCX_AGS_ERROR .
ENDCLASS.



CLASS ZCL_AGS_SERVICE_REST IMPLEMENTATION.


  METHOD create_repo.

    zcl_ags_repo=>create(
      iv_name        = is_data-name
      iv_description = is_data-description ).

  ENDMETHOD.


  METHOD edit_repo.

    zcl_ags_repo=>get_instance( is_data-name )->set_description( is_data-description ).

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
      APPEND INITIAL LINE TO rt_branches ASSIGNING <ls_output>.
      <ls_output>-name = <lo_branch>->get_data( )-name.

* add information about last commit
      lo_commit = zcl_ags_obj_commit=>get_instance( <lo_branch>->get_data( )-sha1 ).
      <ls_output>-time = lo_commit->get_pretty( )-committer-time.
      <ls_output>-commit = lo_commit->sha1( ).
      IF <ls_output>-name = lv_head.
        <ls_output>-head = abap_true.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.


  METHOD list_changes.

    DATA: lt_new   TYPE zcl_ags_cache=>ty_files_tt,
          lt_old   TYPE zcl_ags_cache=>ty_files_tt,
          lo_cache TYPE REF TO zcl_ags_cache,
          ls_new   LIKE LINE OF lt_new,
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

    DATA: lo_branch TYPE REF TO zcl_ags_branch.


    lo_branch = zcl_ags_repo=>get_instance( iv_repo )->get_branch( iv_branch ).

    rt_commits = lo_branch->get_cache( )->list_commits( ).

  ENDMETHOD.


  METHOD list_files.

    rt_files = zcl_ags_repo=>get_instance( iv_repo
      )->get_branch( iv_branch
      )->get_cache(
      )->list_files_by_path( '/' ).

  ENDMETHOD.


  METHOD list_repos.

    rt_list = zcl_ags_repo=>list( ).

  ENDMETHOD.


  METHOD read_blob.

    DATA: lt_files TYPE zcl_ags_cache=>ty_files_tt,
          lv_tmp   TYPE string.

    FIELD-SYMBOLS: <ls_file> LIKE LINE OF lt_files.


    lt_files = zcl_ags_repo=>get_instance( iv_repo )->get_branch(
      iv_branch )->get_cache( )->list_files_simple( ).

    lv_tmp = '/' && iv_filename.
    READ TABLE lt_files ASSIGNING <ls_file>
      WITH KEY filename = lv_tmp.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_ags_error
        EXPORTING
          textid = zcx_ags_error=>m011.
    ENDIF.

    rv_contents = zcl_ags_obj_blob=>get_instance( <ls_file>-sha1 )->get_data( ).

  ENDMETHOD.


  METHOD read_blob_sha1.

    rv_contents = zcl_ags_obj_blob=>get_instance( iv_sha1 )->get_data( ).

  ENDMETHOD.


  METHOD read_commit.

    ASSERT NOT iv_repo IS INITIAL.
    ASSERT NOT iv_commit IS INITIAL.

    MOVE-CORRESPONDING
      zcl_ags_obj_commit=>get_instance( iv_commit )->get_pretty( )
      TO rs_data.

    rs_data-files = list_changes(
      iv_repo = iv_repo
      iv_new  = iv_commit
      iv_old  = rs_data-parent ).

  ENDMETHOD.


  METHOD zif_ags_service~run.

    DATA: lo_swag TYPE REF TO zcl_swag.


    CREATE OBJECT lo_swag
      EXPORTING
        ii_server = ii_server
        iv_base   = c_base
        iv_title  = 'abapGitServer'.
    lo_swag->register( me ).

    lo_swag->run( ).

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
    <ls_meta>-url-regex = '/commit/(\w+)/(\w+)$'.
    APPEND 'IV_REPO' TO <ls_meta>-url-group_names.
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

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
    BEGIN OF ty_file,
        filename    TYPE string,
        sha1        TYPE zags_sha1,
        comment     TYPE string,
        commit_sha1 TYPE zags_sha1,
        time        TYPE zags_unix_time,
      END OF ty_file .
  types:
    ty_branches_tt TYPE STANDARD TABLE OF ty_branch WITH DEFAULT KEY .
  types:
    ty_files_tt TYPE STANDARD TABLE OF ty_file WITH DEFAULT KEY .

  methods CONSTRUCTOR
    importing
      !II_SERVER type ref to IF_HTTP_SERVER .
  methods EDIT_REPO
    importing
      !IS_DATA type TY_CREATE
    raising
      ZCX_AGS_ERROR .
  methods CREATE_REPO
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
  methods LIST_REPOS
    returning
      value(RT_LIST) type ZCL_AGS_REPO=>TY_REPOS_TT
    raising
      ZCX_AGS_ERROR .
  methods LIST_FILES
    importing
      !IV_REPO type ZAGS_REPO_NAME
      !IV_BRANCH type ZAGS_BRANCH_NAME
    returning
      value(RT_FILES) type TY_FILES_TT
    raising
      ZCX_AGS_ERROR .
  methods READ_BLOB
    importing
      !IV_REPO type ZAGS_REPO_NAME
      !IV_BRANCH type STRING
      !IV_FILENAME type STRING
    returning
      value(RV_CONTENTS) type XSTRING
    raising
      ZCX_AGS_ERROR .
  methods READ_COMMIT
    importing
      !IV_COMMIT type ZAGS_SHA1
    returning
      value(RS_DATA) type ZCL_AGS_OBJ_COMMIT=>TY_PRETTY
    raising
      ZCX_AGS_ERROR .
  PROTECTED SECTION.
  PRIVATE SECTION.

    DATA mi_server TYPE REF TO if_http_server.
    CONSTANTS c_base TYPE string VALUE '/sap/zgit/rest' ##NO_TEXT.

    METHODS list_files_commit
      IMPORTING
        !iv_commit      TYPE zags_sha1
      RETURNING
        VALUE(rt_files) TYPE ty_files_tt
      RAISING
        zcx_ags_error.
ENDCLASS.



CLASS ZCL_AGS_SERVICE_REST IMPLEMENTATION.


  METHOD constructor.

    mi_server = ii_server.

  ENDMETHOD.


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

    DATA(lo_repo) = NEW zcl_ags_repo( iv_repo ).
    DATA(lv_head) = lo_repo->get_data( )-head.
    DATA(lt_branches) = lo_repo->list_branches( ).

    LOOP AT lt_branches ASSIGNING FIELD-SYMBOL(<lo_branch>).
      APPEND INITIAL LINE TO rt_branches ASSIGNING FIELD-SYMBOL(<ls_output>).
      <ls_output>-name = <lo_branch>->get_data( )-name.

      DATA(lo_commit) = NEW zcl_ags_obj_commit( <lo_branch>->get_data( )-sha1 ).
      <ls_output>-time = lo_commit->get_pretty( )-committer-time.
      <ls_output>-commit = lo_commit->sha1( ).
      IF <ls_output>-name = lv_head.
        <ls_output>-head = abap_true.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.


  METHOD list_commits.

    DATA(lo_repo) = NEW zcl_ags_repo( iv_repo ).
    DATA(lo_branch) = lo_repo->get_branch( iv_branch ).
    DATA(lo_commit) = NEW zcl_ags_obj_commit( lo_branch->get_data( )-sha1 ).

    APPEND lo_commit->get_pretty( ) TO rt_commits.
    WHILE NOT lo_commit->get( )-parent IS INITIAL.
      lo_commit = NEW zcl_ags_obj_commit( lo_commit->get( )-parent ).
      APPEND lo_commit->get_pretty( ) TO rt_commits.
    ENDWHILE.

  ENDMETHOD.


  METHOD list_files.

    DATA(lo_repo) = NEW zcl_ags_repo( iv_repo ).
    DATA(lv_commit) = lo_repo->get_branch( iv_branch )->get_data( )-sha1.

    rt_files = list_files_commit( lv_commit ).

  ENDMETHOD.


  METHOD list_files_commit.

    TYPES: BEGIN OF ty_tree,
             sha1 TYPE zags_sha1,
             base TYPE string,
           END OF ty_tree.

    DATA: lt_trees TYPE STANDARD TABLE OF ty_tree WITH DEFAULT KEY.


    DATA(lo_commit) = NEW zcl_ags_obj_commit( iv_commit ).
    APPEND VALUE #( sha1 = lo_commit->get( )-tree base = '/' ) TO lt_trees.

    LOOP AT lt_trees ASSIGNING FIELD-SYMBOL(<ls_tree>).
      DATA(lo_tree) = NEW zcl_ags_obj_tree( <ls_tree>-sha1 ).
      DATA(lt_files) = lo_tree->get_files( ).
      LOOP AT lt_files ASSIGNING FIELD-SYMBOL(<ls_file>).
        CASE <ls_file>-chmod.
          WHEN zcl_ags_obj_tree=>c_chmod-dir.
            APPEND VALUE #(
              sha1 = <ls_file>-sha1
              base = <ls_tree>-base && <ls_file>-name && '/' )
              TO lt_trees.
          WHEN OTHERS.
            APPEND VALUE #(
              filename = <ls_tree>-base && <ls_file>-name
              sha1 = <ls_file>-sha1 )
              TO rt_files.
        ENDCASE.
      ENDLOOP.
    ENDLOOP.

* todo, at some point in time this recursion will break
* due to the number of commits
    IF NOT lo_commit->get( )-parent IS INITIAL.
      DATA(lt_old) = list_files_commit( lo_commit->get( )-parent ).
      LOOP AT rt_files ASSIGNING FIELD-SYMBOL(<ls_output>).
        READ TABLE lt_old ASSIGNING FIELD-SYMBOL(<ls_old>)
          WITH KEY filename = <ls_output>-filename.
        IF sy-subrc <> 0
            OR <ls_old>-sha1 <> <ls_output>-sha1.
          <ls_output>-comment     = lo_commit->get_pretty( )-text.
          <ls_output>-commit_sha1 = lo_commit->sha1( ).
          <ls_output>-time        = lo_commit->get_pretty( )-committer-time.
        ELSE.
          <ls_output>-comment     = <ls_old>-comment.
          <ls_output>-commit_sha1 = <ls_old>-commit_sha1.
          <ls_output>-time        = <ls_old>-time.
        ENDIF.
      ENDLOOP.
    ELSE.
      LOOP AT rt_files ASSIGNING <ls_output>.
        <ls_output>-comment     = lo_commit->get_pretty( )-text.
        <ls_output>-commit_sha1 = lo_commit->sha1( ).
        <ls_output>-time        = lo_commit->get_pretty( )-committer-time.
      ENDLOOP.
    ENDIF.

  ENDMETHOD.


  METHOD list_repos.

    rt_list = zcl_ags_repo=>list( ).

  ENDMETHOD.


  METHOD read_blob.

    DATA(lo_repo) = NEW zcl_ags_repo( iv_repo ).
    DATA(lv_commit) = lo_repo->get_branch( CONV #( iv_branch ) )->get_data( )-sha1.
    DATA(lt_files) = list_files_commit( lv_commit ).

    READ TABLE lt_files ASSIGNING FIELD-SYMBOL(<ls_file>)
      WITH KEY filename = '/' && iv_filename.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_ags_error
        EXPORTING
          textid = zcx_ags_error=>m011.
    ENDIF.

    DATA(lo_blob) = NEW zcl_ags_obj_blob( <ls_file>-sha1 ).
    rv_contents = lo_blob->get_data( ).

  ENDMETHOD.


  METHOD read_commit.

    DATA(lo_commit) = NEW zcl_ags_obj_commit( iv_commit ).
    rs_data = lo_commit->get_pretty( ).

  ENDMETHOD.


  METHOD zif_ags_service~run.

    DATA: lv_path TYPE string.


    DATA(lo_swag) = NEW zcl_swag(
      ii_server = mi_server
      iv_base   = c_base ).
    lo_swag->register( me ).

    DATA(lv_json_url) = c_base && '/swagger.json'.

    lv_path = mi_server->request->get_header_field( '~path' ).
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
    <ls_meta>-produce   = zcl_swag=>c_content_type-text_plain.
    <ls_meta>-handler   = 'READ_BLOB'.

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
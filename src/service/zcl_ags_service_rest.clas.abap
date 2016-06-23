CLASS zcl_ags_service_rest DEFINITION
  PUBLIC
  CREATE PUBLIC.

  PUBLIC SECTION.

    INTERFACES zif_ags_service.

    METHODS constructor
      IMPORTING
        !ii_server TYPE REF TO if_http_server.
  PROTECTED SECTION.
PRIVATE SECTION.

  TYPES:
    BEGIN OF ty_file,
      filename    TYPE string,
      sha1        TYPE zags_sha1,
      comment     TYPE string,
      commit_sha1 TYPE zags_sha1,
      time        TYPE zags_unix_time,
    END OF ty_file.
  TYPES:
    ty_files_tt TYPE STANDARD TABLE OF ty_file WITH DEFAULT KEY.
  TYPES:
    BEGIN OF ty_branch,
      name   TYPE zags_branch_name,
      time   TYPE zags_unix_time,
      commit TYPE zags_sha1,
    END OF ty_branch.
  TYPES:
    ty_branches_tt TYPE STANDARD TABLE OF ty_branch WITH DEFAULT KEY.

  DATA mi_server TYPE REF TO if_http_server.

  METHODS list_branches
    IMPORTING
      !iv_name           TYPE zags_repo_name
    RETURNING
      VALUE(rt_branches) TYPE ty_branches_tt
    RAISING
      zcx_ags_error.
  METHODS create_repo
    RAISING
      zcx_ags_error.
  METHODS json_upper
    RETURNING
      VALUE(rv_json) TYPE xstring.
  METHODS list_commits
    IMPORTING
      !iv_name          TYPE zags_repo_name
      !iv_branch        TYPE zags_branch_name
    RETURNING
      VALUE(rt_commits) TYPE zcl_ags_obj_commit=>ty_pretty_tt
    RAISING
      zcx_ags_error.
  METHODS list_files
    IMPORTING
      !iv_commit      TYPE zags_sha1
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
  METHODS read_commit
    IMPORTING
      !iv_sha1       TYPE zags_sha1
    RETURNING
      VALUE(rs_data) TYPE zcl_ags_obj_commit=>ty_pretty
    RAISING
      zcx_ags_error.
  METHODS to_json
    IMPORTING
      !ig_data       TYPE any
    RETURNING
      VALUE(rv_json) TYPE xstring.
ENDCLASS.



CLASS ZCL_AGS_SERVICE_REST IMPLEMENTATION.


  METHOD constructor.

    mi_server = ii_server.

  ENDMETHOD.


  METHOD create_repo.

    DATA: lv_name        TYPE zags_repos-name,
          lv_description TYPE zags_repos-description.


    DATA(lv_json) = json_upper( ).

    CALL TRANSFORMATION id
      SOURCE XML lv_json
      RESULT name = lv_name
      description = lv_description.

    zcl_ags_repo=>create(
      iv_name        = lv_name
      iv_description = lv_description ).

    mi_server->response->set_cdata( 'ok' ).

  ENDMETHOD.


  METHOD json_upper.
* todo, create lcl_json class instead of methods JSON_UPPER and TO_JSON?
    DATA(lv_json) = mi_server->request->get_cdata( ).

    DATA(lo_writer) = cl_sxml_string_writer=>create( type = if_sxml=>co_xt_json ).

    CALL TRANSFORMATION demo_json_xml_to_upper
      SOURCE XML lv_json
      RESULT XML lo_writer.

    rv_json = lo_writer->get_output( ).

  ENDMETHOD.


  METHOD list_branches.

    DATA(lo_repo) = NEW zcl_ags_repo( iv_name ).
    DATA(lt_branches) = lo_repo->list_branches( ).

    LOOP AT lt_branches ASSIGNING FIELD-SYMBOL(<lo_branch>).
      APPEND INITIAL LINE TO rt_branches ASSIGNING FIELD-SYMBOL(<ls_output>).
      <ls_output>-name = <lo_branch>->get_data( )-name.

      DATA(lo_commit) = NEW zcl_ags_obj_commit( <lo_branch>->get_data( )-sha1 ).
      <ls_output>-time = lo_commit->get_pretty( )-committer-time.
      <ls_output>-commit = lo_commit->sha1( ).
    ENDLOOP.

  ENDMETHOD.


  METHOD list_commits.

    DATA(lo_repo) = NEW zcl_ags_repo( iv_name ).
    DATA(lo_branch) = lo_repo->get_branch( iv_branch ).
    DATA(lo_commit) = NEW zcl_ags_obj_commit( lo_branch->get_data( )-sha1 ).

    APPEND lo_commit->get_pretty( ) TO rt_commits.
    WHILE NOT lo_commit->get( )-parent IS INITIAL.
      lo_commit = NEW zcl_ags_obj_commit( lo_commit->get( )-parent ).
      APPEND lo_commit->get_pretty( ) TO rt_commits.
    ENDWHILE.

  ENDMETHOD.


  METHOD list_files.

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
      DATA(lt_old) = list_files( lo_commit->get( )-parent ).
      LOOP AT rt_files ASSIGNING FIELD-SYMBOL(<ls_output>).
        READ TABLE lt_old ASSIGNING FIELD-SYMBOL(<ls_old>)
          WITH KEY filename = <ls_output>-filename.
        IF sy-subrc <> 0 OR <ls_old>-sha1 <> <ls_output>-sha1.
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
    DATA(lt_files) = list_files( lv_commit ).

    READ TABLE lt_files ASSIGNING FIELD-SYMBOL(<ls_file>)
      WITH KEY filename = iv_filename.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_ags_error
        EXPORTING
          textid = zcx_ags_error=>m011.
    ENDIF.

    DATA(lo_blob) = NEW zcl_ags_obj_blob( <ls_file>-sha1 ).
    rv_contents = lo_blob->get_data( ).

  ENDMETHOD.


  METHOD read_commit.

    DATA(lo_commit) = NEW zcl_ags_obj_commit( iv_sha1 ).
    rs_data = lo_commit->get_pretty( ).

  ENDMETHOD.


  METHOD to_json.

    DATA: lo_writer TYPE REF TO cl_sxml_string_writer.

    lo_writer = cl_sxml_string_writer=>create( if_sxml=>co_xt_json ).
    CALL TRANSFORMATION id
      SOURCE data = ig_data
      RESULT XML lo_writer.
    rv_json = lo_writer->get_output( ).

  ENDMETHOD.


  METHOD zif_ags_service~run.

    DATA: lv_path     TYPE string,
          lv_last     TYPE string,
          lv_filename TYPE string,
          lv_branch   TYPE string,
          lv_name     TYPE zags_repo_name.


    lv_path = mi_server->request->get_header_field( '~path' ).

*****************************
* /list/
* /create/
*
* /repo/(repo)/branches
* /repo/(repo)/blob/(commit/branch)/(filename)
* /repo/(repo)/tree/(branch/sha1)
* /repo/(repo)/commits/(branch)
* /repo/(repo)/commit/(sha1)
*****************************

    DATA(lv_base) = '/sap/zgit/rest'.

    FIND REGEX lv_base && '/repo/(\w*)/'
      IN lv_path
      SUBMATCHES lv_name ##NO_TEXT.

    FIND REGEX '/(\w*)$'
      IN lv_path
      SUBMATCHES lv_last ##NO_TEXT.

    IF lv_path CP lv_base && '/list/'.
      mi_server->response->set_data( to_json( list_repos( ) ) ).
    ELSEIF lv_path CP lv_base && '/create/'.
      create_repo( ).
    ELSEIF lv_path CP lv_base && '/repo/*/branches'.
      mi_server->response->set_data( to_json( list_branches( lv_name ) ) ).
    ELSEIF lv_path CP lv_base && '/repo/*/tree/*'.
      DATA(lo_repo) = NEW zcl_ags_repo( lv_name ).
      DATA(lv_commit) = lo_repo->get_branch( CONV #( lv_last ) )->get_data( )-sha1.
      mi_server->response->set_data( to_json( list_files( lv_commit ) ) ).
    ELSEIF lv_path CP lv_base && '/repo/*/blob/*'.
      FIND REGEX '/blob/(\w+)(/.*)$'
        IN lv_path
        SUBMATCHES lv_branch lv_filename ##NO_TEXT.
      mi_server->response->set_data( read_blob(
        iv_repo     = lv_name
        iv_branch   = lv_branch
        iv_filename = lv_filename ) ).
    ELSEIF lv_path CP lv_base && '/repo/*/commit/*'.
      mi_server->response->set_data( to_json( read_commit( CONV #( lv_last ) ) ) ).
    ELSEIF lv_path CP lv_base && '/repo/*/commits/*'.
      mi_server->response->set_data( to_json(
        list_commits( iv_name   = lv_name
                      iv_branch = CONV #( lv_last ) ) ) ).
    ELSE.
      RAISE EXCEPTION TYPE zcx_ags_error
        EXPORTING
          textid = zcx_ags_error=>m010.
    ENDIF.

  ENDMETHOD.
ENDCLASS.
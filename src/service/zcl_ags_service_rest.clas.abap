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
        filename TYPE string,
      END OF ty_file.
    TYPES:
      ty_files_tt TYPE STANDARD TABLE OF ty_file WITH DEFAULT KEY.

    DATA mi_server TYPE REF TO if_http_server.

    METHODS list_files
      IMPORTING
        !iv_name        TYPE zags_repo_name
      RETURNING
        VALUE(rt_files) TYPE ty_files_tt
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


  METHOD list_files.
* todo, unit test this method
* todo, move this method to somewhere else?
    TYPES: BEGIN OF ty_tree,
             sha1 TYPE zags_sha1,
             base TYPE string,
           END OF ty_tree.

    DATA: lt_trees TYPE STANDARD TABLE OF ty_tree WITH DEFAULT KEY.

    DATA(lo_repo) = NEW zcl_ags_repo( iv_name ).
    DATA(lo_branch) = lo_repo->get_branch( lo_repo->get_data( )-head ).
    DATA(lo_commit) = NEW zcl_ags_obj_commit( lo_branch->get_data( )-sha1 ).
    APPEND VALUE #( sha1 = lo_commit->get_tree( ) base = '/' ) TO lt_trees.

    LOOP AT lt_trees ASSIGNING FIELD-SYMBOL(<ls_tree>).
      DATA(lo_tree) = NEW zcl_ags_obj_tree( <ls_tree>-sha1 ).
      LOOP AT lo_tree->get_files( ) ASSIGNING FIELD-SYMBOL(<ls_file>).
        CASE <ls_file>-chmod.
          WHEN zcl_ags_obj_tree=>c_chmod-dir.
            APPEND VALUE #(
              sha1 = lo_commit->get_tree( )
              base = <ls_tree>-base && <ls_file>-name && '/' )
              TO lt_trees.
          WHEN OTHERS.
            APPEND VALUE #( filename = <ls_tree>-base && <ls_file>-name ) TO rt_files.
        ENDCASE.
      ENDLOOP.
    ENDLOOP.

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

    DATA: lv_path TYPE string,
          lv_name TYPE zags_repo_name.


    lv_path = mi_server->request->get_header_field( '~path' ).

    IF lv_path CP '*/repositories/'.
      DATA(lt_list) = zcl_ags_repo=>list( ).
      mi_server->response->set_data( to_json( lt_list ) ).
    ELSEIF lv_path CP '*/repo/*/files/'.
      FIND REGEX '/sap/zgit/rest/repo/(.*)/files/'
        IN lv_path
        SUBMATCHES lv_name ##NO_TEXT.
      ASSERT sy-subrc = 0.
      mi_server->response->set_data( to_json( list_files( lv_name ) ) ).
    ELSE.
      RAISE EXCEPTION TYPE zcx_ags_error
        EXPORTING
          textid = zcx_ags_error=>m010.
    ENDIF.

  ENDMETHOD.
ENDCLASS.
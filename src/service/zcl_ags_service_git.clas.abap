CLASS zcl_ags_service_git DEFINITION
  PUBLIC
  CREATE PUBLIC.

  PUBLIC SECTION.

    INTERFACES zif_ags_service.
  PROTECTED SECTION.
  PRIVATE SECTION.

    TYPES:
      BEGIN OF ty_push,
        old    TYPE zags_sha1,
        new    TYPE zags_sha1,
        name   TYPE zags_branch_name,
        length TYPE i,
      END OF ty_push.
    TYPES:
      BEGIN OF ty_request,
        want   TYPE STANDARD TABLE OF zags_sha1 WITH DEFAULT KEY,
        deepen TYPE i,
      END OF ty_request.

    DATA mi_server TYPE REF TO if_http_server.

    METHODS branch_list
      RAISING
        zcx_ags_error.
    METHODS decode_push
      IMPORTING
        !iv_data       TYPE xstring
      RETURNING
        VALUE(rs_push) TYPE ty_push
      RAISING
        zcx_ags_error.
    METHODS decode_request
      IMPORTING
        !iv_string        TYPE string
      RETURNING
        VALUE(rs_request) TYPE ty_request.
    METHODS get_null
      RETURNING
        VALUE(rv_char) TYPE char1.
    METHODS pack
      RAISING
        zcx_ags_error.
    METHODS repo_name
      RETURNING
        VALUE(rv_name) TYPE zags_repos-name.
    METHODS unpack
      RAISING
        zcx_ags_error.
    METHODS unpack_ok.
ENDCLASS.



CLASS ZCL_AGS_SERVICE_GIT IMPLEMENTATION.


  METHOD branch_list.

    DEFINE _capability.
      APPEND &1 TO lt_capabilities ##no_text.
    END-OF-DEFINITION.

    DATA: lv_reply        TYPE string,
          lt_reply        TYPE TABLE OF string,
          lv_length       TYPE xstring,
          lv_tmp          TYPE string,
          lt_capabilities TYPE TABLE OF string,
          lv_content      TYPE string,
          lv_utf          TYPE string,
          lv_name         TYPE zags_repos-name,
          lv_head         TYPE zags_sha1,
          lo_repo         TYPE REF TO zcl_ags_repo,
          lt_branches     TYPE zcl_ags_repo=>ty_branches_tt.

    FIELD-SYMBOLS: <lo_branch> LIKE LINE OF lt_branches.


    _capability 'multi_ack'.
    _capability 'thin-pack'.
    _capability 'side-band'.
    _capability 'side-band-64k'.
    _capability 'ofs-delta'.
    _capability 'shallow'.
    _capability 'no-progress'.
    _capability 'include-tag'.
    _capability 'multi_ack_detailed'.
    _capability 'no-done'.
    _capability 'symref=HEAD:refs/heads/master'.
    _capability 'agent=git/abapGitServer'.

    CONCATENATE LINES OF lt_capabilities INTO lv_reply SEPARATED BY space.

    lv_name = repo_name( ).
    CREATE OBJECT lo_repo
      EXPORTING
        iv_name = lv_name.
    lv_head = lo_repo->get_branch( lo_repo->get_data( )-head )->get_data( )-sha1.

    APPEND |001e# service=git-upload-pack| TO lt_reply ##no_text.

    lv_content = |{ lv_head } HEAD{ get_null( ) }{ lv_reply }|.
    lv_length = lcl_length=>encode( strlen( lv_content ) + 5 ).
    lv_utf = to_lower( zcl_ags_util=>xstring_to_string_utf8( lv_length ) ).

    lv_tmp = '0000' && lv_utf && lv_content.
    APPEND lv_tmp TO lt_reply.

    lt_branches = lo_repo->list_branches( ).
    LOOP AT lt_branches ASSIGNING <lo_branch>.
      lv_content = <lo_branch>->get_data( )-sha1
        && ' refs/heads/'
        && <lo_branch>->get_data( )-name ##no_text.

      lv_length = lcl_length=>encode( strlen( lv_content ) + 5 ).
      lv_utf = to_lower( zcl_ags_util=>xstring_to_string_utf8( lv_length ) ).

      lv_tmp = lv_utf && lv_content.
      APPEND lv_tmp TO lt_reply.
    ENDLOOP.

    APPEND '0000' TO lt_reply.

    CONCATENATE LINES OF lt_reply INTO lv_reply
      SEPARATED BY cl_abap_char_utilities=>newline.

    mi_server->response->set_header_field(
      name  = 'Server'
      value = 'abapGitServer' ) ##NO_TEXT.
    mi_server->response->set_header_field(
      name  = 'Cache-Control'
      value = 'no-cache' ) ##NO_TEXT.
    mi_server->response->set_content_type(
      'application/x-git-upload-pack-advertisement' ) ##NO_TEXT.

* must be sent as raw, using data will change the content-type of the response
    DATA(lv_raw) = zcl_ags_util=>string_to_xstring_utf8( lv_reply ).
    mi_server->response->set_data( lv_raw ).

  ENDMETHOD.


  METHOD decode_push.

    DATA: lv_first TYPE xstring,
          lv_utf   TYPE string,
          lv_data  TYPE string,
          lt_data  TYPE TABLE OF string.


    lv_first = iv_data(4).
    lv_utf = zcl_ags_util=>xstring_to_string_utf8( lv_first ).
    rs_push-length = lcl_length=>decode( lv_utf ).

    lv_first = iv_data(rs_push-length).
    lv_data = zcl_ags_util=>xstring_to_string_utf8( lv_first ).
    lv_data = lv_data+4. " skip length, already decoded

    SPLIT lv_data AT get_null( ) INTO TABLE lt_data.
    ASSERT lines( lt_data ) > 0.

    READ TABLE lt_data INDEX 1 INTO lv_data.              "#EC CI_SUBRC

    rs_push-old  = lv_data.
    rs_push-new  = lv_data+41.
    rs_push-name = lv_data+93. " also skip 'refs/heads/'

  ENDMETHOD.


  METHOD decode_request.

    DATA: lv_line    TYPE string,
          lv_command TYPE string,
          lv_rest    TYPE string,
          lt_lines   TYPE TABLE OF string.


    SPLIT iv_string AT cl_abap_char_utilities=>newline INTO TABLE lt_lines.

    LOOP AT lt_lines INTO lv_line.

      SPLIT lv_line AT space INTO lv_command lv_rest.
      IF lv_command(4) = '0000'.
        RETURN.
      ENDIF.

      lv_command = lv_command+4.

      CASE lv_command.
        WHEN 'want'.
          APPEND lv_rest TO rs_request-want.
        WHEN 'deepen'.
          rs_request-deepen = lv_rest.
        WHEN OTHERS.
* todo, unknown command
          ASSERT 0 = 1.
      ENDCASE.

    ENDLOOP.

    SORT rs_request-want ASCENDING.
    DELETE ADJACENT DUPLICATES FROM rs_request-want.

  ENDMETHOD.


  METHOD get_null.

    DATA: lv_x(4) TYPE x VALUE '00000000',
          lv_z(2) TYPE c.

    FIELD-SYMBOLS <lv_y> TYPE c.


    ASSIGN lv_x TO <lv_y> CASTING.
    lv_z = <lv_y>.
    rv_char = lv_z(1).

  ENDMETHOD.


  METHOD pack.

    CONSTANTS: lc_band1  TYPE x VALUE '01',
               lc_length TYPE i VALUE 8196.

    DATA: lv_response TYPE xstring,
          lo_commit   TYPE REF TO zcl_ags_obj_commit,
          lv_encoded  TYPE zags_hex4,
          lt_objects  TYPE zcl_ags_pack=>ty_objects_tt,
          lv_pack     TYPE xstring,
          ls_request  TYPE ty_request,
          lv_branch   LIKE LINE OF ls_request-want,
          lv_length   TYPE i.


    ls_request = decode_request( mi_server->request->get_cdata( ) ).

    lv_pack = zcl_ags_util=>string_to_xstring_utf8( |NAK\n| ).
    lv_encoded = lcl_length=>encode( xstrlen( lv_pack ) + 4 ).
    CONCATENATE lv_response lv_encoded lv_pack INTO lv_response IN BYTE MODE.

    LOOP AT ls_request-want INTO lv_branch.
      CREATE OBJECT lo_commit
        EXPORTING
          iv_sha1 = lv_branch.

      APPEND LINES OF zcl_ags_pack=>explode(
        ii_object = lo_commit
        iv_deepen = ls_request-deepen ) TO lt_objects.
    ENDLOOP.

* make sure there are no duplicate objects
    SORT lt_objects BY type ASCENDING sha1 ASCENDING.
    DELETE ADJACENT DUPLICATES FROM lt_objects COMPARING type sha1.

    lv_pack = zcl_ags_pack=>encode( lt_objects ).

    WHILE xstrlen( lv_pack ) > 0.
      IF xstrlen( lv_pack ) >= lc_length.
        lv_length = lc_length.
      ELSE.
        lv_length = xstrlen( lv_pack ).
      ENDIF.

* make sure to include the length encoding itself and band identifier in the length
      lv_encoded = lcl_length=>encode( lv_length + 5 ).

      CONCATENATE lv_response lv_encoded lc_band1 lv_pack(lv_length)
        INTO lv_response IN BYTE MODE.

      lv_pack = lv_pack+lv_length.
    ENDWHILE.

    lv_pack = zcl_ags_util=>string_to_xstring_utf8( '0000' ).
    CONCATENATE lv_response lv_pack INTO lv_response IN BYTE MODE.

    mi_server->response->set_data( lv_response ).

  ENDMETHOD.


  METHOD repo_name.

    DATA: lv_path TYPE string.

    lv_path = mi_server->request->get_header_field( '~path' ).
    FIND REGEX 'sap/zgit/git/(.*)\.git*'
      IN lv_path
      SUBMATCHES rv_name ##no_text.

  ENDMETHOD.


  METHOD unpack.

    CONSTANTS: lc_utf_0000 TYPE x LENGTH 4 VALUE '30303030'.

    DATA: lv_xstring TYPE xstring,
          lt_objects TYPE zcl_ags_pack=>ty_objects_tt,
          lo_repo    TYPE REF TO zcl_ags_repo,
          lo_branch  TYPE REF TO zcl_ags_branch,
          ls_push    TYPE ty_push.


    ls_push = decode_push( mi_server->request->get_data( ) ).

    lv_xstring = mi_server->request->get_data( ).
    lv_xstring = lv_xstring+ls_push-length.
    ASSERT lv_xstring(4) = lc_utf_0000.
    lv_xstring = lv_xstring+4.

    lt_objects = zcl_ags_pack=>decode( lv_xstring ).

    CREATE OBJECT lo_repo
      EXPORTING
        iv_name = repo_name( ).

    IF ls_push-old CO '0'.
* create branch
      zcl_ags_branch=>create(
        io_repo   = lo_repo
        iv_name   = ls_push-name
        iv_commit = ls_push-new ).
    ELSEIF ls_push-new CO '0'.
* delete branch
      lo_branch = lo_repo->get_branch( ls_push-name ).
      ASSERT lo_branch->get_data( )-sha1 = ls_push-old.
      lo_branch->delete( ).
    ELSE.
* update branch

      READ TABLE lt_objects WITH KEY sha1 = ls_push-new TRANSPORTING NO FIELDS.
* new commit should exist in objects
      ASSERT sy-subrc = 0.

      lo_branch = lo_repo->get_branch( ls_push-name ).

      ASSERT lo_branch->get_data( )-sha1 = ls_push-old.

      zcl_ags_pack=>save( lt_objects ).

      lo_branch->update_sha1( ls_push-new ).
    ENDIF.

    unpack_ok( ).

  ENDMETHOD.


  METHOD unpack_ok.

* todo, this is all wrong(but will work in most cases):
    mi_server->response->set_cdata( '000eunpack ok#0019ok refs/heads/master#00000000' ).

  ENDMETHOD.


  METHOD zif_ags_service~run.

    DATA: lv_path  TYPE string,
          lv_xdata TYPE string.


    mi_server = ii_server.

    lv_path = mi_server->request->get_header_field( '~path_info' ).
    lv_xdata = mi_server->request->get_data( ).

    IF lv_xdata IS INITIAL.
      branch_list( ).
    ELSEIF lv_path CP '*git-upload-pack*'.
      pack( ).
    ELSEIF lv_path CP '*git-receive-pack*'.
      unpack( ).
    ELSE.
      RAISE EXCEPTION TYPE zcx_ags_error
        EXPORTING
          textid = zcx_ags_error=>m008.
    ENDIF.

  ENDMETHOD.
ENDCLASS.
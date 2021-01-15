CLASS zcl_ags_service_git DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES zif_ags_service .
  PROTECTED SECTION.
  PRIVATE SECTION.

    TYPES:
      BEGIN OF ty_push,
        old    TYPE zags_sha1,
        new    TYPE zags_sha1,
        name   TYPE zags_branch_name,
        length TYPE i,
      END OF ty_push .
    TYPES:
      BEGIN OF ty_request,
        want         TYPE STANDARD TABLE OF zags_sha1 WITH DEFAULT KEY,
        have         TYPE STANDARD TABLE OF zags_sha1 WITH DEFAULT KEY,
        capabilities TYPE STANDARD TABLE OF string WITH DEFAULT KEY,
        deepen       TYPE i,
      END OF ty_request .
    TYPES:
      ty_ack_mode TYPE c LENGTH 1 .

    CONSTANTS:
      BEGIN OF c_ack_mode,
        normal        TYPE ty_ack_mode VALUE '1',
        detailed      TYPE ty_ack_mode VALUE '2',
        not_specified TYPE ty_ack_mode VALUE '3',
      END OF c_ack_mode .
    DATA mi_server TYPE REF TO if_http_server .

    METHODS find_ack_mode
      IMPORTING
        !is_request    TYPE ty_request
      RETURNING
        VALUE(rv_mode) TYPE ty_ack_mode .
    METHODS negotiate_packfile
      IMPORTING
        !io_response TYPE REF TO zcl_ags_xstream
        !is_request  TYPE ty_request .
    METHODS branch_list
      IMPORTING
        !iv_service TYPE string
      RAISING
        zcx_ags_error .
    METHODS decode_push
      IMPORTING
        !iv_data       TYPE xstring
      RETURNING
        VALUE(rs_push) TYPE ty_push
      RAISING
        zcx_ags_error .
    METHODS decode_request
      IMPORTING
        !iv_string        TYPE string
      RETURNING
        VALUE(rs_request) TYPE ty_request .
    METHODS get_null
      RETURNING
        VALUE(rv_char) TYPE char1 .
    METHODS pack
      RAISING
        zcx_ags_error .
    METHODS repo_name
      RETURNING
        VALUE(rv_name) TYPE zags_repos-name .
    METHODS unpack
      RAISING
        zcx_ags_error .
    METHODS unpack_ok
      IMPORTING
        !iv_branch_name   TYPE zags_branch_name
      RETURNING
        VALUE(rv_xstring) TYPE xstring .
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
          lt_branches     TYPE zcl_ags_repo=>ty_branches_tt,
          lv_raw          TYPE xstring.

    FIELD-SYMBOLS: <lo_branch> LIKE LINE OF lt_branches.


    IF iv_service IS INITIAL.
      RAISE EXCEPTION TYPE zcx_ags_error
        EXPORTING
          textid = zcx_ags_error=>m003.
    ENDIF.

    _capability 'multi_ack'.
*    _capability 'thin-pack'.
    _capability 'no-thin'.
    _capability 'side-band'.
    _capability 'side-band-64k'.
*    _capability 'ofs-delta'.
    _capability 'shallow'.
    _capability 'no-progress'.
    _capability 'include-tag'.
    _capability 'report-status'.
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

    lv_content = |# service={ iv_service }| ##NO_TEXT.
    lv_length = zcl_ags_length=>encode( strlen( lv_content ) + 5 ).
    lv_utf = to_lower( zcl_ags_util=>xstring_to_string_utf8( lv_length ) ).
    lv_tmp = lv_utf && lv_content.
    APPEND lv_tmp TO lt_reply.

    lv_content = |{ lv_head } HEAD{ get_null( ) }{ lv_reply }|.
    lv_length = zcl_ags_length=>encode( strlen( lv_content ) + 5 ).
    lv_utf = to_lower( zcl_ags_util=>xstring_to_string_utf8( lv_length ) ).

    lv_tmp = '0000' && lv_utf && lv_content.
    APPEND lv_tmp TO lt_reply.

    lt_branches = lo_repo->list_branches( ).
    LOOP AT lt_branches ASSIGNING <lo_branch>.
      lv_content = <lo_branch>->get_data( )-sha1
        && | |
        && <lo_branch>->get_data( )-name ##no_text.

      lv_length = zcl_ags_length=>encode( strlen( lv_content ) + 5 ).
      lv_utf = to_lower( zcl_ags_util=>xstring_to_string_utf8( lv_length ) ).

      lv_tmp = lv_utf && lv_content.
      APPEND lv_tmp TO lt_reply.
    ENDLOOP.

    APPEND '0000' TO lt_reply.

    CONCATENATE LINES OF lt_reply INTO lv_reply
      SEPARATED BY cl_abap_char_utilities=>newline.

    mi_server->response->set_header_field(
      name  = 'Server'
      value = 'abapGitServer' ) ##no_text.
    mi_server->response->set_header_field(
      name  = 'Cache-Control'
      value = 'no-cache' ) ##no_text.
    mi_server->response->set_content_type( |application/x-{ iv_service }-advertisement| ) ##no_text.

* must be sent as raw, using data will change the content-type of the response
    lv_raw = zcl_ags_util=>string_to_xstring_utf8( lv_reply ).
    mi_server->response->set_data( lv_raw ).

  ENDMETHOD.


  METHOD decode_push.

    DATA: lv_first TYPE xstring,
          lv_utf   TYPE string,
          lv_data  TYPE string,
          lt_data  TYPE TABLE OF string.


    lv_first = iv_data(4).
    lv_utf = zcl_ags_util=>xstring_to_string_utf8( lv_first ).
    rs_push-length = zcl_ags_length=>decode( lv_utf ).

    lv_first = iv_data(rs_push-length).
    lv_data = zcl_ags_util=>xstring_to_string_utf8( lv_first ).
    lv_data = lv_data+4. " skip length, already decoded

    SPLIT lv_data AT get_null( ) INTO TABLE lt_data.
    ASSERT lines( lt_data ) > 0.

    READ TABLE lt_data INDEX 1 INTO lv_data.              "#EC CI_SUBRC

    rs_push-old  = lv_data.
    rs_push-new  = lv_data+41.
    rs_push-name = lv_data+82.

  ENDMETHOD.


  METHOD decode_request.
* todo: add unit tests

    DATA: lv_line         TYPE string,
          lv_command      TYPE string,
          lv_rest         TYPE string,
          lv_capabilities TYPE string,
          lt_lines        TYPE TABLE OF string.


    SPLIT iv_string AT cl_abap_char_utilities=>newline INTO TABLE lt_lines.

    LOOP AT lt_lines INTO lv_line.

      SPLIT lv_line AT space INTO lv_command lv_rest.

      IF lv_command(4) = '0000'.
        lv_command = lv_command+4.
      ENDIF.
      IF strlen( lv_command ) >= 4.
        lv_command = lv_command+4.
      ENDIF.

      CASE lv_command.
        WHEN 'want'.
          IF lines( rs_request-want ) = 0.
            SPLIT lv_rest AT space INTO lv_rest lv_capabilities.
            SPLIT lv_capabilities AT space INTO TABLE rs_request-capabilities.
          ENDIF.
          APPEND lv_rest TO rs_request-want.
        WHEN 'have'.
          APPEND lv_rest TO rs_request-have.
        WHEN 'deepen'.
          rs_request-deepen = lv_rest.
        WHEN '' OR 'done'.
          RETURN.
        WHEN OTHERS.
* todo, unknown command
          ASSERT 0 = 1.
      ENDCASE.

    ENDLOOP.

    SORT rs_request-want ASCENDING.
    DELETE ADJACENT DUPLICATES FROM rs_request-want.

  ENDMETHOD.


  METHOD find_ack_mode.

    READ TABLE is_request-capabilities WITH KEY table_line = 'multi_ack'
      TRANSPORTING NO FIELDS.
    IF sy-subrc = 0.
      rv_mode = c_ack_mode-normal.
    ENDIF.

    READ TABLE is_request-capabilities WITH KEY table_line = 'multi_ack_detailed'
      TRANSPORTING NO FIELDS.
    IF sy-subrc = 0.
      ASSERT rv_mode IS INITIAL.
      rv_mode = c_ack_mode-detailed.
    ENDIF.

    IF rv_mode IS INITIAL.
      rv_mode = c_ack_mode-not_specified.
    ENDIF.

  ENDMETHOD.


  METHOD get_null.

    DATA: lv_x(4) TYPE x VALUE '00000000',
          lv_z(2) TYPE c.

    FIELD-SYMBOLS <lv_y> TYPE c.


    ASSIGN lv_x TO <lv_y> CASTING.
    lv_z = <lv_y>.
    rv_char = lv_z(1).

  ENDMETHOD.


  METHOD negotiate_packfile.
* see https://github.com/git/git/blob/master/Documentation/technical/pack-protocol.txt#L298

    DATA: lv_ack_mode TYPE ty_ack_mode,
          lv_no_done  TYPE abap_bool,
          lv_subrc    LIKE sy-subrc,
          lv_data     TYPE string,
          lv_sha1     LIKE LINE OF is_request-have,
          lo_server   TYPE REF TO cl_http_server.


    lv_ack_mode = find_ack_mode( is_request ).

    READ TABLE is_request-capabilities WITH KEY table_line = 'no-done' TRANSPORTING NO FIELDS.
    lv_no_done = xsdbool( sy-subrc = 0 ).

    IF lv_ack_mode = c_ack_mode-detailed AND lines( is_request-have ) = 0.
      lv_ack_mode = c_ack_mode-normal.
    ENDIF.

    CASE lv_ack_mode.
      WHEN c_ack_mode-normal.
        io_response->append_length( zcl_ags_util=>string_to_xstring_utf8( |NAK\n| ) ).
      WHEN c_ack_mode-detailed.

        IF lv_no_done = abap_false.
          LOOP AT is_request-have INTO lv_sha1.
* todo, "common" or "ready"?
            io_response->append_length( zcl_ags_util=>string_to_xstring_utf8( |ACK { lv_sha1 } common\n| ) ).
          ENDLOOP.

          io_response->append_length( zcl_ags_util=>string_to_xstring_utf8( |NAK\n| ) ).

          mi_server->response->set_data( io_response->get( ) ).

          lo_server ?= mi_server.
          lo_server->send_response( ).

          lv_subrc = lo_server->receive_request( ).
          lv_data = mi_server->request->get_cdata( ).
* todo, this works, but everything is sent, just like a clone operation

          io_response->clear( ).
        ENDIF.

        io_response->append_length( zcl_ags_util=>string_to_xstring_utf8( |ACK { is_request-want[ 1 ] }\n| ) ).

      WHEN c_ack_mode-not_specified.
* todo, not implemented
        ASSERT 0 = 1.
      WHEN OTHERS.
        ASSERT 0 = 1.
    ENDCASE.

  ENDMETHOD.


  METHOD pack.

    CONSTANTS: lc_length TYPE i VALUE 8196.

    DATA: lo_response TYPE REF TO zcl_ags_xstream,
          lo_commit   TYPE REF TO zcl_ags_obj_commit,
          lt_objects  TYPE zif_abapgit_definitions=>ty_objects_tt,
          lv_pack     TYPE xstring,
          lv_tmp      TYPE xstring,
          lv_repo     TYPE zags_repos-repo,
          ls_request  TYPE ty_request,
          lv_branch   LIKE LINE OF ls_request-want,
          lv_length   TYPE i.


    ls_request = decode_request( mi_server->request->get_cdata( ) ).

    lv_repo = zcl_ags_repo=>get_instance( repo_name( ) )->get_data( )-repo.

    CREATE OBJECT lo_response.

    negotiate_packfile( io_response = lo_response
                        is_request  = ls_request ).

    LOOP AT ls_request-want INTO lv_branch.
      lo_commit = zcl_ags_obj_commit=>load(
          iv_repo = lv_repo
          iv_sha1 = lv_branch ).

      APPEND LINES OF zcl_ags_pack=>explode(
        iv_repo   = lv_repo
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

      lv_tmp = lv_pack(lv_length).
      lo_response->append_band01( lv_tmp ).

      lv_pack = lv_pack+lv_length.
    ENDWHILE.

    lo_response->append( zcl_ags_util=>string_to_xstring_utf8( '0000' ) ).

    mi_server->response->set_data( lo_response->get( ) ).

    mi_server->response->set_header_field(
      name  = if_http_header_fields=>content_type
      value = 'application/x-git-upload-pack-result' ) ##NO_TEXT.

  ENDMETHOD.


  METHOD repo_name.

    DATA: lv_path TYPE string.

    lv_path = mi_server->request->get_header_field( '~path' ).
    FIND REGEX 'sap/zabapgitserver/git/(.*)\.git*'
      IN lv_path
      SUBMATCHES rv_name ##no_text.

  ENDMETHOD.


  METHOD unpack.

    CONSTANTS: lc_utf_0000 TYPE x LENGTH 4 VALUE '30303030'.

    DATA: lv_xstring TYPE xstring,
          lt_objects TYPE zif_abapgit_definitions=>ty_objects_tt,
          lo_repo    TYPE REF TO zcl_ags_repo,
          lo_branch  TYPE REF TO zcl_ags_branch,
          ls_push    TYPE ty_push.


    ls_push = decode_push( mi_server->request->get_data( ) ).

    lv_xstring = mi_server->request->get_data( ).
    lv_xstring = lv_xstring+ls_push-length.
    ASSERT lv_xstring(4) = lc_utf_0000.
    lv_xstring = lv_xstring+4.

    lt_objects = zcl_ags_pack=>decode( lv_xstring ).

    lo_repo = zcl_ags_repo=>get_instance( repo_name( ) ).

    IF ls_push-old CO '0'.
* create branch
      lo_repo->create_branch(
        iv_name   = ls_push-name
        iv_commit = ls_push-new ).
    ELSEIF ls_push-new CO '0'.
* delete branch
      lo_branch = lo_repo->get_branch( ls_push-name ).
      ASSERT lo_branch->get_data( )-sha1 = ls_push-old.
      lo_branch->delete( ).
    ELSE.
* update branch
      lo_repo->get_branch( ls_push-name )->push(
        iv_new     = ls_push-new
        iv_old     = ls_push-old
        it_objects = lt_objects ).
    ENDIF.

* method set_data has to be used, or SAP will modify the "Content-Type" header
    mi_server->response->set_data( unpack_ok( ls_push-name ) ).

    mi_server->response->set_header_field(
      name  = if_http_header_fields=>content_type
      value = 'application/x-git-receive-pack-result' ) ##NO_TEXT.

  ENDMETHOD.


  METHOD unpack_ok.

    DATA: lo_response TYPE REF TO zcl_ags_xstream.

    CREATE OBJECT lo_response.

* send report-status when capability is enabled,
    lo_response->append_band01( zcl_ags_xstream=>create(
      )->append_length( zcl_ags_util=>string_to_xstring_utf8( |unpack ok\n| ) )->get( ) ).

    lo_response->append_band01( zcl_ags_xstream=>create(
      )->append_length( zcl_ags_util=>string_to_xstring_utf8( |ok { iv_branch_name }\n| ) )->get( ) ).

    lo_response->append_band01( zcl_ags_util=>string_to_xstring_utf8( '0000' ) ).

    lo_response->append( zcl_ags_util=>string_to_xstring_utf8( '0000' ) ).

    rv_xstring = lo_response->get( ).

  ENDMETHOD.


  METHOD zif_ags_service~run.

    DATA: lv_path  TYPE string,
          lv_xdata TYPE string.


    mi_server = ii_server.

    lv_path = mi_server->request->get_header_field( '~path_info' ).
    lv_xdata = mi_server->request->get_data( ).

    IF lv_xdata IS INITIAL.
      branch_list( mi_server->request->get_form_field( 'service' ) ).
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

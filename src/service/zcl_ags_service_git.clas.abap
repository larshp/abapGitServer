CLASS zcl_ags_service_git DEFINITION
  PUBLIC
  CREATE PUBLIC.

  PUBLIC SECTION.

    METHODS run
      IMPORTING
        !ii_server TYPE REF TO if_http_server
      RAISING
        zcx_ags_error.
  PROTECTED SECTION.
PRIVATE SECTION.

  TYPES:
    ty_hex4 TYPE x LENGTH 4.
  TYPES:
    BEGIN OF ty_push,
      branch      TYPE zags_sha1,
      commit      TYPE zags_sha1,
      branch_name TYPE zags_branch_name,
      length      TYPE i,
    END OF ty_push .

  METHODS decode_length
    IMPORTING
      !iv_data         TYPE string
    RETURNING
      VALUE(rv_length) TYPE i .
  METHODS decode_push
    IMPORTING
      !iv_data       TYPE string
    RETURNING
      VALUE(rs_push) TYPE ty_push
    RAISING
      zcx_ags_error .
  METHODS encode_length
    IMPORTING
      !iv_length    TYPE i
    RETURNING
      VALUE(rv_hex) TYPE ty_hex4 .
  METHODS unpack
    IMPORTING
      !ii_server TYPE REF TO if_http_server
    RAISING
      zcx_ags_error .
  METHODS pack
    IMPORTING
      !ii_server TYPE REF TO if_http_server
    RAISING
      zcx_ags_error .
  METHODS decode_want
    IMPORTING
      !iv_string     TYPE string
    RETURNING
      VALUE(rv_sha1) TYPE zags_sha1 .
  METHODS repo_name
    IMPORTING
      !ii_server     TYPE REF TO if_http_server
    RETURNING
      VALUE(rv_name) TYPE zags_repos-name .
  METHODS get_null
    RETURNING
      VALUE(rv_char) TYPE char1 .
  METHODS branch_list
    IMPORTING
      !ii_server TYPE REF TO if_http_server
    RAISING
      zcx_ags_error .
ENDCLASS.



CLASS ZCL_AGS_SERVICE_GIT IMPLEMENTATION.


  METHOD branch_list.

    DEFINE _capability.
      APPEND &1 TO lt_capabilities ##NO_TEXT.
    END-OF-DEFINITION.

    DATA: lv_reply        TYPE string,
          lt_reply        TYPE TABLE OF string,
          lt_capabilities TYPE TABLE OF string.


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

    DATA(lv_name) = repo_name( ii_server ).
    DATA(lo_repo) = NEW zcl_ags_repo( lv_name ).
    DATA(lt_branches) = lo_repo->list_branches( ).
    DATA(lv_sha1) = lt_branches[ 1 ]->get_data( )-sha1.

* todo, list all branches
    APPEND '001e# service=git-upload-pack' TO lt_reply ##NO_TEXT.
    APPEND '000000e8' && lv_sha1 && ' HEAD' && get_null( ) && lv_reply TO lt_reply.
    APPEND '003f' && lv_sha1 && ' refs/heads/master' TO lt_reply ##NO_TEXT.
    APPEND '0000' TO lt_reply.

    CONCATENATE LINES OF lt_reply INTO lv_reply
      SEPARATED BY cl_abap_char_utilities=>newline.

    ii_server->response->set_cdata( lv_reply ).

  ENDMETHOD.


  METHOD decode_length.

    DATA: lv_x TYPE x LENGTH 1.


    ASSERT strlen( iv_data ) >= 4.

* todo, extend implementation
    ASSERT iv_data(2) = '00'.

    lv_x = iv_data+2.

    rv_length = lv_x.

  ENDMETHOD.


  METHOD decode_push.

    DATA: lt_data TYPE TABLE OF string.

    rs_push-length = decode_length( iv_data ).

    DATA(lv_data) = iv_data(rs_push-length).
    lv_data = lv_data+4. " skip length, already decoded

    SPLIT lv_data AT get_null( ) INTO TABLE lt_data.
    ASSERT lines( lt_data ) > 0.

    lv_data = lt_data[ 1 ].

    rs_push-branch      = lv_data.
    rs_push-commit      = lv_data+41.
    rs_push-branch_name = lv_data+82.

  ENDMETHOD.


  METHOD decode_want.

* todo, proper decoding
    rv_sha1 = iv_string+9.

  ENDMETHOD.


  METHOD encode_length.

    DATA: lv_char TYPE string,
          lv_x    TYPE x LENGTH 2.

    lv_x = iv_length.

    lv_char = lv_x.

    DATA(lv_xstring) = zcl_ags_util=>string_to_xstring_utf8( lv_char ).

    rv_hex = lv_xstring.

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

    CONSTANTS: lc_band1 TYPE x VALUE '01'.

    DATA: lv_response TYPE xstring,
          lv_length   TYPE i.


    DATA(lv_branch) = decode_want( ii_server->request->get_cdata( ) ).

    DATA(lo_commit) = NEW zcl_ags_obj_commit( lv_branch ).

    DATA(lv_pack) = zcl_ags_pack=>encode( zcl_ags_pack=>explode( lo_commit ) ).

    WHILE xstrlen( lv_pack ) > 0.
      IF xstrlen( lv_pack ) >= 8196.
        lv_length = 8196.
      ELSE.
        lv_length = xstrlen( lv_pack ).
      ENDIF.

* make sure to include the length encoding itself and band identifier in the length
      DATA(lv_encoded) = encode_length( lv_length + 5 ).

      CONCATENATE lv_response lv_encoded lc_band1 lv_pack(lv_length)
        INTO lv_response IN BYTE MODE.

      lv_pack = lv_pack+lv_length.
    ENDWHILE.

    ii_server->response->set_data( lv_response ).

  ENDMETHOD.


  METHOD repo_name.

    DATA(lv_path) = ii_server->request->get_header_field( '~path' ).
    FIND REGEX 'sap/zgit/git/(.*)\.git*'
      IN lv_path
      SUBMATCHES rv_name ##NO_TEXT.

  ENDMETHOD.


  METHOD run.

    DATA(lv_path) = ii_server->request->get_header_field( '~path_info' ).

    IF ii_server->request->get_cdata( ) IS INITIAL.
      branch_list( ii_server ).
    ELSEIF lv_path CP '*git-upload-pack*'.
      pack( ii_server ).
    ELSEIF lv_path CP '*git-receive-pack*'.
      unpack( ii_server ).
    ELSE.
      RAISE EXCEPTION TYPE zcx_ags_error
        EXPORTING
          textid = zcx_ags_error=>m008.
    ENDIF.

  ENDMETHOD.


  METHOD unpack.

    DATA(ls_push) = decode_push( ii_server->request->get_cdata( ) ).

    DATA(lv_xstring) = ii_server->request->get_data( ).
    lv_xstring = lv_xstring+ls_push-length.
    ASSERT lv_xstring(4) = '30303030'. " in utf8 = '0000'
    lv_xstring = lv_xstring+4.

    DATA(lt_objects) = zcl_ags_pack=>decode( lv_xstring ).

    READ TABLE lt_objects WITH KEY sha1 = ls_push-commit TRANSPORTING NO FIELDS.
* new commit should exist in objects
    ASSERT sy-subrc = 0.

    DATA(lo_repo) = NEW zcl_ags_repo( repo_name( ii_server ) ).

* todo, new branches?
    DATA(lo_branch) = lo_repo->get_branch( ls_push-branch_name ).

    ASSERT lo_branch->get_data( )-sha1 = ls_push-branch.

* todo, update sha1 of branch to ls_push-commit

* todo

  ENDMETHOD.
ENDCLASS.
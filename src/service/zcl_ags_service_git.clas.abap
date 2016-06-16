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

    METHODS encode_length
      IMPORTING
        !iv_length    TYPE i
      RETURNING
        VALUE(rv_hex) TYPE ty_hex4.
    METHODS pack
      IMPORTING
        !iv_branch TYPE zags_sha1
        !ii_server TYPE REF TO if_http_server
      RAISING
        zcx_ags_error.
    METHODS decode_want
      IMPORTING
        !iv_string     TYPE string
      RETURNING
        VALUE(rv_sha1) TYPE zags_sha1.
    METHODS repo_name
      IMPORTING
        !ii_server     TYPE REF TO if_http_server
      RETURNING
        VALUE(rv_name) TYPE zags_repos-name.
    METHODS get_null
      RETURNING
        VALUE(rv_char) TYPE char1.
    METHODS branch_list
      IMPORTING
        !ii_server TYPE REF TO if_http_server
      RAISING
        zcx_ags_error.
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

*    CALL METHOD ('\PROGRAM=ZABAPGIT\CLASS=LCL_CONVERT')=>int_to_xstring
*      EXPORTING
*        iv_i       = iv_length
*        iv_length  = 4
*      RECEIVING
*        rv_xstring = rv_hex.

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


    DATA(lo_commit) = NEW zcl_ags_obj_commit( iv_branch ).

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
    FIND REGEX 'sap/zgit/git/(.*).git*'
      IN lv_path
      SUBMATCHES rv_name ##NO_TEXT.

  ENDMETHOD.


  METHOD run.

    DATA(lv_data) = ii_server->request->get_cdata( ).

    IF lv_data IS INITIAL.
      branch_list( ii_server ).
    ELSE.
      pack( iv_branch = decode_want( lv_data )
            ii_server = ii_server ).
    ENDIF.

  ENDMETHOD.
ENDCLASS.
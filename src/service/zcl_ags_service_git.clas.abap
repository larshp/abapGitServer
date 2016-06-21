class ZCL_AGS_SERVICE_GIT definition
  public
  create public .

public section.

  interfaces ZIF_AGS_SERVICE .

  methods CONSTRUCTOR
    importing
      !II_SERVER type ref to IF_HTTP_SERVER .
  PROTECTED SECTION.
private section.

  types:
    BEGIN OF ty_push,
      branch      TYPE zags_sha1,
      commit      TYPE zags_sha1,
      branch_name TYPE zags_branch_name,
      length      TYPE i,
    END OF ty_push .

  data MI_SERVER type ref to IF_HTTP_SERVER .

  methods BRANCH_LIST
    raising
      ZCX_AGS_ERROR .
  methods DECODE_PUSH
    importing
      !IV_DATA type STRING
    returning
      value(RS_PUSH) type TY_PUSH
    raising
      ZCX_AGS_ERROR .
  methods DECODE_WANT
    importing
      !IV_STRING type STRING
    returning
      value(RV_SHA1) type ZAGS_SHA1 .
  methods GET_NULL
    returning
      value(RV_CHAR) type CHAR1 .
  methods PACK
    raising
      ZCX_AGS_ERROR .
  methods REPO_NAME
    returning
      value(RV_NAME) type ZAGS_REPOS-NAME .
  methods UNPACK
    raising
      ZCX_AGS_ERROR .
  methods UNPACK_OK .
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

    DATA(lv_name) = repo_name( ).
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

    mi_server->response->set_cdata( lv_reply ).

  ENDMETHOD.


  METHOD constructor.

    mi_server = ii_server.

  ENDMETHOD.


  METHOD decode_push.

    DATA: lt_data TYPE TABLE OF string.

    rs_push-length = lcl_length=>decode( iv_data ).

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


    DATA(lv_branch) = decode_want( mi_server->request->get_cdata( ) ).

    DATA(lo_commit) = NEW zcl_ags_obj_commit( lv_branch ).

    DATA(lv_pack) = zcl_ags_pack=>encode( zcl_ags_pack=>explode( lo_commit ) ).

    WHILE xstrlen( lv_pack ) > 0.
      IF xstrlen( lv_pack ) >= 8196.
        lv_length = 8196.
      ELSE.
        lv_length = xstrlen( lv_pack ).
      ENDIF.

* make sure to include the length encoding itself and band identifier in the length
      DATA(lv_encoded) = lcl_length=>encode( lv_length + 5 ).

      CONCATENATE lv_response lv_encoded lc_band1 lv_pack(lv_length)
        INTO lv_response IN BYTE MODE.

      lv_pack = lv_pack+lv_length.
    ENDWHILE.

    mi_server->response->set_data( lv_response ).

  ENDMETHOD.


  METHOD repo_name.

    DATA(lv_path) = mi_server->request->get_header_field( '~path' ).
    FIND REGEX 'sap/zgit/git/(.*)\.git*'
      IN lv_path
      SUBMATCHES rv_name ##NO_TEXT.

  ENDMETHOD.


  METHOD unpack.

    CONSTANTS: lc_utf_0000 TYPE x LENGTH 4 VALUE '30303030'.


    DATA(ls_push) = decode_push( mi_server->request->get_cdata( ) ).

    DATA(lv_xstring) = mi_server->request->get_data( ).
    lv_xstring = lv_xstring+ls_push-length.
    ASSERT lv_xstring(4) = lc_utf_0000.
    lv_xstring = lv_xstring+4.

    DATA(lt_objects) = zcl_ags_pack=>decode( lv_xstring ).

    READ TABLE lt_objects WITH KEY sha1 = ls_push-commit TRANSPORTING NO FIELDS.
* new commit should exist in objects
    ASSERT sy-subrc = 0.

    DATA(lo_repo) = NEW zcl_ags_repo( repo_name( ) ).

* todo, new branches?
    DATA(lo_branch) = lo_repo->get_branch( ls_push-branch_name ).

    ASSERT lo_branch->get_data( )-sha1 = ls_push-branch.

    zcl_ags_pack=>save( lt_objects ).

    lo_branch->update_sha1( ls_push-commit ).

    unpack_ok( ).

  ENDMETHOD.


  METHOD unpack_ok.

* todo, this is all wrong(but will work in most cases):
    mi_server->response->set_cdata( '000eunpack ok#0019ok refs/heads/master#00000000' ).

  ENDMETHOD.


  METHOD zif_ags_service~run.

    DATA(lv_path) = mi_server->request->get_header_field( '~path_info' ).

    IF mi_server->request->get_cdata( ) IS INITIAL.
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
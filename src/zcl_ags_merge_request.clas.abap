CLASS zcl_ags_merge_request DEFINITION
  PUBLIC
  FINAL
  CREATE PRIVATE .

  PUBLIC SECTION.
    TYPES: BEGIN OF ty_diff,
      path     TYPE string,
      filename TYPE string,
      type     TYPE string,
      code_diff TYPE zif_abapgit_definitions=>ty_diffs_tt,
    END OF ty_diff.
    TYPES: ty_diff_tt TYPE STANDARD TABLE OF ty_diff.

    CLASS-METHODS create
      IMPORTING
        iv_repository TYPE zif_abapgit_persistence=>ty_repo
        iv_source_branch TYPE string
      RETURNING VALUE(rv_result) TYPE REF TO zcl_ags_merge_request
      RAISING
        zcx_abapgit_exception.

    METHODS get_diff
      EXPORTING
        et_diff TYPE ty_diff_tt.

  PROTECTED SECTION.
  PRIVATE SECTION.
    DATA mo_diff_branch TYPE REF TO zcl_abapgit_diff_branch.

    METHODS constructor
      IMPORTING
        io_diff_branch TYPE REF TO zcl_abapgit_diff_branch.

ENDCLASS.



CLASS ZCL_AGS_MERGE_REQUEST IMPLEMENTATION.


  METHOD constructor.
    mo_diff_branch = io_diff_branch.
  ENDMETHOD.


  METHOD create.

    DATA(lo_online_repo) = NEW zcl_abapgit_repo_online( iv_repository ).
    rv_result = NEW zcl_ags_merge_request( zcl_abapgit_diff_branch=>create(
      io_repo = lo_online_repo iv_source_branch = iv_source_branch ) ).

  ENDMETHOD.


  METHOD get_diff.

    CLEAR et_diff.
    LOOP AT mo_diff_branch->mt_diff_files REFERENCE INTO DATA(lr_diff_file).
      INSERT VALUE #( path = lr_diff_file->*-path filename = lr_diff_file->*-filename
        type = lr_diff_file->*-type
        code_diff = COND zif_abapgit_definitions=>ty_diffs_tt(
          WHEN lr_diff_file->*-o_diff IS BOUND THEN lr_diff_file->*-o_diff->get( ) ) )
        INTO TABLE et_diff.
    ENDLOOP.

  ENDMETHOD.
ENDCLASS.

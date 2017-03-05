CLASS zcl_ags_file_operations DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS add
      IMPORTING
        !iv_filename       TYPE string
        !iv_path           TYPE string
        !iv_file_contents  TYPE string
        !iv_commit_message TYPE string .
    METHODS constructor
      IMPORTING
        !io_branch TYPE REF TO zcl_ags_branch .
    METHODS delete
      IMPORTING
        !iv_filename       TYPE string
        !iv_path           TYPE string
        !iv_commit_message TYPE string .
    METHODS modify
      IMPORTING
        !iv_filename       TYPE string
        !iv_path           TYPE string
        !iv_file_contents  TYPE string
        !iv_commit_message TYPE string .
  PROTECTED SECTION.

    DATA mo_branch TYPE REF TO zcl_ags_branch .
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_AGS_FILE_OPERATIONS IMPLEMENTATION.


  METHOD add.
  ENDMETHOD.


  METHOD constructor.
  ENDMETHOD.


  METHOD delete.
  ENDMETHOD.


  METHOD modify.
  ENDMETHOD.
ENDCLASS.

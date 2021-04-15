CLASS zcx_ags_merge_req_exc DEFINITION
  PUBLIC
  INHERITING FROM zcx_ags_error
  CREATE PUBLIC .

  PUBLIC SECTION.

    CONSTANTS: BEGIN OF duplicate,
      msgid TYPE symsgid VALUE 'ZABAPGITSERVER',
      msgno TYPE symsgno VALUE '015',
      attr1 TYPE scx_attrname VALUE 'ID',
      attr2 TYPE scx_attrname VALUE '',
      attr3 TYPE scx_attrname VALUE '',
      attr4 TYPE scx_attrname VALUE '',
    END OF duplicate.

    METHODS constructor
      IMPORTING
        !textid    LIKE if_t100_message=>t100key OPTIONAL
        !previous  LIKE previous OPTIONAL
        !sha1      TYPE zags_sha1 OPTIONAL
        !string    TYPE string OPTIONAL
        !repo_name TYPE zags_repo_name OPTIONAL
        !id        TYPE zags_merge_request_id OPTIONAL .
protected section.
private section.
ENDCLASS.



CLASS ZCX_AGS_MERGE_REQ_EXC IMPLEMENTATION.


  method CONSTRUCTOR.
CALL METHOD SUPER->CONSTRUCTOR
EXPORTING
PREVIOUS = PREVIOUS
SHA1 = SHA1
STRING = STRING
REPO_NAME = REPO_NAME
ID = ID
.
clear me->textid.
if textid is initial.
  IF_T100_MESSAGE~T100KEY = IF_T100_MESSAGE=>DEFAULT_TEXTID.
else.
  IF_T100_MESSAGE~T100KEY = TEXTID.
endif.
  endmethod.
ENDCLASS.

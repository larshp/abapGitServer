class ZCX_AGS_ERROR definition
  public
  inheriting from CX_STATIC_CHECK
  create public .

public section.

  interfaces IF_T100_MESSAGE .

  constants:
    begin of ZCX_AGS_ERROR,
      msgid type symsgid value 'ZABAPGITSERVER',
      msgno type symsgno value '000',
      attr1 type scx_attrname value '',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of ZCX_AGS_ERROR .
  constants:
    begin of M001,
      msgid type symsgid value 'ZABAPGITSERVER',
      msgno type symsgno value '001',
      attr1 type scx_attrname value '',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of M001 .
  constants:
    begin of M002,
      msgid type symsgid value 'ZABAPGITSERVER',
      msgno type symsgno value '002',
      attr1 type scx_attrname value '',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of M002 .
  constants:
    begin of M004,
      msgid type symsgid value 'ZABAPGITSERVER',
      msgno type symsgno value '004',
      attr1 type scx_attrname value '',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of M004 .
  constants:
    begin of M005,
      msgid type symsgid value 'ZABAPGITSERVER',
      msgno type symsgno value '005',
      attr1 type scx_attrname value 'SHA1',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of M005 .
  constants:
    begin of M008,
      msgid type symsgid value 'ZABAPGITSERVER',
      msgno type symsgno value '008',
      attr1 type scx_attrname value '',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of M008 .
  constants:
    begin of M009,
      msgid type symsgid value 'ZABAPGITSERVER',
      msgno type symsgno value '009',
      attr1 type scx_attrname value '',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of M009 .
  constants:
    begin of M011,
      msgid type symsgid value 'ZABAPGITSERVER',
      msgno type symsgno value '011',
      attr1 type scx_attrname value '',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of M011 .
  constants:
    begin of M012,
      msgid type symsgid value 'ZABAPGITSERVER',
      msgno type symsgno value '012',
      attr1 type scx_attrname value '',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of M012 .
  constants:
    begin of M013,
      msgid type symsgid value 'ZABAPGITSERVER',
      msgno type symsgno value '013',
      attr1 type scx_attrname value 'REPO_NAME',
      attr2 type scx_attrname value 'ID',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of M013 .
  constants:
    begin of M003,
      msgid type symsgid value 'ZABAPGITSERVER',
      msgno type symsgno value '003',
      attr1 type scx_attrname value '',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of M003 .
  data SHA1 type ZAGS_SHA1 .
  data STRING type STRING .
  data REPO_NAME type ZAGS_REPO_NAME .
  data ID type ZAGS_MERGE_REQUEST_ID .

  methods CONSTRUCTOR
    importing
      !TEXTID like IF_T100_MESSAGE=>T100KEY optional
      !PREVIOUS like PREVIOUS optional
      !SHA1 type ZAGS_SHA1 optional
      !STRING type STRING optional
      !REPO_NAME type ZAGS_REPO_NAME optional
      !ID type ZAGS_MERGE_REQUEST_ID optional .
protected section.
private section.
ENDCLASS.



CLASS ZCX_AGS_ERROR IMPLEMENTATION.


  method CONSTRUCTOR ##ADT_SUPPRESS_GENERATION.
CALL METHOD SUPER->CONSTRUCTOR
EXPORTING
PREVIOUS = PREVIOUS
.
me->SHA1 = SHA1 .
me->STRING = STRING .
me->REPO_NAME = REPO_NAME .
me->ID = ID .
clear me->textid.
if textid is initial.
  IF_T100_MESSAGE~T100KEY = ZCX_AGS_ERROR .
else.
  IF_T100_MESSAGE~T100KEY = TEXTID.
endif.
  endmethod.
ENDCLASS.

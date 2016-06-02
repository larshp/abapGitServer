interface ZIF_AGS_OBJECT
  public .


  constants C_NEWLINE type ABAP_CHAR1 value CL_ABAP_CHAR_UTILITIES=>NEWLINE ##NO_TEXT.

  methods SERIALIZE
    returning
      value(RV_DATA) type XSTRING .
  methods DESERIALIZE
    importing
      !IV_DATA type XSTRING .
  methods SHA1
    returning
      value(RV_SHA1) type ZAGS_SHA1
    raising
      ZCX_AGS_ERROR .
endinterface.
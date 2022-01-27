interface ZIF_AGS_BACKEND_BLOB
  public .


  interfaces IF_BADI_INTERFACE .

  methods AFTER_SAVE
    importing
      !IO_BLOB type ref to ZCL_AGS_OBJ_BLOB
      !IS_OBJECT type ZAGS_OBJECTS .
endinterface.

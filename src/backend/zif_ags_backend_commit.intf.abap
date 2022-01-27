interface ZIF_AGS_BACKEND_COMMIT
  public .


  interfaces IF_BADI_INTERFACE .

  methods AFTER_SAVE
    importing
      !IO_COMMIT type ref to ZCL_AGS_OBJ_COMMIT
      !IS_OBJECT type ZAGS_OBJECTS .
endinterface.

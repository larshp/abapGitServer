interface ZIF_AGS_BACKEND_TREE
  public .


  interfaces IF_BADI_INTERFACE .

  methods AFTER_SAVE
    importing
      !IO_TREE type ref to ZCL_AGS_OBJ_TREE
      !IS_OBJECT type ZAGS_OBJECTS .
endinterface.

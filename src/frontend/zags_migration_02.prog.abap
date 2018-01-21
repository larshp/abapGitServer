REPORT zags_migration_02.
* Migration, fill ADLER32 in ZAGS_OBJECTS

START-OF-SELECTION.
  PERFORM run.

FORM run.

  DATA: lt_objects  TYPE STANDARD TABLE OF zags_objects,
        lv_adler32  TYPE zags_objects-adler32,
        lo_progress TYPE REF TO zcl_abapgit_progress.

  FIELD-SYMBOLS: <ls_object> LIKE LINE OF lt_objects.


  SELECT * FROM zags_objects INTO TABLE lt_objects WHERE adler32 IS NULL.
  WRITE: / sy-dbcnt.

  CREATE OBJECT lo_progress
    EXPORTING
      iv_total = lines( lt_objects ).

  LOOP AT lt_objects ASSIGNING <ls_object>.
    lo_progress->show( iv_current = sy-tabix
                       iv_text    = 'Adler32' ).

    lv_adler32 = zcl_abapgit_hash=>adler32( <ls_object>-data_raw ).

    UPDATE zags_objects SET adler32 = lv_adler32
      WHERE repo = <ls_object>-repo
      AND sha1 = <ls_object>-sha1.
    ASSERT sy-subrc = 0.
    ASSERT sy-dbcnt = 1.
  ENDLOOP.

ENDFORM.

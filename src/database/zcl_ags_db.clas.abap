class ZCL_AGS_DB definition
  public
  create public .

public section.

  class-methods GET_REPO
    returning
      value(RO_REPO) type ref to ZCL_AGS_DB_REPO .
  class-methods SET_FAKE .
protected section.
private section.

  class-data MV_FAKE type ABAP_BOOL .
  class-data MO_REPO type ref to ZCL_AGS_DB_REPO .
ENDCLASS.



CLASS ZCL_AGS_DB IMPLEMENTATION.


  METHOD GET_REPO.

    IF mo_repo IS INITIAL.
      CREATE OBJECT mo_repo.
    ENDIF.

    ro_repo->set_fake( ).

    ro_repo = mo_repo.

  ENDMETHOD.


  METHOD set_fake.

    CLEAR mo_repo.

    mv_fake = abap_true.

  ENDMETHOD.
ENDCLASS.

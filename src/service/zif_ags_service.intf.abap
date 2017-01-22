INTERFACE zif_ags_service
  PUBLIC.


  METHODS run
    IMPORTING
      !ii_server TYPE REF TO if_http_server
    RAISING
      zcx_ags_error.
ENDINTERFACE.

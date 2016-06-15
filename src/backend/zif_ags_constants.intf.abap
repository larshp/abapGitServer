INTERFACE zif_ags_constants
  PUBLIC.

  CONSTANTS: BEGIN OF c_type,
               blob   TYPE zags_type VALUE 'blob',
               commit TYPE zags_type VALUE 'commit',
               tree   TYPE zags_type VALUE 'tree',
             END OF c_type.

ENDINTERFACE.
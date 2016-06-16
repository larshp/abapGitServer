INTERFACE zif_ags_constants
  PUBLIC.

  CONSTANTS: BEGIN OF c_type,
               blob   TYPE zags_type VALUE 'blob' ##NO_TEXT,
               commit TYPE zags_type VALUE 'commit' ##NO_TEXT,
               tree   TYPE zags_type VALUE 'tree' ##NO_TEXT,
             END OF c_type.

ENDINTERFACE.
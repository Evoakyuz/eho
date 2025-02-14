CLASS lhc_Singleton DEFINITION INHERITING FROM cl_abap_behavior_handler.
  PRIVATE SECTION.

    METHODS get_instance_features FOR INSTANCE FEATURES
      IMPORTING keys REQUEST requested_features FOR Singleton RESULT result.

    METHODS get_global_authorizations FOR GLOBAL AUTHORIZATION
      IMPORTING REQUEST requested_authorizations FOR Singleton RESULT result.

ENDCLASS.

CLASS lhc_Singleton IMPLEMENTATION.

  METHOD get_instance_features.
  ENDMETHOD.

  METHOD get_global_authorizations.
  ENDMETHOD.

ENDCLASS.

CLASS lhc_Bank DEFINITION INHERITING FROM cl_abap_behavior_handler.
  PRIVATE SECTION.

    METHODS get_global_features FOR GLOBAL FEATURES
      IMPORTING REQUEST requested_features FOR Bank RESULT result.
*    METHODS earlynumbering_cba_Comps FOR NUMBERING
*      IMPORTING entities FOR CREATE Bank\_Comps.


ENDCLASS.

CLASS lhc_Bank IMPLEMENTATION.

  METHOD get_global_features.
  ENDMETHOD.



*  METHOD earlynumbering_cba_Comps.
**  mapped-bank = VALUE #( ( %cid = entities[ 1 ]-%target[ 1 ]-%cid
**                           %is_draft = entities[ 1 ]-%is_draft
**                           Bankcode = entities[ 1 ]-Bankcode ) ).
*  ENDMETHOD.

ENDCLASS.

CLASS lhc_Comps DEFINITION INHERITING FROM cl_abap_behavior_handler.
  PRIVATE SECTION.

    METHODS get_global_features FOR GLOBAL FEATURES
      IMPORTING REQUEST requested_features FOR Comp RESULT result.

ENDCLASS.

CLASS lhc_Comps IMPLEMENTATION.

  METHOD get_global_features.
  ENDMETHOD.

ENDCLASS.

CLASS lsc_ZEHO_I_SINGLETON DEFINITION INHERITING FROM cl_abap_behavior_saver.
  PROTECTED SECTION.

    METHODS save_modified REDEFINITION.

    METHODS cleanup_finalize REDEFINITION.

ENDCLASS.

CLASS lsc_ZEHO_I_SINGLETON IMPLEMENTATION.

  METHOD save_modified.
  ENDMETHOD.

  METHOD cleanup_finalize.
  ENDMETHOD.

ENDCLASS.

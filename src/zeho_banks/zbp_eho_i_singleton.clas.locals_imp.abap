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
    METHODS get_instance_features FOR INSTANCE FEATURES
      IMPORTING keys REQUEST requested_features FOR Bank RESULT result.
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

  METHOD get_instance_features.
    TYPES : BEGIN  OF lty_bank ,
            bankcode TYPE bankk,
            is_draft TYPE abp_behv_flag,
            exist(1),
            END OF lty_bank.
    DATA : lt_bank TYPE TABLE OF lty_bank.

    SELECT DISTINCT data~*
*                    COUNT(  )
    FROM @KEYS as data
    INNER JOIN zeho_a_comp as comp
                 ON data~Bankcode = comp~bankcode
             INTO TABLE @DATA(lt_keys).


*    LOOP AT lt_keys ASSIGNING FIELD-SYMBOL(<fs_key>).
*       APPEND INITIAL LINE TO lt_bank ASSIGNING FIELD-SYMBOL(<fs_bank>).
*       <fs_bank>-bankcode = <fs_key>-Bankcode.
*       <fs_bank>-is_draft = <fs_key>-is_draft.
*
*
*    ENDLOOP.

       result = VALUE #( FOR key IN lt_keys  (
                        %is_DRAFT = KEY-%is_draft
                        Bankcode  = KEY-Bankcode
                        %features-%delete  = if_abap_behv=>fc-o-disabled
                        ) ).
*     result = VALUE #( FOR key IN KEYS  (
*                        %is_DRAFT = KEY-%is_draft
*                        Bankcode  = KEY-Bankcode
*                        %features-%delete  = if_abap_behv=>fc-o-disabled
*                        ) ).

*     READ ENTITIES OF Bank IN LOCAL MODE
*     ENTITY Bank
*       FIELDS ( Bankcode )
*       WITH CORRESPONDING #( keys )
*     RESULT DATA(Banks)
*     FAILED failed.

*     SELECT count( * )
*     from zeho_a_comp
*     where bankcode = bank-


    " evaluate the conditions, set the operation state, and set result parameter
*    result = VALUE #( FOR bank IN Banks
*                      ( %tky                   = travel-%tky
*
**                        %features-%update      = COND #( WHEN travel-OverallStatus = travel_status-accepted
**                                                        THEN if_abap_behv=>fc-o-disabled ELSE if_abap_behv=>fc-o-enabled   )
*                        %features-%delete      = COND #( WHEN travel-OverallStatus = travel_status-open
*                                                        THEN if_abap_behv=>fc-o-enabled ELSE if_abap_behv=>fc-o-disabled   )
*
*                    ) ).
*

  ENDMETHOD.

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

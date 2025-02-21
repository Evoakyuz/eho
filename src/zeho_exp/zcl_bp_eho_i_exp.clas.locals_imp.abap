CLASS lhc_EXP DEFINITION INHERITING FROM cl_abap_behavior_handler.
  PRIVATE SECTION.

    METHODS get_instance_authorizations FOR INSTANCE AUTHORIZATION
      IMPORTING keys REQUEST requested_authorizations FOR exp RESULT result.
    METHODS getdefaultsforcreate FOR READ
      IMPORTING keys FOR FUNCTION exp~getdefaultsforcreate RESULT result.

ENDCLASS.

CLASS lhc_EXP IMPLEMENTATION.

  METHOD get_instance_authorizations.
  ENDMETHOD.

  METHOD GetDefaultsForCreate.
    DATA : lt_pur_con TYPE TABLE FOR READ RESULT  zeho_i_exp\\exp .
    APPEND INITIAL LINE TO lt_pur_con ASSIGNING FIELD-SYMBOL(<fs_acct>).
    <fs_acct>-Dc = 'A'.
    result = VALUE #( FOR <fs_rec_m> IN lt_pur_con
                         (

                            %cid = keys[ 1 ]-%cid
                            %param-Dc = <fs_rec_m>-Dc )
                         ) .

  ENDMETHOD.

ENDCLASS.

CLASS lsc_ZEHO_I_EXP DEFINITION INHERITING FROM cl_abap_behavior_saver.
  PROTECTED SECTION.

    METHODS adjust_numbers REDEFINITION.

    METHODS cleanup_finalize REDEFINITION.

ENDCLASS.

CLASS lsc_ZEHO_I_EXP IMPLEMENTATION.

  METHOD adjust_numbers.
  ENDMETHOD.

  METHOD cleanup_finalize.
  ENDMETHOD.

ENDCLASS.

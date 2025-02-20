CLASS lhc_ACCT DEFINITION INHERITING FROM cl_abap_behavior_handler.
  PRIVATE SECTION.

    METHODS get_instance_authorizations FOR INSTANCE AUTHORIZATION
      IMPORTING keys REQUEST requested_authorizations FOR acct RESULT result.
    METHODS GetDefaultsForCreate FOR READ
      IMPORTING keys FOR FUNCTION acct~getdefaultsforcreate RESULT result.
*    METHODS DefaultForCreate FOR READ
*      IMPORTING keys FOR FUNCTION ACCT~DefaultForCreate RESULT result.

ENDCLASS.

CLASS lhc_ACCT IMPLEMENTATION.

  METHOD get_instance_authorizations.
  ENDMETHOD.

  METHOD GetDefaultsForCreate.
       DATA : lt_pur_con TYPE TABLE FOR READ RESULT  zeho_i_acct\\acct .
      APPEND INITIAL LINE TO lt_pur_con ASSIGNING FIELD-SYMBOL(<fs_acct>).
      <fs_acct>-Dc = 'A'.
      <fs_acct>-Priority = 1.
   result = VALUE #( FOR <fs_rec_m> IN lt_pur_con
                        (

                           %cid = keys[ 1 ]-%cid
                           %param-Priority = <fs_rec_m>-Priority
                           %param-Dc = <fs_rec_m>-Dc )
                        ) .
    if 1 = 2.

    endif.
  ENDMETHOD.



ENDCLASS.

CLASS lsc_ZEHO_I_ACCT DEFINITION INHERITING FROM cl_abap_behavior_saver.
  PROTECTED SECTION.

    METHODS adjust_numbers REDEFINITION.

    METHODS cleanup_finalize REDEFINITION.

ENDCLASS.

CLASS lsc_ZEHO_I_ACCT IMPLEMENTATION.

  METHOD adjust_numbers.
  if 1 = 1.

  endif.
  ENDMETHOD.

  METHOD cleanup_finalize.
   if 1 = 1.

   endif.
  ENDMETHOD.

ENDCLASS.

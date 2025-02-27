CLASS zeho_cl_pre_run DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES if_oo_adt_classrun.

    CONSTANTS : c_acc TYPE tabname VALUE 'ZEHO_A_ACC'.


    TYPES : BEGIN OF  lty_tab_fiels,
              mandt     TYPE sy-mandt,
              tabname   TYPE tabname,
              fieldname TYPE zeho_de_fieldname,
            END OF lty_tab_fiels.

    DATA : lt_fields TYPE TABLE OF lty_tab_fiels.




  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zeho_cl_pre_run IMPLEMENTATION.
  METHOD if_oo_adt_classrun~main.
    DATA : go_struct TYPE REF TO cl_abap_structdescr,
           gt_comp   TYPE abap_component_tab,
           gs_comp   TYPE abap_componentdescr.
    DATA : lc_service TYPE REF TO zeho_cl_service.
    CLEAR lt_fields.
    go_struct ?= cl_abap_typedescr=>describe_by_name( c_acc ).
    gt_comp = go_struct->get_components( ).

    DELETE gt_comp WHERE name = 'CLIENT'.
    lt_fields  =  VALUE #(  FOR comp IN gt_comp
                             (
                               mandt   = sy-mandt
                               tabname = c_acc
                               fieldname =  comp-name    )
                             ).

    DELETE FROM  zeho_a_tab_fld WHERE tabname = @c_acc.
    INSERT  zeho_a_tab_fld  FROM TABLE @lt_fields .


    DATA ls_bank TYPE zeho_a_bank.
    DATA lt_bank TYPE TABLE OF zeho_a_bank.
    ls_bank-bankcode = 'DUMMY'.
    ls_bank-client = sy-mandt.
    APPEND ls_bank TO lt_bank.
    MODIFY  zeho_a_bank FROM  TABLE @lt_bank.

    DATA  : bankcode   TYPE zeho_tt_bankcode_range,
            s_bankcode TYPE zeho_s_bankcode_range,
            bukrs      TYPE zeho_tt_bukrs_range,
            s_bukrs    TYPE zeho_s_bukrs_range,
            account    TYPE zeho_tt_acc_range,
*      !LOG type ref to ZSOA_EHO_CL_APP_LOG optional
            begdate    TYPE datum,
            enddate    TYPE datum.
    begdate = '20240226'.
    enddate = '20240226'.
    s_bankcode-sign = 'I'.
    s_bankcode-option = 'EQ'.
    s_bankcode-low = 'AKBANK'.
    APPEND s_bankcode TO bankcode.
    s_bukrs-sign = 'I'.
    s_bukrs-option = 'EQ'.
    s_bukrs-low = '2200'.
    APPEND s_bukrs TO bukrs.
    TRY.
        CREATE OBJECT lc_service
          EXPORTING
            bankcode = bankcode
            bukrs    = bukrs
*           account  =
            begdate  = begdate
            enddate  = enddate.
      CATCH zeho_cl_messages.
    ENDTRY.


  ENDMETHOD.

ENDCLASS.

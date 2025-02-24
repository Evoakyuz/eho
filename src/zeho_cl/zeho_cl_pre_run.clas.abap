CLASS zeho_cl_pre_run DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
  INTERFACES if_oo_adt_classrun.

  CONSTANTS : c_acc TYPE tabname VALUE 'ZEHO_A_ACC'.


  TYPES : BEGIN OF  lty_tab_fiels,
          mandt   TYPE sy-mandt ,
          tabname TYPE tabname,
          fieldname TYPE zeho_de_fieldname,
          END OF lty_tab_fiels.

  DATA : lt_fields TYPE TABLE OF lty_tab_fiels.




  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zeho_cl_pre_run IMPLEMENTATION.
  METHOD if_oo_adt_classrun~main.
    DATA : go_struct TYPE REF TO cl_abap_structdescr,
           gt_comp TYPE abap_component_tab,
           gs_comp TYPE abap_componentdescr.

    CLEAR lt_fields.
    go_struct ?= cl_abap_typedescr=>describe_by_name( c_acc ).
    gt_comp = go_struct->get_components( ).

    DELETE gt_comp WHERE name = 'CLIENT'.
   lt_fields  =  VALUE #(  FOR comp in gt_comp
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
   INSERT zeho_a_bank FROM  TABLE @lt_bank.


  ENDMETHOD.

ENDCLASS.

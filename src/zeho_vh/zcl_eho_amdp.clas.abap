CLASS zcl_eho_amdp DEFINITION PUBLIC FINAL CREATE PUBLIC.
  PUBLIC SECTION.
    INTERFACES if_sadl_exit_calc_element_read.

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_eho_amdp IMPLEMENTATION.


  METHOD if_sadl_exit_calc_element_read~calculate.
    DATA:
      lt_view_data TYPE STANDARD TABLE OF zeho_c_comps WITH EMPTY KEY.

    lt_view_data = CORRESPONDING #( it_original_data ).

    LOOP AT lt_view_data REFERENCE INTO DATA(lr_view_data).
*      lr_view_data->PricePerUnit = lr_view_data->PositionPrice / lr_view_data->PositionQuantity.
    ENDLOOP.
*    CLEAR it_original_data.
*    ct_calculated_data = CORRESPONDING #( lt_view_data ).
    CLEAR ct_calculated_data.
    DATA : go_struct TYPE REF TO cl_abap_structdescr,
           gt_comp   TYPE abap_component_tab,
           gs_comp   TYPE abap_componentdescr.
    go_struct ?= cl_abap_typedescr=>describe_by_name( 'ZEHO_A_ACC' ).
    gt_comp = go_struct->get_components( ).
    FIELD-SYMBOLS : <fs_val> TYPE any.
    LOOP AT gt_comp ASSIGNING FIELD-SYMBOL(<fs_comp>).
      APPEND INITIAL LINE TO ct_calculated_data ASSIGNING FIELD-SYMBOL(<fs_data>).
      ASSIGN COMPONENT 'FIELDNAME' OF STRUCTURE <fs_data> TO <fs_val>.
      IF <fs_val> IS ASSIGNED.
        <fs_val> = <fs_comp>-name.
      ENDIF.
*<fs_data>-fieldname = <fs_comp>-name.
    ENDLOOP.
  ENDMETHOD.


  METHOD if_sadl_exit_calc_element_read~get_calculation_info.
  ENDMETHOD.

ENDCLASS.

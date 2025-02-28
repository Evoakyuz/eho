CLASS zeho_cl_rap_activities_query DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES if_rap_query_provider .
  PROTECTED SECTION.
  PRIVATE SECTION.
    TYPES ty_response_tab TYPE STANDARD TABLE OF zeho_i_activities_custom WITH EMPTY KEY.
    DATA : lv_where TYPE string.
    DATA : tt_exp        TYPE zeho_tt_exp,
           tt_acct       TYPE zeho_tt_actt,
           tt_activities TYPE zeho_tt_activities,
           tt_customer   TYPE zeho_tt_customer,
           tt_supplier   TYPE zeho_tt_supplier.
ENDCLASS.



CLASS zeho_cl_rap_activities_query IMPLEMENTATION.


  METHOD if_rap_query_provider~select.

    DATA : lt_activities TYPE zeho_tt_activities.
    DATA response_Tab TYPE ty_response_tab.
    DATA tt_activitytype_Range TYPE zeho_tt_activitytype_range.
    DATA rd_activity TYPE REF TO ZEHO_S_ACTIVITY.

    CLEAR : tt_acct , tt_activities , tt_exp , tt_customer , tt_supplier.

*    IF NOT io_request->is_data_requested( ).
*      RETURN.
*    ENDIF.
    " GET SELECT-OPTIONS

    TRY.
        DATA(filters) = io_request->get_filter( )->get_as_ranges( ).
      CATCH cx_rap_query_filter_no_range INTO DATA(ex_ranges).
*        RAISE EXCEPTION NEW zcx_rap_query_prov_not_im( previous = ex_ranges ).
    ENDTRY.
    DATA(selopt_bankcode) = VALUE #( filters[ name = 'BANKCODE' ]-range OPTIONAL ).
    DATA(selopt_bukrs) = VALUE #( filters[ name = 'BUKRS' ]-range OPTIONAL ).
    DATA(selopt_actdate) = VALUE #( filters[ name = 'ACT_DATE' ]-range OPTIONAL ).




    TRY.
        DATA(lv_where) = zeho_cl_seltab=>combine_seltabs(
                           it_named_seltabs = VALUE #(
                         (    name = 'BANKCODE' dref = REF #(  selopt_bankcode ) )
                         (    name = 'BUKRS'    dref = REF #(  selopt_bukrs    ) )
                         (    name = 'ACT_DATE' dref = REF #(  selopt_actdate  ) )
                          )
                         ).

        zeho_cl_amdp=>get_acitivities(
          EXPORTING
            i_where_clause = lv_where
            i_mandt        = sy-mandt
          IMPORTING
            et_aa          = tt_activities
        ).

        if tt_activities is not INITIAL.

        CLEAR lv_where.
        lt_activities = tt_activities.
        SORT lt_activities by  activity_type.
        DELETE ADJACENT DUPLICATES FROM lt_activities COMPARING activity_type.

        tt_activitytype_range = VALUE #( for act IN  lt_activities
                       ( sign = 'I' option = 'EQ' low =  act-activity_type )
                ).


         lv_where = zeho_cl_seltab=>combine_seltabs(
                           it_named_seltabs = VALUE #(
                         (    name = 'BANKCODE' dref = REF #(  selopt_bankcode ) )
                         (    name = 'BUKRS'    dref = REF #(  selopt_bukrs    ) )
                         (    name = 'ACTIVITY_TYPE' dref = REF #(  tt_activitytype_range  ) )
                          )
                         ).

       zeho_cl_amdp=>get_custom_for_activity(
         EXPORTING
           i_where_clause = lv_where
           i_mandt        = sy-mandt
         IMPORTING
           et_exp         = tt_exp
           et_actt        = tt_acct
       ).

       zeho_cl_amdp=>get_partners(
         EXPORTING
           i_where     = lv_where
         IMPORTING
           et_customer = tt_customer
           et_supplier = tt_supplier
       ).


       LOOP AT tt_activities REFERENCE INTO rd_activity WHERE belnr = ' '
                                                          and cancel_process = ' '.

        zeho_cl_document_processing=>fill_list(
          EXPORTING
            tt_exp   = tt_exp
            tt_acct  = tt_acct
            tt_cust  = tt_customer
            tt_suppl = tt_supplier
          CHANGING
            rd_aa    = rd_activity
        ).

       ENDLOOp.

        endif.

      CATCH zeho_cl_messages.
        "handle exception
    ENDTRY.


    SELECT *
    FROM zeho_a_aa
    WHERE bankcode IN @selopt_bankcode
      AND bukrs    IN @selopt_bukrs
      AND act_date IN @selopt_actdate
    INTO CORRESPONDING FIELDS OF TABLE @response_Tab.

    IF response_Tab IS NOT INITIAL.


    ENDIF.



  ENDMETHOD.
ENDCLASS.

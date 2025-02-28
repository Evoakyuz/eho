CLASS zeho_cl_document_processing DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    CONSTANTS : c_stcd TYPE char4 VALUE 'STCD'.


    CLASS-DATA: cl_doc  TYPE REF TO zeho_cl_document_processing,
                e_subrc TYPE sy-subrc,
                stcd_counter TYPE sy-tabix,
                field_name   TYPe zeho_de_fieldname.
    CLASS-METHODS : fill_list IMPORTING tt_exp   TYPE zeho_tt_exp
                                        tt_acct  TYPE zeho_tt_actt
                                        tt_cust  TYPE zeho_tt_customer
                                        tt_suppl TYPE zeho_tt_supplier
                              CHANGING  rd_aa    TYPE REF TO zeho_s_activity.



  PROTECTED SECTION.
  PRIVATE SECTION.
    METHODS : check_explanation IMPORTING tt_exp  TYPE zeho_tt_exp
                                CHANGING  rd_aa   TYPE REF TO zeho_s_activity
                                          e_subrc TYPE sy-subrc.
   METHODS : check_activity_type IMPORTING tt_acct  TYPE zeho_tt_actt
                                            tt_cust  TYPE zeho_tt_customer
                                            tt_suppl TYPE zeho_tt_supplier
                                 CHANGING  rd_aa   TYPE REF TO zeho_s_activity
                                          e_subrc TYPE sy-subrc.

ENDCLASS.



CLASS zeho_cl_document_processing IMPLEMENTATION.
  METHOD fill_list.

*   me->check_explanation
    CREATE OBJECT cl_doc.
    CLEAR e_subrc.
    cl_doc->check_explanation(
      EXPORTING
        tt_exp = tt_exp
      CHANGING
        rd_aa  = rd_aa
        e_subrc = e_subrc
    ).

   CHECK e_subrc <> 0.


  ENDMETHOD.

  METHOD check_explanation.
    DATA lrd_exp TYPE REF TO zeho_s_exp.
    DATA lv_check TYPE char1.

    LOOP AT tt_exp REFERENCE INTO lrd_exp.
      IF rd_aa->description CP lrd_exp->explanation AND lrd_exp->dc IS INITIAL.
        lv_check = abap_true.
      ELSEIF rd_aa->description CP lrd_exp->explanation AND rd_aa->shkzg EQ lrd_exp->dc .
        lv_check = abap_true.
      ENDIF.
      IF  lv_check = abap_true.
          rd_aa->blart          =    lrd_exp->blart          .
          rd_aa->lifnr          =    lrd_exp->lifnr          .
          rd_aa->kunnr          =    lrd_exp->kunnr          .
          rd_aa->secondgl_acc   =    lrd_exp->secondgl_acc   .
          rd_aa->kostl          =    lrd_exp->kostl          .
          rd_aa->prctr          =    lrd_exp->prctr          .
          rd_aa->gsber          =    lrd_exp->gsber          .
          rd_aa->umskz          =    lrd_exp->umskz          .
          rd_aa->mwskz          =    lrd_exp->mwskz          .
          rd_aa->cancel_process =    lrd_exp->cancel_process .
        e_subrc = 0.
        EXIT.
      ENDIF.

    ENDLOOP.


  ENDMETHOD.

  METHOD check_activity_type.
    DATA lv_count TYPE sy-tabix.
     DATA lv_priority  TYPE zeho_a_actt-priority.
     DATA lrd_acttype TYPE REF TO zeho_s_acct.
     DATA : lrd_cust TYPE REF TO zeho_s_customer.
     DATA : lrd_suppl TYPE REF TO zeho_s_supplier.
    CLEAR lv_count.
    CLEAR lv_count.
    LOOP AT tt_acct TRANSPORTING NO FIELDS WHERE  bukrs         = rd_aa->bukrs
                                             AND  bankcode      = rd_aa->bankcode
                                             AND  activity_type = rd_aa->activity_type.
      lv_count = lv_count + 1.
    ENDLOOP.
    lv_priority = 1.
     DO lv_count TIMES.
      READ TABLE tt_acct REFERENCE INTO lrd_acttype WITH KEY bukrs         = rd_aa->bukrs
                                                             bankcode      = rd_aa->bankcode
                                                             activity_type = rd_aa->activity_type
                                                             priority      = lv_priority.

      IF lrd_acttype->dc IS NOT INITIAL
        AND lrd_acttype->dc <> rd_aa->shkzg.
        lv_priority = lv_priority + 1.
        CONTINUE.
      ENDIF.

       IF lrd_acttype->vendor_control IS NOT INITIAL.
           CLEAR stcd_counter.
           do 3 times.
             field_name = c_stcd && stcd_counter.

             READ TABLE tt_suppl REFERENCE INTO lrd_suppl WITH KEY (field_name) = rd_aa->sender_vkn.
             IF sy-subrc = 0 and rd_aa->sender_vkn is not initial.
*           rd_aa->lifnr = lrd_suppl->lifnr.
*           rd_aa->name1 = lrd_suppl->name1.
*           rd_aa->customization_type = me->c_act..
*           rd_aa->affacted_priority = lv_priority.
*            matching_ok = abap_true.

             endif.

           enddo.
       endif.


     ENDDO.




  ENDMETHOD.

ENDCLASS.

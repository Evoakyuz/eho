CLASS lcl_buffer DEFINITION CREATE PRIVATE .

  PUBLIC SECTION.
    INTERFACES zeho_activity_if.


*    TYPES : BEGIN OF ts_message,
*              bankcode TYPE zeho_i_activities-bankcode,
*              bukrs    TYPE zeho_i_activities-bukrs,
*              act_date TYPE zeho_i_activities-act_date,
*              act_time TYPE zeho_i_activities-act_time,
*              act_no   TYPE zeho_i_activities-act_no,
*              iban     TYPE zeho_i_activities-iban,
*              branch   TYPE zeho_i_activities-branch,
*              symsg    TYPE symsg,
*              fields   TYPE string_table,
*            END OF ts_message,
*
*            ty_activities      TYPE STANDARD TABLE OF zeho_i_activities,
*            tt_activity_in     TYPE TABLE FOR READ IMPORT zeho_i_activities,
*            tt_activity_out    TYPE TABLE FOR READ RESULT zeho_i_activities,
*            tt_activity_failed TYPE TABLE FOR FAILED zeho_i_activities,
*            tt_message         TYPE STANDARD TABLE OF ts_message.
*    TYPES: lt_entyry  TYPE TABLE FOR ACTION IMPORT i_journalentrytp~post,
*           ls_entry   TYPE LINE OF lt_entyry,
*           lty_glitem TYPE LINE OF ls_entry-%param-_glitems.
*
*    TYPES  : ty_je_post  TYPE TABLE FOR ACTION IMPORT i_journalentrytp~post.
*
*    TYPES : BEGIN OF ty_bapi_data,
*              bankcode    TYPE zeho_i_activities-bankcode,
*              bukrs       TYPE zeho_i_activities-bukrs,
*              act_date    TYPE zeho_i_activities-act_date,
*              act_time    TYPE zeho_i_activities-act_time,
*              act_no      TYPE zeho_i_activities-act_no,
*              iban        TYPE zeho_i_activities-iban,
*              branch      TYPE zeho_i_activities-branch,
*              acccount_no TYPE zeho_i_activities-acccount_no,
*              bapi_data   TYPE ty_je_post,
*            END OF ty_bapi_data.
*
*    TYPES : tty_bapi_data TYPE TABLE OF ty_bapi_data.
*
*    TYPES: ty_failed   TYPE TABLE FOR FAILED zeho_i_activities,
*           ty_reported TYPE TABLE FOR REPORTED zeho_i_activities,
*           ty_result   TYPE TABLE FOR READ RESULT zeho_i_activities.
*
*    TYPES : BEGIN OF ty_act_with_key,
*              act TYPE zeho_s_activity,
*              cid TYPE abp_behv_cid,
*              pid TYPE abp_behv_pid,
*            END OF ty_act_with_key,
*            tty_act_with_key TYPE STANDARD TABLE OF ty_act_with_key WITH DEFAULT KEY.


    DATA : tt_act_key TYPE zeho_activity_if~tty_act_with_key.




    CLASS-METHODS get_instance
      RETURNING VALUE(ro_instance) TYPE REF TO lcl_buffer.

    METHODS get_data
      IMPORTING it_activity        TYPE zeho_activity_if~tt_activity_in OPTIONAL
      EXPORTING et_acitivity       TYPE zeho_activity_if~tt_activity_out
                et_activity_failed TYPE zeho_activity_if~tt_activity_failed.
    METHODS set_post   IMPORTING tt_act TYPE zeho_tt_activities.
    METHODS get_post   EXPORTING tt_act TYPE zeho_tt_activities.
    METHODS set_keys   IMPORTING tt_keys TYPE zeho_activity_if~tty_act_with_key.
    METHODS get_keys   EXPORTING tt_keys TYPE zeho_activity_if~tty_act_with_key.
    METHODS set_reverse  IMPORTING tt_act TYPE zeho_activity_if~tty_act_with_key.
    METHODS get_reverse  EXPORTING tt_act TYPE zeho_activity_if~tty_act_with_key.
    METHODS post_doc IMPORTING tt_act      TYPE zeho_tt_activities
                               tt_bapidata TYPE zeho_activity_if~tty_bapi_data
                     EXPORTING tt_keys     TYPE zeho_activity_if~tty_act_with_key
                               tt_failed   TYPE zeho_activity_if~ty_failed
                               tt_reported TYPE zeho_activity_if~ty_reported.
    METHODS set_reread IMPORTING tt_act TYPE zeho_tt_activities.
    METHODS set_bapi_data IMPORTING tt_bapi_data TYPE zeho_activity_if~tty_bapi_data.
    METHODS get_bapi_data EXPORTING tt_bapi_data TYPE zeho_activity_if~tty_bapi_data.





    METHODS clear_cache.

*    METHOD read_activity.
*                et_message         TYPE tt_message .

  PRIVATE SECTION.
    CLASS-DATA: go_instance TYPE REF TO lcl_buffer.
    DATA : tt_activity TYPE zeho_activity_if~ty_activities.
    DATA : tt_glitem   TYPE zeho_activity_if~lty_glitem.
    DATA : tt_post  TYPE zeho_tt_activities.
    DATA : lt_keys  TYPE zeho_activity_if~tty_act_with_key.
    DATA : tt_reread TYPE zeho_tt_activities.
    DATA : tt_reverse_key TYPE zeho_activity_if~tty_act_with_key.
    DATA : post_bapi_data TYPE zeho_activity_if~tty_bapi_data.


ENDCLASS.


CLASS lcl_buffer IMPLEMENTATION.
  METHOD get_instance.
    IF go_instance IS NOT BOUND.
      go_instance = NEW #( ).
    ENDIF.
    ro_instance = go_instance.
  ENDMETHOD.

  METHOD get_data.
    DATA lt_activity  TYPE TABLE FOR READ IMPORT zeho_i_activities.
    CLEAR lt_activity.
    IF it_activity IS SUPPLIED.
*       lt_activity = it_activity.
      IF tt_activity IS NOT INITIAL.

        LOOP AT it_activity ASSIGNING FIELD-SYMBOL(<fS_activity>).
          IF line_exists( tt_activity[  bankcode                   = <fS_activity>-bankcode
                                        bukrs                      = <fS_activity>-bukrs
                                        iban                       = <fS_activity>-iban
                                        branch                     = <fS_activity>-branch
                                        acccount_no                = <fS_activity>-acccount_no
                                        act_date                   = <fS_activity>-act_date
                                        act_time                   = <fS_activity>-act_time
                                        act_no                     = <fS_activity>-act_no  ] ) .
            IF line_exists( tt_reread[  bankcode                   = <fS_activity>-bankcode
                                        bukrs                      = <fS_activity>-bukrs
                                        iban                       = <fS_activity>-iban
                                        branch                     = <fS_activity>-branch
                                        acccount_no                = <fS_activity>-acccount_no
                                        act_date                   = <fS_activity>-act_date
                                        act_time                   = <fS_activity>-act_time
                                        act_no                     = <fS_activity>-act_no  ] ) .

              APPEND <fS_activity> TO lt_activity.
            ENDIF.



          ELSE.
            APPEND <fS_activity> TO lt_activity.

          ENDIF.
        ENDLOOP.

      ELSE.
        lt_activity = it_activity.
      ENDIF.

      IF lt_activity IS NOT INITIAL.
        zeho_cl_post_buffer=>get_instance(  )->get_data(
          EXPORTING
            it_activity        = lt_activity
          IMPORTING
            et_acitivity       = et_acitivity
            et_activity_failed = et_activity_failed
        ).
      ENDIF.

      IF et_acitivity IS NOT INITIAL.
        DATA: lt_activity_update TYPE zeho_activity_if~ty_activities.
        CLEAR lt_activity_update.
        LOOP AT et_acitivity ASSIGNING FIELD-SYMBOL(<fS_act>).
          READ TABLE tt_activity ASSIGNING FIELD-SYMBOL(<fs_activ>)
                               WITH KEY    bankcode                   = <fS_act>-bankcode
                                           bukrs                      = <fS_act>-bukrs
                                           iban                       = <fS_act>-iban
                                           branch                     = <fS_act>-branch
                                           acccount_no                = <fS_act>-acccount_no
                                           act_date                   = <fS_act>-act_date
                                           act_time                   = <fS_act>-act_time
                                           act_no                     = <fS_act>-act_no   .
          IF sy-subrc = 0.
            <fs_activ> = CORRESPONDING #( <fS_act> ).
          ELSE.
            APPEND INITIAL LINE TO tt_activity ASSIGNING <fs_activ>.
            <fs_activ> = CORRESPONDING #( <fS_act> ).
          ENDIF.
        ENDLOOP.
*      ENDIF.
**        tt_activity = CORRESPONDING #( BASE (  tt_activity ) et_acitivity ).
      ELSE.
        et_acitivity = VALUE #( FOR ls_act IN it_activity
                                FOR act IN tt_activity
                                WHERE   (   bankcode                  = ls_act-bankcode
                                    AND    bukrs                      = ls_act-bukrs
                                    AND    iban                       = ls_act-iban
                                    AND    branch                     = ls_act-branch
                                    AND    acccount_no                = ls_act-acccount_no
                                    AND    act_date                   = ls_act-act_date
                                    AND    act_time                   = ls_act-act_time
                                    AND    act_no                     = ls_act-act_no   )

                                (    CORRESPONDING #( act  )  )

                            ).

      ENDIF.

    ENDIF.

  ENDMETHOD.

  METHOD get_post.
    IF tt_post IS NOT INITIAL.
      tt_act = tt_post.
    ENDIF.
  ENDMETHOD.
  METHOD set_reverse.

    IF tt_act IS NOT INITIAL.
      tt_reverse_key = tt_act.
    ENDIF.

  ENDMETHOD..

  METHOD get_reverse.
    IF tt_reverse_key IS NOT INITIAL.
      tt_act = tt_reverse_key.
    ENDIF.
  ENDMETHOD.

  METHOD set_post.
    IF tt_act IS NOT INITIAL.
      tt_post = tt_act.
    ENDIF.
  ENDMETHOD.

  METHOD get_keys.
    IF lt_keys IS NOT INITIAL.
      tt_keys = lt_keys.
    ENDIF.
  ENDMETHOD.

  METHOD set_keys.
    IF tt_keys IS NOT INITIAL.
      lt_keys = tt_keys.
    ENDIF.
  ENDMETHOD.

  METHOD set_reread.
    IF tt_act IS NOT INITIAL.
      tt_reread = tt_act.
    ENDIF.
  ENDMETHOD.

  METHOD get_bapi_data.
    IF post_bapi_data IS NOT INITIAL.
      tt_bapi_data = post_bapi_data.
    ENDIF.
  ENDMETHOD.

  METHOD set_bapi_data.
    IF tt_bapi_data IS NOT INITIAL.
      post_bapi_data = tt_bapi_data.
    ENDIF.
  ENDMETHOD.

  METHOD post_doc.

    DATA: lt_je_deep TYPE TABLE FOR ACTION IMPORT i_journalentrytp~post,
          lv_cid     TYPE abp_behv_cid,
          ls_je      TYPE STRUCTURE FOR ACTION IMPORT i_journalentrytp~post.
    CLEAR:  lt_je_deep , ls_je.


    DATA : cl_document TYPE REF TO zeho_cl_document_processing.


    IF cl_document IS NOT BOUND.
      CREATE OBJECT cl_document.
    ENDIF.

    DATA ls_aa TYPE zeho_i_activities.
    LOOP AT tt_act ASSIGNING FIELD-SYMBOL(<fs_aa>).

      lt_je_deep   = VALUE #( tt_bapidata[
                       bankcode = <fs_aa>-bankcode
                       bukrs    = <fs_aa>-bukrs
                       iban     = <fs_aa>-iban
                       branch   = <fs_aa>-branch
                       act_no   = <fs_aa>-act_no
                       act_date = <fs_aa>-act_date
                       act_time = <fs_aa>-act_time
                       acccount_no = <fs_aa>-acccount_no
                      ]-bapi_data  OPTIONAL ).

      IF lt_je_deep IS NOT INITIAL.
*        TRY.
*            lv_cid = to_upper( cl_uuid_factory=>create_system_uuid( )->create_uuid_x16( ) ).
*          CATCH cx_uuid_error.
*            ASSERT 1 = 0.
*        ENDTRY.
*        ls_je-%cid = lv_cid.

        ls_aa = CORRESPONDING #( <fs_aa> ).



**      cl_document->fill_header(
**        EXPORTING
**          rd_aa = ls_aa
**        CHANGING
**          e_je  = ls_je
**      ).
**
**      cl_document->fill_gl_account(
**        EXPORTING
**
**          rd_aa     = ls_aa
**          e_dc      = COND #( WHEN  ls_aa-shkzg = 'H'
**                      THEN 'S' ELSE 'H'  )
**          e_hkont   = <fs_aa>-hkont
**        CHANGING
**          tt_glitems = ls_je-%param-_glitems
**      ).
**
**      IF <fs_aa>-secondgl_acc IS NOT INITIAL.
**
**
**        cl_document->fill_secondgl_account(
**        EXPORTING
**          rd_aa     = ls_aa
**          e_dc      = COND #( WHEN  ls_aa-shkzg = 'H'
**                      THEN 'S' ELSE 'H'  )
**          e_hkont   = <fs_aa>-secondgl_acc
**        CHANGING
**          tt_glitems = ls_je-%param-_glitems
**            ).
**      ELSEIF <fs_aa>-lifnr IS NOT INITIAL.
**
**        cl_document->fill_supplier(
**          EXPORTING
**            rd_aa      = ls_aa
**            e_dc      = COND #( WHEN  ls_aa-shkzg = 'H'
**                      THEN 'S' ELSE 'H'  )
**          CHANGING
**            tt_apitems = ls_je-%param-_apitems
**        ).
**      ELSEIF <fs_aa>-kunnr IS NOT INITIAL.
**        cl_document->fill_customer(
**          EXPORTING
**            rd_aa      = ls_aa
**            e_dc      = COND #( WHEN  ls_aa-shkzg = 'H'
**                         THEN 'S' ELSE 'H'  )
**          CHANGING
**            tt_aritems =  ls_je-%param-_aritems
**        ).
**      ENDIF.
**
**      APPEND ls_je TO lt_je_deep.

        MODIFY ENTITIES OF i_journalentrytp
          ENTITY journalentry
          EXECUTE post FROM lt_je_deep
          FAILED FINAL(ls_failed_deep)
          REPORTED FINAL(ls_reported_deep)
          MAPPED FINAL(ls_mapped_deep) .


        IF ls_failed_deep IS NOT INITIAL.
*        APPEND INITIAL LINE TO tt_keys ASSIGNING FIELD-SYMBOL(<fs_temp_key>).
*        <fs_temp_key>-act = CORRESPONDING #( ls_aa ).
*        <fs_temp_key>-cid = lv_cid.
*        <fs_temp_key>-pid = VALUE #( ls_mapped_deep-journalentry[ %cid = lv_cid ]-%pid OPTIONAL ).
          LOOP AT ls_reported_deep-journalentry ASSIGNING FIELD-SYMBOL(<ls_reported_deep>).
            DATA(lv_result) = <ls_reported_deep>-%msg->if_message~get_text( ).

            APPEND VALUE #( bankcode = ls_aa-bankcode
                            bukrs    = ls_aa-bukrs
                            iban     = ls_aa-iban
                            branch   = ls_aa-branch
                            act_no   = ls_aa-act_no
                            act_date = ls_aa-act_date
                            act_time = ls_aa-act_time
                            acccount_no = ls_aa-acccount_no
                        %msg = <ls_reported_deep>-%msg ) TO tt_reported.

*         APPEND VALUE #( bankcode = ls_aa-bankcode
*                          = if_abap_behv=>mk-on
*                          %is_draft = if_abap_behv=>mk-on
*                          %msg = ls_report-%msg ) TO tt_reported.
          ENDLOOP.
        ELSE.

          APPEND INITIAL LINE TO tt_keys  ASSIGNING FIELD-SYMBOL(<fs_temp_key>).
          <fs_temp_key>-act = CORRESPONDING #( ls_aa ).
          <fs_temp_key>-cid = lt_je_deep[ 1 ]-%cid.
          <fs_temp_key>-pid = VALUE #( ls_mapped_deep-journalentry[ %cid = <fs_temp_key>-cid ]-%pid OPTIONAL ).

        ENDIF.

      ENDIF.
    ENDLOOP.
  ENDMETHOD.



  METHOD clear_cache.
    CLEAR :   tt_post , lt_keys, tt_reread , post_bapi_data.

  ENDMETHOD.



ENDCLASS.


CLASS lhc_Activities DEFINITION INHERITING FROM cl_abap_behavior_handler.

  PUBLIC SECTION.
    INTERFACES zeho_activity_if.
*    TYPES : BEGIN OF ty_act_with_key,
*              act TYPE zeho_s_activity,
*              cid TYPE abp_behv_cid,
*              pid TYPE abp_behv_pid,
*            END OF ty_act_with_key,
*            tty_act_with_key TYPE STANDARD TABLE OF ty_act_with_key WITH DEFAULT KEY.
*
*    TYPES  : ty_je_post  TYPE TABLE FOR ACTION IMPORT i_journalentrytp~post.
*    TYPES : BEGIN OF ty_bapi_data,
*              bankcode    TYPE zeho_i_activities-bankcode,
*              bukrs       TYPE zeho_i_activities-bukrs,
*              act_date    TYPE zeho_i_activities-act_date,
*              act_time    TYPE zeho_i_activities-act_time,
*              act_no      TYPE zeho_i_activities-act_no,
*              iban        TYPE zeho_i_activities-iban,
*              branch      TYPE zeho_i_activities-branch,
*              acccount_no TYPE zeho_i_activities-acccount_no,
*              bapi_data   TYPE ty_je_post,
*            END OF ty_bapi_data.
*
*    TYPES : tty_bapi_data TYPE TABLE OF ty_bapi_data.

    DATA : tt_act_key TYPE zeho_activity_if~tty_act_with_key.

  PRIVATE SECTION.

*    METHODS get_instance_features FOR INSTANCE FEATURES
*      IMPORTING keys REQUEST requested_features FOR Activities RESULT result.

    METHODS get_instance_authorizations FOR INSTANCE AUTHORIZATION
      IMPORTING keys REQUEST requested_authorizations FOR Activities RESULT result.

*    METHODS update FOR MODIFY
*      IMPORTING entities FOR UPDATE Activities.

    METHODS read FOR READ
      IMPORTING keys FOR READ Activities RESULT result.

    METHODS lock FOR LOCK
      IMPORTING keys FOR LOCK Activities.

    METHODS multipleSave FOR MODIFY
      IMPORTING keys FOR ACTION Activities~multipleSave RESULT result.
    METHODS reverseActivity FOR MODIFY
      IMPORTING keys FOR ACTION Activities~reverseActivity. "RESULT result.

    METHODS reverseActivitySelf FOR MODIFY
      IMPORTING keys FOR ACTION Activities~reverseActivitySelf RESULT result.
*       RESULT result.

*    METHODS saveActivity FOR MODIFY
*      IMPORTING keys FOR ACTION Activities~saveActivity  RESULT result.
    METHODS get_instance_features FOR INSTANCE FEATURES
      IMPORTING keys REQUEST requested_features FOR Activities RESULT result.
    METHODS update FOR MODIFY
      IMPORTING entities FOR UPDATE activities.
    METHODS checkMaintenance FOR MODIFY
      IMPORTING keys FOR ACTION activities~checkMaintenance .
    METHODS saveActivity FOR MODIFY
      IMPORTING keys FOR ACTION Activities~saveActivity.
    METHODS checkMaintenanceSelf FOR MODIFY
      IMPORTING keys FOR ACTION Activities~checkMaintenanceSelf RESULT result.
    METHODS saveActivitySelf FOR MODIFY
      IMPORTING keys FOR ACTION Activities~saveActivitySelf RESULT result.

*      RESULT result..

ENDCLASS.

CLASS lhc_Activities IMPLEMENTATION.



  METHOD get_instance_authorizations.
  ENDMETHOD.



  METHOD read.

    IF 1 = 2.
    ENDIF.
    DATA(lo_buffer) = lcl_buffer=>get_instance( ).

    lo_buffer->get_data(
      EXPORTING
        it_activity        = keys
      IMPORTING
        et_acitivity       = result
        et_activity_failed = failed-activities
*         et_message         =
    ).
  ENDMETHOD.

  METHOD lock.
  ENDMETHOD.

  METHOD multipleSave.
  ENDMETHOD.

  METHOD reverseActivity.

    DATA: lt_jr TYPE TABLE FOR ACTION IMPORT i_journalentrytp~Reverse.
    DATA : lv_cid     TYPE abp_behv_cid.
    DATA : tt_keys        TYPE zeho_activity_if~tty_act_with_key,
           lt_je_validate TYPE TABLE FOR FUNCTION IMPORT i_journalentrytp~validate,
           ls_je_validate TYPE STRUCTURE FOR FUNCTION IMPORT i_journalentrytp~validate.
    DATA ls_aa TYPE zeho_i_activities.
    DATA : tt_bapidata_rev TYPE zeho_activity_if~tty_bapi_data_reverse.

    CLEAR : tt_bapidata_rev , tt_keys.
    READ ENTITIES OF zeho_i_activities IN LOCAL MODE
    ENTITY Activities
    ALL FIELDS WITH
    CORRESPONDING #( keys )
    RESULT DATA(activities)
    FAILED failed.
***
***
***    """"""""""""""""""""""""""""""""""""""""""""""""""""""""
***    LOOP AT activities ASSIGNING FIELD-SYMBOL(<fs_act>).
***      CLEAR lt_jr.
***      ls_aa = CORRESPONDING #( <fs_act> ).
***      APPEND INITIAL LINE TO lt_jr ASSIGNING FIELD-SYMBOL(<jr>).
***      <jr>-companycode = <fs_act>-bukrs.
***      <jr>-fiscalyear  = <fs_act>-gjahr.
***      <jr>-accountingdocument = <fs_act>-belnr.
***      <jr>-%param = VALUE #(
***      postingdate = <fs_act>-act_date
***      reversalreason = '01'
***      createdbyuser = sy-uname
***      ).
***
***
***      lt_je_validate = CORRESPONDING #( lt_jr ).
***      READ ENTITIES OF i_journalentrytp
***      ENTITY journalentry
***      EXECUTE validate FROM lt_je_validate
***      RESULT DATA(lt_check_result)
***      FAILED DATA(ls_failed_deep)
***      REPORTED DATA(ls_reported_deep).
***
***
***      IF ls_failed_deep IS NOT INITIAL.
***
***        LOOP AT ls_reported_deep-journalentry ASSIGNING FIELD-SYMBOL(<ls_reported_deep>).
***          DATA(lv_result) = <ls_reported_deep>-%msg->if_message~get_text( ).
***
***          APPEND VALUE #(
***
***                 bankcode = ls_aa-bankcode
***                 bukrs    = ls_aa-bukrs
***                 iban     = ls_aa-iban
***                 branch   = ls_aa-branch
***                 act_no   = ls_aa-act_no
***                 act_date = ls_aa-act_date
***                 act_time = ls_aa-act_time
***                 acccount_no = ls_aa-acccount_no
***
***              ) TO failed-activities.
***
***        ENDLOOP.
***      ELSE.
***
***        APPEND VALUE #(
***
***           bankcode = ls_aa-bankcode
***           bukrs    = ls_aa-bukrs
***           iban     = ls_aa-iban
***           branch   = ls_aa-branch
***           act_no   = ls_aa-act_no
***           act_date = ls_aa-act_date
***           act_time = ls_aa-act_time
***           acccount_no = ls_aa-acccount_no
***           belnr       = ls_aa-belnr
***           gjahr       = ls_aa-gjahr
***           bapi_data   = CORRESPONDING #( lt_jr )
***        ) TO tt_bapidata_rev.
***
***      ENDIF.
***
***
***
***    ENDLOOP.
***
***    LOOP  AT tt_bapidata_rev ASSIGNING FIELD-SYMBOL(<fs_rev>).
***
***      MODIFY ENTITIES OF i_journalentrytp
***      ENTITY journalentry
***      EXECUTE reverse FROM <fs_rev>-bapi_data
***      FAILED DATA(ls_failed)
***      REPORTED DATA(ls_reported)
***      MAPPED DATA(ls_mapped).
***
***
***      IF ls_mapped-journalentry IS NOT INITIAL.
***
***
***        APPEND INITIAL LINE TO tt_keys  ASSIGNING FIELD-SYMBOL(<fs_temp_key>).
***        <fs_temp_key>-act = CORRESPONDING #( <fs_act> ).
***        <fs_temp_key>-cid = lv_cid.
***        <fs_temp_key>-pid = VALUE #( ls_mapped-journalentry[ %cid = lv_cid ]-%pid OPTIONAL ).
***
***      ENDIF.
***
***    ENDLOOP.
***
***    IF tt_keys IS NOT INITIAL.
***      DATA(lo_buffer) = lcl_buffer=>get_instance( ).
***      lo_buffer->set_reverse(  tt_act = tt_keys  ).
***    ENDIF.
***
***    mapped-activities = CORRESPONDING #( activities ).


    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""



    """"""""""""""""""""""""""""""""""""""""""""""""""""""""

    LOOP AT activities ASSIGNING FIELD-SYMBOL(<fs_act>).
      CLEAR lt_jr.


      APPEND INITIAL LINE TO lt_jr ASSIGNING FIELD-SYMBOL(<jr>).

      <jr>-companycode = <fs_act>-bukrs.
      <jr>-fiscalyear  = <fs_act>-gjahr.
      <jr>-accountingdocument = <fs_act>-belnr.
      <jr>-%param = VALUE #(
      postingdate = <fs_act>-act_date
      reversalreason = '01'
      createdbyuser = sy-uname

      ).

      MODIFY ENTITIES OF i_journalentrytp
        ENTITY journalentry
        EXECUTE reverse FROM lt_jr
        FAILED DATA(ls_failed)
        REPORTED DATA(ls_reported)
        MAPPED DATA(ls_mapped).

      LOOP AT ls_reported-journalentry ASSIGNING FIELD-SYMBOL(<ls_reported_deep>).
        DATA(lv_result) = <ls_reported_deep>-%msg->if_message~get_text( ).

        APPEND VALUE #( bankcode = <fs_act>-bankcode
                        bukrs    = <fs_act>-bukrs
                        iban     = <fs_act>-iban
                        branch   = <fs_act>-branch
                        act_no   = <fs_act>-act_no
                        act_date = <fs_act>-act_date
                        act_time = <fs_act>-act_time
                        acccount_no = <fs_act>-acccount_no
               %msg = <ls_reported_deep>-%msg ) TO reported-activities.


      ENDLOOP.

      IF ls_mapped-journalentry IS NOT INITIAL.


        APPEND INITIAL LINE TO tt_keys  ASSIGNING FIELD-SYMBOL(<fs_temp_key>).
        <fs_temp_key>-act = CORRESPONDING #( <fs_act> ).
        <fs_temp_key>-cid = lv_cid.
        <fs_temp_key>-pid = VALUE #( ls_mapped-journalentry[ %cid = lv_cid ]-%pid OPTIONAL ).

      ENDIF.


    ENDLOOP.

    IF tt_keys IS NOT INITIAL.
      DATA(lo_buffer) = lcl_buffer=>get_instance( ).
      lo_buffer->set_reverse(  tt_act = tt_keys  ).
    ENDIF.
    mapped-activities = CORRESPONDING #( activities ).



  ENDMETHOD.

  METHOD reverseactivityself.
    DATA: lt_jr TYPE TABLE FOR ACTION IMPORT i_journalentrytp~Reverse.
    DATA : lv_cid     TYPE abp_behv_cid.
    DATA : tt_keys        TYPE zeho_activity_if~tty_act_with_key,
           lt_je_validate TYPE TABLE FOR FUNCTION IMPORT i_journalentrytp~validate,
           ls_je_validate TYPE STRUCTURE FOR FUNCTION IMPORT i_journalentrytp~validate.
    DATA ls_aa TYPE zeho_i_activities.
    DATA : tt_bapidata_rev TYPE zeho_activity_if~tty_bapi_data_reverse.
    DATA  tt_activities TYPE zeho_tt_activities.

    CLEAR : tt_bapidata_rev , tt_keys.
    READ ENTITIES OF zeho_i_activities IN LOCAL MODE
    ENTITY Activities
    ALL FIELDS WITH
    CORRESPONDING #( keys )
    RESULT DATA(activities)
    FAILED failed.

    tt_activities = CORRESPONDING #( activities ).

    """"""""""""""""""""""""""""""""""""""""""""""""""""""""
***    LOOP AT activities ASSIGNING FIELD-SYMBOL(<fs_act>).
***      CLEAR lt_jr.
***      ls_aa = CORRESPONDING #( <fs_act> ).
***      APPEND INITIAL LINE TO lt_jr ASSIGNING FIELD-SYMBOL(<jr>).
***      <jr>-companycode = <fs_act>-bukrs.
***      <jr>-fiscalyear  = <fs_act>-gjahr.
***      <jr>-accountingdocument = <fs_act>-belnr.
***      <jr>-%param = VALUE #(
***      postingdate = <fs_act>-act_date
***      reversalreason = '01'
***      createdbyuser = sy-uname
***      ).
***
***
***      lt_je_validate = CORRESPONDING #( lt_jr ).
***      READ ENTITIES OF i_journalentrytp
***      ENTITY journalentry
***      EXECUTE validate FROM lt_je_validate
***      RESULT DATA(lt_check_result)
***      FAILED DATA(ls_failed_deep)
***      REPORTED DATA(ls_reported_deep).
***
***
***      IF ls_failed_deep IS NOT INITIAL.
***
***        LOOP AT ls_reported_deep-journalentry ASSIGNING FIELD-SYMBOL(<ls_reported_deep>).
***          DATA(lv_result) = <ls_reported_deep>-%msg->if_message~get_text( ).
***
***          APPEND VALUE #(
***
***                 bankcode = ls_aa-bankcode
***                 bukrs    = ls_aa-bukrs
***                 iban     = ls_aa-iban
***                 branch   = ls_aa-branch
***                 act_no   = ls_aa-act_no
***                 act_date = ls_aa-act_date
***                 act_time = ls_aa-act_time
***                 acccount_no = ls_aa-acccount_no
***
***              ) TO failed-activities.
***
***        ENDLOOP.
***      ELSE.
***
***        APPEND VALUE #(
***
***           bankcode = ls_aa-bankcode
***           bukrs    = ls_aa-bukrs
***           iban     = ls_aa-iban
***           branch   = ls_aa-branch
***           act_no   = ls_aa-act_no
***           act_date = ls_aa-act_date
***           act_time = ls_aa-act_time
***           acccount_no = ls_aa-acccount_no
***           belnr       = ls_aa-belnr
***           gjahr       = ls_aa-gjahr
***           bapi_data   = CORRESPONDING #( lt_jr )
***        ) TO tt_bapidata_rev.
***
***      ENDIF.
***
***
***
***    ENDLOOP.
***
***    LOOP  AT tt_bapidata_rev ASSIGNING FIELD-SYMBOL(<fs_rev>).
***
***      MODIFY ENTITIES OF i_journalentrytp
***      ENTITY journalentry
***      EXECUTE reverse FROM <fs_rev>-bapi_data
***      FAILED DATA(ls_failed)
***      REPORTED DATA(ls_reported)
***      MAPPED DATA(ls_mapped).
***
***
***      IF ls_mapped-journalentry IS NOT INITIAL.
***
***
***        APPEND INITIAL LINE TO tt_keys  ASSIGNING FIELD-SYMBOL(<fs_temp_key>).
***        <fs_temp_key>-act = CORRESPONDING #( <fs_act> ).
***        <fs_temp_key>-cid = lv_cid.
***        <fs_temp_key>-pid = VALUE #( ls_mapped-journalentry[ %cid = lv_cid ]-%pid OPTIONAL ).
***
***      ENDIF.
***
***    ENDLOOP.
***
***    IF tt_keys IS NOT INITIAL.
***      DATA(lo_buffer) = lcl_buffer=>get_instance( ).
***      lo_buffer->set_reverse(  tt_act = tt_keys  ).
***    ENDIF.



    LOOP AT activities ASSIGNING FIELD-SYMBOL(<fs_act>).
      CLEAR lt_jr.


      APPEND INITIAL LINE TO lt_jr ASSIGNING FIELD-SYMBOL(<jr>).

      <jr>-companycode = <fs_act>-bukrs.
      <jr>-fiscalyear  = <fs_act>-gjahr.
      <jr>-accountingdocument = <fs_act>-belnr.
      <jr>-%param = VALUE #(
      postingdate = <fs_act>-act_date
      reversalreason = '01'
      createdbyuser = sy-uname

      ).

      MODIFY ENTITIES OF i_journalentrytp
        ENTITY journalentry
        EXECUTE reverse FROM lt_jr
        FAILED DATA(ls_failed)
        REPORTED DATA(ls_reported)
        MAPPED DATA(ls_mapped).

      LOOP AT ls_reported-journalentry ASSIGNING FIELD-SYMBOL(<ls_reported_deep>).
        DATA(lv_result) = <ls_reported_deep>-%msg->if_message~get_text( ).

        APPEND VALUE #( bankcode = <fs_act>-bankcode
                        bukrs    = <fs_act>-bukrs
                        iban     = <fs_act>-iban
                        branch   = <fs_act>-branch
                        act_no   = <fs_act>-act_no
                        act_date = <fs_act>-act_date
                        act_time = <fs_act>-act_time
                        acccount_no = <fs_act>-acccount_no
               %msg = <ls_reported_deep>-%msg ) TO reported-activities.


      ENDLOOP.

      IF ls_mapped-journalentry IS NOT INITIAL.


        APPEND INITIAL LINE TO tt_keys  ASSIGNING FIELD-SYMBOL(<fs_temp_key>).
        <fs_temp_key>-act = CORRESPONDING #( <fs_act> ).
        <fs_temp_key>-cid = lv_cid.
        <fs_temp_key>-pid = VALUE #( ls_mapped-journalentry[ %cid = lv_cid ]-%pid OPTIONAL ).

      ENDIF.


    ENDLOOP.


    IF tt_keys IS NOT INITIAL.
      DATA(lo_buffer) = lcl_buffer=>get_instance( ).
      lo_buffer->set_reverse(  tt_act = tt_keys  ).
      lo_buffer->set_reread(  tt_act = tt_activities ).
    ENDIF.

    result = VALUE #( FOR activity IN activities
(
                      bankcode = activity-bankcode
                      bukrs    = activity-bukrs
                      iban     = activity-iban
                      branch   = activity-branch
                      acccount_no = activity-acccount_no
                      act_date = activity-act_date
                      act_time = activity-act_time
                      act_no   = activity-act_no
                      %param   = activity

                       )
                                     ).

  ENDMETHOD.

***  METHOD saveActivity.
***
***    DATA: lt_je_deep TYPE TABLE FOR ACTION IMPORT i_journalentrytp~post,
***          lv_cid     TYPE abp_behv_cid,
***          ls_je      TYPE STRUCTURE FOR ACTION IMPORT i_journalentrytp~post.
****          lt_gl      TYPE TABLE FOR ACTION IMPORT  i_journalentrytp~Post-
****          param      TYPE STRUCTURE FOR HIERARCHY d_journalentrypostparameter.
***    DATA : cl_document TYPE REF TO zeho_cl_document_processing.
***
***
***    READ ENTITIES OF zeho_i_activities IN LOCAL MODE
***    ENTITY Activities
***    ALL FIELDS WITH
***    CORRESPONDING #( keys )
***    RESULT DATA(activities)
***    FAILED failed.
***
***
***    DATA(lo_buffer) = lcl_buffer=>get_instance( ).
***
***
***
***
***    DATA tt_act TYPE  zeho_tt_activities.
***    tt_act = CORRESPONDING #(  activities  ).
***    lo_buffer->set_post( tt_act ).
***
***    mapped-activities = CORRESPONDING #( activities ).
***
***  ENDMETHOD.

  METHOD get_instance_features.
    READ ENTITIES OF zeho_i_activities IN LOCAL MODE
  ENTITY Activities
  ALL FIELDS WITH
   CORRESPONDING #( keys )
  RESULT DATA(activities)
  FAILED failed.

    result = VALUE #( FOR activity IN activities
                   ( bankcode                   = activity-bankcode
                     bukrs                      = activity-bukrs
                     iban                       = activity-iban
                     branch                     = activity-branch
                     acccount_no                = activity-acccount_no
                     act_date                   = activity-act_date
                     act_time                   = activity-act_time
                     act_no                     = activity-act_no

                     %features-%update      = COND #( WHEN  activity-belnr <> ' '
                                                     THEN if_abap_behv=>fc-o-disabled
*                                                     WHEN activity-cancel_process <> abap_false
*                                                     THEN if_abap_behv=>fc-o-disabled
                                                      ELSE if_abap_behv=>fc-o-enabled   )
                     %action-saveActivity           = COND #( WHEN  activity-belnr <> ' '
                                                     THEN if_abap_behv=>fc-o-disabled
                                                       WHEN activity-cancel_process <> abap_false
                                                     THEN if_abap_behv=>fc-o-disabled
                                                      ELSE if_abap_behv=>fc-o-enabled   )
                   %action-saveActivitySelf           = COND #( WHEN  activity-belnr <> ' '
                                                     THEN if_abap_behv=>fc-o-disabled
                                                       WHEN activity-cancel_process <> abap_false
                                                     THEN if_abap_behv=>fc-o-disabled
                                                      ELSE if_abap_behv=>fc-o-enabled   )
                     %action-multipleSave           = COND #( WHEN  activity-belnr <> ' '
                                                     THEN if_abap_behv=>fc-o-disabled
                                                       WHEN activity-cancel_process <> abap_false
                                                     THEN if_abap_behv=>fc-o-disabled
                                                     ELSE if_abap_behv=>fc-o-enabled   )
                     %action-reverseActivity        = COND #( WHEN  activity-belnr <> ' '
                                                     THEN if_abap_behv=>fc-o-enabled
                                                       WHEN activity-cancel_process = abap_false
                                                     THEN if_abap_behv=>fc-o-disabled
                                                     ELSE if_abap_behv=>fc-o-disabled   )
                     %action-reverseActivitySelf        = COND #( WHEN  activity-belnr <> ' '
                                                     THEN if_abap_behv=>fc-o-enabled
                                                       WHEN activity-cancel_process = abap_false
                                                     THEN if_abap_behv=>fc-o-disabled
                                                     ELSE if_abap_behv=>fc-o-disabled   )
                     %action-checkMaintenance      = COND #( WHEN  activity-belnr = ' ' AND  activity-cancel_process = abap_true
                                                       THEN if_abap_behv=>fc-o-disabled
                                                       WHEN  activity-belnr <> ' ' AND  activity-cancel_process = abap_false
                                                       THEN  if_abap_behv=>fc-o-disabled
*                                                       WHEN activity-cancel_process <> abap_false
*                                                      THEN if_abap_behv=>fc-o-disabled
                                                      ELSE if_abap_behv=>fc-o-enabled   )
                  %action-checkMaintenanceSelf      = COND #( WHEN  activity-belnr = ' ' AND  activity-cancel_process = abap_true
                                                     THEN if_abap_behv=>fc-o-disabled
                                                     WHEN  activity-belnr <> ' ' AND  activity-cancel_process = abap_false
                                                     THEN  if_abap_behv=>fc-o-disabled
*                                                     WHEN activity-cancel_process <> abap_false
*                                                    THEN if_abap_behv=>fc-o-disabled
                                                    ELSE if_abap_behv=>fc-o-enabled   )
                 ) ).

  ENDMETHOD.

  METHOD update.
    DATA lt_log TYPE TABLE OF zeho_a_log.

    lt_log = VALUE #( FOR entity IN entities
                        ( CORRESPONDING #( entity MAPPING client = DEFAULT sy-mandt ) )
                         ).

    MODIFY zeho_a_log FROM TABLE @lt_log.


  ENDMETHOD.

  METHOD checkMaintenance.
    DATA rd_activity TYPE REF TO zeho_s_activity.
    DATA : t_actt        TYPE  zeho_tt_actt,
           t_exp         TYPE  zeho_tt_exp,
           t_cust        TYPE zeho_tt_customer,
           t_supl        TYPE zeho_tt_supplier,
           t_acc         TYPE zeho_tt_acc,
           tt_activities TYPE zeho_tt_activities.
    DATA : tt_bukrs_range TYPE zeho_tt_bukrs_range,
           lv_where       TYPE string.
    DATA : tt_aa_cust TYPE  zeho_tt_act_custom.
    DATA : lt_log TYPE TABLE OF zeho_a_log.

    READ ENTITIES OF zeho_i_activities IN LOCAL MODE
    ENTITY Activities
    ALL FIELDS WITH
    CORRESPONDING #( keys )
    RESULT DATA(activities)
    FAILED failed.

    DATA(lt_act) =  activities[]. "CORRESPONDING zeho_i( activities MAPPING FROM ENTITY ).
    SORT lt_act BY bukrs.
    DELETE ADJACENT DUPLICATES FROM lt_act COMPARING bukrs.

    tt_bukrs_range = VALUE #( FOR comp IN lt_act
                             ( sign = 'I' option = 'EQ' low = comp-bukrs high = comp-bukrs )
                               ).

    lv_where = zeho_cl_seltab=>combine_seltabs(
                       it_named_seltabs = VALUE #(
                     (    name = 'BUKRS'    dref = REF #(  tt_bukrs_range    ) )
                      )
                     ).

    zeho_cl_amdp=>get_partners(
           EXPORTING
             i_where     = lv_where
           IMPORTING
             et_customer = t_cust
             et_supplier = t_supl
         ).

    SELECT DISTINCT *
     FROM zeho_a_acc AS acc
    INNER JOIN @activities AS data
       ON data~bankcode = acc~bankcode
      AND data~bukrs    = acc~bukrs
    INTO CORRESPONDING FIELDS OF TABLE @t_acc.

    CLEAR t_exp[].
    SELECT exp~*
    FROM ZEHO_a_exp  AS exp
    INNER JOIN @activities AS data
             ON data~bankcode = exp~bankcode
            AND data~bukrs    = exp~bukrs
           INTO CORRESPONDING FIELDS OF TABLE  @t_exp.


    CLEAR t_actt[].
    SELECT actt~* FROM ZEHO_a_actt  AS actt
    INNER JOIN @activities AS data
             ON data~bankcode = actt~bankcode
            AND data~bukrs    = actt~bukrs
           INTO CORRESPONDING FIELDS OF TABLE  @t_actt.

    tt_activities = CORRESPONDING #( activities ).

    LOOP AT tt_activities REFERENCE INTO rd_activity   WHERE belnr = ' '.
*                                                         AND cancel_process = ' '.

      zeho_cl_document_processing=>fill_list(
        EXPORTING
          tt_exp   = t_exp
          tt_acct  = t_actt
          tt_cust  = t_cust
          tt_suppl = t_supl
          tt_account   = t_acc
        CHANGING
          rd_aa    = rd_activity
      ).

    ENDLOOP.

    tt_aa_cust = VALUE #( FOR ls_wa IN  tt_activities
                         WHERE ( belnr = ' ' )
                          ( CORRESPONDING #( ls_wa MAPPING client = DEFAULT sy-mandt )  )
                        ).
    lt_log = VALUE #( FOR ls_wa IN  tt_activities
                         WHERE ( belnr = ' ' )
                          ( CORRESPONDING #( ls_wa MAPPING client = DEFAULT sy-mandt )  )
                        ).

    MODIFY zeho_a_actcust FROM TABLE @tt_aa_cust.
    MODIFY zeho_a_log FROM TABLE @lt_log.

*    MODIFY ENTITIES OF zeho_i_activities IN LOCAL MODE
*    ENTITY Activities
*    UPDATE FIELDS ( hkont
*                    belnr
*                    gjahr
*                    blart
*                    lifnr
*                    kunnr
*                    name1
*                    secondgl_acc
*                    kostl
*                    prctr
*                    gsber
*                    umskz
*                    mwskz
*                    cancel_process
*                    customization_type
*                    affacted_priority
*                    local_amount
*                    )
*   WITH VALUE #( FOR activity IN tt_activities
*                    ( CORRESPONDING #( activity ) )
*                     ).
*    mapped-activities = VALUE #( for act in tt_activities
*                        ( CORRESPONDING #( act ) )
*                ).


  ENDMETHOD.


  METHOD saveActivity.

    DATA:
      lt_je_validate TYPE TABLE FOR FUNCTION IMPORT i_journalentrytp~validate,
      ls_je_validate TYPE STRUCTURE FOR FUNCTION IMPORT i_journalentrytp~validate,
      lt_je_deep     TYPE TABLE FOR ACTION IMPORT i_journalentrytp~Post,
      lv_cid         TYPE abp_behv_cid,
      ls_je          TYPE STRUCTURE FOR ACTION IMPORT  i_journalentrytp~Post.
*          lt_gl      TYPE TABLE FOR ACTION IMPORT  i_journalentrytp~Post-
*          param      TYPE STRUCTURE FOR HIERARCHY d_journalentrypostparameter.
    DATA : cl_document TYPE REF TO zeho_cl_document_processing.
    DATA tt_bapidata TYPE  zeho_activity_if~tty_bapi_data.

    READ ENTITIES OF zeho_i_activities IN LOCAL MODE
    ENTITY Activities
    ALL FIELDS WITH
    CORRESPONDING #( keys )
    RESULT DATA(activities)
    FAILED failed.


    DATA(lo_buffer) = lcl_buffer=>get_instance( ).
    DATA tt_act TYPE  zeho_tt_activities.
    tt_act = CORRESPONDING #(  activities  ).

    """"""""""""""""""""""""""""""""""""""""""""""""""""""
*         DATA: lt_je_deep TYPE TABLE FOR ACTION IMPORT i_journalentrytp~post,
*          lv_cid     TYPE abp_behv_cid,
*          ls_je      TYPE STRUCTURE FOR ACTION IMPORT i_journalentrytp~post.
    CLEAR:  lt_je_deep , ls_je , tt_bapidata.


*    DATA : cl_document TYPE REF TO zeho_cl_document_processing.


    IF cl_document IS NOT BOUND.
      CREATE OBJECT cl_document.
    ENDIF.

    DATA ls_aa TYPE zeho_i_activities.

    LOOP AT tt_act ASSIGNING FIELD-SYMBOL(<fs_aa>).
      CLEAR  : lt_je_deep , lt_je_validate , ls_je , ls_je_validate.
      TRY.
          lv_cid = to_upper( cl_uuid_factory=>create_system_uuid( )->create_uuid_x16( ) ).
        CATCH cx_uuid_error.
          ASSERT 1 = 0.
      ENDTRY.
      ls_je-%cid = lv_cid.

      ls_aa = CORRESPONDING #( <fs_aa> ).

      cl_document->fill_header(
        EXPORTING
          rd_aa = ls_aa
        CHANGING
          e_je  = ls_je
      ).

      cl_document->fill_gl_account(
        EXPORTING

          rd_aa     = ls_aa
          e_dc      = COND #( WHEN  ls_aa-shkzg = 'H'
                      THEN 'S' ELSE 'H'  )
          e_hkont   = <fs_aa>-hkont
        CHANGING
          tt_glitems = ls_je-%param-_glitems
      ).

      IF <fs_aa>-secondgl_acc IS NOT INITIAL.


        cl_document->fill_secondgl_account(
        EXPORTING
          rd_aa     = ls_aa
          e_dc      = COND #( WHEN  ls_aa-shkzg = 'H'
                      THEN 'S' ELSE 'H'  )
          e_hkont   = <fs_aa>-secondgl_acc
        CHANGING
          tt_glitems = ls_je-%param-_glitems
            ).
      ELSEIF <fs_aa>-lifnr IS NOT INITIAL.

        cl_document->fill_supplier(
          EXPORTING
            rd_aa      = ls_aa
            e_dc      = COND #( WHEN  ls_aa-shkzg = 'H'
                      THEN 'S' ELSE 'H'  )
          CHANGING
            tt_apitems = ls_je-%param-_apitems
        ).
      ELSEIF <fs_aa>-kunnr IS NOT INITIAL.
        cl_document->fill_customer(
          EXPORTING
            rd_aa      = ls_aa
            e_dc      = COND #( WHEN  ls_aa-shkzg = 'H'
                         THEN 'S' ELSE 'H'  )
          CHANGING
            tt_aritems =  ls_je-%param-_aritems
        ).
      ENDIF.

      APPEND ls_je TO lt_je_deep.

      lt_je_validate = CORRESPONDING #( lt_je_deep ).

      READ ENTITIES OF i_journalentrytp
      ENTITY journalentry
      EXECUTE validate FROM lt_je_validate
      RESULT DATA(lt_check_result)
      FAILED DATA(ls_failed_deep)
      REPORTED DATA(ls_reported_deep).


      IF ls_failed_deep IS NOT INITIAL.
*        APPEND INITIAL LINE TO tt_keys ASSIGNING FIELD-SYMBOL(<fs_temp_key>).
*        <fs_temp_key>-act = CORRESPONDING #( ls_aa ).
*        <fs_temp_key>-cid = lv_cid.
*        <fs_temp_key>-pid = VALUE #( ls_mapped_deep-journalentry[ %cid = lv_cid ]-%pid OPTIONAL ).
        LOOP AT ls_reported_deep-journalentry ASSIGNING FIELD-SYMBOL(<ls_reported_deep>).
          DATA(lv_result) = <ls_reported_deep>-%msg->if_message~get_text( ).

          APPEND VALUE #( bankcode = ls_aa-bankcode
                          bukrs    = ls_aa-bukrs
                          iban     = ls_aa-iban
                          branch   = ls_aa-branch
                          act_no   = ls_aa-act_no
                          act_date = ls_aa-act_date
                          act_time = ls_aa-act_time
                          acccount_no = ls_aa-acccount_no
                          %msg = <ls_reported_deep>-%msg ) TO reported-activities.

**          APPEND VALUE #(
**
**                 bankcode = ls_aa-bankcode
**                 bukrs    = ls_aa-bukrs
**                 iban     = ls_aa-iban
**                 branch   = ls_aa-branch
**                 act_no   = ls_aa-act_no
**                 act_date = ls_aa-act_date
**                 act_time = ls_aa-act_time
**                 acccount_no = ls_aa-acccount_no
**
**              ) TO failed-activities.


        ENDLOOP.
      ELSE.

        APPEND VALUE #(

           bankcode = ls_aa-bankcode
           bukrs    = ls_aa-bukrs
           iban     = ls_aa-iban
           branch   = ls_aa-branch
           act_no   = ls_aa-act_no
           act_date = ls_aa-act_date
           act_time = ls_aa-act_time
           acccount_no = ls_aa-acccount_no
           bapi_data   = CORRESPONDING #( lt_je_deep )
        ) TO tt_bapidata.

**        APPEND INITIAL LINE TO tt_keys  ASSIGNING FIELD-SYMBOL(<fs_temp_key>).
**        <fs_temp_key>-act = CORRESPONDING #( ls_aa ).
**        <fs_temp_key>-cid = lv_cid.
**        <fs_temp_key>-pid = VALUE #( ls_mapped_deep-journalentry[ %cid = lv_cid ]-%pid OPTIONAL ).

      ENDIF.


    ENDLOOP.



    """""""""""""""""""""""""""""""""""""""""""""""""""""

*    tt_act = CORRESPONDING #(  activities  ).
    CLEAR tt_act.
    LOOP AT activities ASSIGNING FIELD-SYMBOL(<fs_act>).

      IF NOT line_exists( failed-activities[

          bankcode = <fs_act>-bankcode
          bukrs    = <fs_act>-bukrs
          iban     = <fs_act>-iban
          branch   = <fs_act>-branch
          act_no   = <fs_act>-act_no
          act_date = <fs_act>-act_date
          act_time = <fs_act>-act_time
          acccount_no = <fs_act>-acccount_no
      ] ).
        tt_act = VALUE #(  BASE tt_act

                ( CORRESPONDING #( <fs_act> ) )
        ).



      ENDIF.

    ENDLOOP.

    lo_buffer->set_post( tt_act ).
    lo_buffer->set_bapi_data( tt_bapidata ).

    mapped-activities = CORRESPONDING #( activities ).


  ENDMETHOD.

  METHOD checkMaintenanceSelf.

    DATA rd_activity TYPE REF TO zeho_s_activity.
    DATA : t_actt        TYPE  zeho_tt_actt,
           t_exp         TYPE  zeho_tt_exp,
           t_cust        TYPE zeho_tt_customer,
           t_supl        TYPE zeho_tt_supplier,
           t_acc         TYPE zeho_tt_acc,
           tt_activities TYPE zeho_tt_activities.
    DATA : tt_bukrs_range TYPE zeho_tt_bukrs_range,
           lv_where       TYPE string.
    DATA : tt_aa_cust TYPE  zeho_tt_act_custom.
    DATA : lt_log TYPE TABLE OF zeho_a_log.

    READ ENTITIES OF zeho_i_activities IN LOCAL MODE
    ENTITY Activities
    ALL FIELDS WITH
    CORRESPONDING #( keys )
    RESULT DATA(activities)
    FAILED failed.

    DATA(lt_act) =  activities[]. "CORRESPONDING zeho_i( activities MAPPING FROM ENTITY ).
    SORT lt_act BY bukrs.
    DELETE ADJACENT DUPLICATES FROM lt_act COMPARING bukrs.

    tt_bukrs_range = VALUE #( FOR comp IN lt_act
                             ( sign = 'I' option = 'EQ' low = comp-bukrs high = comp-bukrs )
                               ).

    lv_where = zeho_cl_seltab=>combine_seltabs(
                       it_named_seltabs = VALUE #(
                     (    name = 'BUKRS'    dref = REF #(  tt_bukrs_range    ) )
                      )
                     ).

    zeho_cl_amdp=>get_partners(
           EXPORTING
             i_where     = lv_where
           IMPORTING
             et_customer = t_cust
             et_supplier = t_supl
         ).

    SELECT DISTINCT *
     FROM zeho_a_acc AS acc
    INNER JOIN @activities AS data
       ON data~bankcode = acc~bankcode
      AND data~bukrs    = acc~bukrs
    INTO CORRESPONDING FIELDS OF TABLE @t_acc.

    CLEAR t_exp[].
    SELECT exp~*
    FROM ZEHO_a_exp  AS exp
    INNER JOIN @activities AS data
             ON data~bankcode = exp~bankcode
            AND data~bukrs    = exp~bukrs
           INTO CORRESPONDING FIELDS OF TABLE  @t_exp.


    CLEAR t_actt[].
    SELECT actt~* FROM ZEHO_a_actt  AS actt
    INNER JOIN @activities AS data
             ON data~bankcode = actt~bankcode
            AND data~bukrs    = actt~bukrs
           INTO CORRESPONDING FIELDS OF TABLE  @t_actt.

    tt_activities = CORRESPONDING #( activities ).

    LOOP AT tt_activities REFERENCE INTO rd_activity   WHERE belnr = ' '.
*                                                         AND cancel_process = ' '.

      zeho_cl_document_processing=>fill_list(
        EXPORTING
          tt_exp   = t_exp
          tt_acct  = t_actt
          tt_cust  = t_cust
          tt_suppl = t_supl
          tt_account   = t_acc
        CHANGING
          rd_aa    = rd_activity
      ).

    ENDLOOP.

    tt_aa_cust = VALUE #( FOR ls_wa IN  tt_activities
                         WHERE ( belnr = ' ' )
                          ( CORRESPONDING #( ls_wa MAPPING client = DEFAULT sy-mandt )  )
                        ).
    lt_log = VALUE #( FOR ls_wa IN  tt_activities
                         WHERE ( belnr = ' ' )
                          ( CORRESPONDING #( ls_wa MAPPING client = DEFAULT sy-mandt )  )
                        ).

    MODIFY zeho_a_actcust FROM TABLE @tt_aa_cust.
    MODIFY zeho_a_log FROM TABLE @lt_log.

    DATA(lo_buffer) = lcl_buffer=>get_instance( ).
    lo_buffer->set_reread(  tt_act = tt_activities ).


    READ ENTITIES OF zeho_i_activities IN LOCAL MODE
ENTITY Activities
ALL FIELDS WITH
CORRESPONDING #( keys )
RESULT DATA(activities_updated)
FAILED failed.

    result = VALUE #( FOR activit_updated IN activities_updated
    (
                             bankcode = activit_updated-bankcode
                             bukrs    = activit_updated-bukrs
                             iban     = activit_updated-iban
                             branch   = activit_updated-branch
                             acccount_no = activit_updated-acccount_no
                             act_date = activit_updated-act_date
                             act_time = activit_updated-act_time
                             act_no   = activit_updated-act_no
                             %param   = activit_updated

                              )
                                            ).

  ENDMETHOD.

  METHOD saveActivitySelf.

    DATA:
      lt_je_validate TYPE TABLE FOR FUNCTION IMPORT i_journalentrytp~validate,
      ls_je_validate TYPE STRUCTURE FOR FUNCTION IMPORT i_journalentrytp~validate,
      lt_je_deep     TYPE TABLE FOR ACTION IMPORT i_journalentrytp~Post,
      lv_cid         TYPE abp_behv_cid,
      ls_je          TYPE STRUCTURE FOR ACTION IMPORT  i_journalentrytp~Post.
*          lt_gl      TYPE TABLE FOR ACTION IMPORT  i_journalentrytp~Post-
*          param      TYPE STRUCTURE FOR HIERARCHY d_journalentrypostparameter.
    DATA : cl_document TYPE REF TO zeho_cl_document_processing.
    DATA tt_bapidata TYPE  zeho_activity_if~tty_bapi_data.


    READ ENTITIES OF zeho_i_activities IN LOCAL MODE
    ENTITY Activities
    ALL FIELDS WITH
    CORRESPONDING #( keys )
    RESULT DATA(activities)
    FAILED failed.


    DATA(lo_buffer) = lcl_buffer=>get_instance( ).
    DATA tt_act TYPE  zeho_tt_activities.
    tt_act = CORRESPONDING #(  activities  ).

    """"""""""""""""""""""""""""""""""""""""""""""""""""""
*         DATA: lt_je_deep TYPE TABLE FOR ACTION IMPORT i_journalentrytp~post,
*          lv_cid     TYPE abp_behv_cid,
*          ls_je      TYPE STRUCTURE FOR ACTION IMPORT i_journalentrytp~post.
    CLEAR:  lt_je_deep , ls_je , tt_bapidata.


*    DATA : cl_document TYPE REF TO zeho_cl_document_processing.


    IF cl_document IS NOT BOUND.
      CREATE OBJECT cl_document.
    ENDIF.

    DATA ls_aa TYPE zeho_i_activities.

    LOOP AT tt_act ASSIGNING FIELD-SYMBOL(<fs_aa>).
      CLEAR  : lt_je_deep , lt_je_validate , ls_je , ls_je_validate.
      TRY.
          lv_cid = to_upper( cl_uuid_factory=>create_system_uuid( )->create_uuid_x16( ) ).
        CATCH cx_uuid_error.
          ASSERT 1 = 0.
      ENDTRY.
      ls_je-%cid = lv_cid.

      ls_aa = CORRESPONDING #( <fs_aa> ).

      cl_document->fill_header(
        EXPORTING
          rd_aa = ls_aa
        CHANGING
          e_je  = ls_je
      ).

      cl_document->fill_gl_account(
        EXPORTING

          rd_aa     = ls_aa
          e_dc      = COND #( WHEN  ls_aa-shkzg = 'H'
                      THEN 'S' ELSE 'H'  )
          e_hkont   = <fs_aa>-hkont
        CHANGING
          tt_glitems = ls_je-%param-_glitems
      ).

      IF <fs_aa>-secondgl_acc IS NOT INITIAL.


        cl_document->fill_secondgl_account(
        EXPORTING
          rd_aa     = ls_aa
          e_dc      = COND #( WHEN  ls_aa-shkzg = 'H'
                      THEN 'S' ELSE 'H'  )
          e_hkont   = <fs_aa>-secondgl_acc
        CHANGING
          tt_glitems = ls_je-%param-_glitems
            ).
      ELSEIF <fs_aa>-lifnr IS NOT INITIAL.

        cl_document->fill_supplier(
          EXPORTING
            rd_aa      = ls_aa
            e_dc      = COND #( WHEN  ls_aa-shkzg = 'H'
                      THEN 'S' ELSE 'H'  )
          CHANGING
            tt_apitems = ls_je-%param-_apitems
        ).
      ELSEIF <fs_aa>-kunnr IS NOT INITIAL.
        cl_document->fill_customer(
          EXPORTING
            rd_aa      = ls_aa
            e_dc      = COND #( WHEN  ls_aa-shkzg = 'H'
                         THEN 'S' ELSE 'H'  )
          CHANGING
            tt_aritems =  ls_je-%param-_aritems
        ).
      ENDIF.

      APPEND ls_je TO lt_je_deep.

      lt_je_validate = CORRESPONDING #( lt_je_deep ).

      READ ENTITIES OF i_journalentrytp
      ENTITY journalentry
      EXECUTE validate FROM lt_je_validate
      RESULT DATA(lt_check_result)
      FAILED DATA(ls_failed_deep)
      REPORTED DATA(ls_reported_deep).


      IF ls_failed_deep IS NOT INITIAL.
*        APPEND INITIAL LINE TO tt_keys ASSIGNING FIELD-SYMBOL(<fs_temp_key>).
*        <fs_temp_key>-act = CORRESPONDING #( ls_aa ).
*        <fs_temp_key>-cid = lv_cid.
*        <fs_temp_key>-pid = VALUE #( ls_mapped_deep-journalentry[ %cid = lv_cid ]-%pid OPTIONAL ).
        LOOP AT ls_reported_deep-journalentry ASSIGNING FIELD-SYMBOL(<ls_reported_deep>).
          DATA(lv_result) = <ls_reported_deep>-%msg->if_message~get_text( ).

          APPEND VALUE #( bankcode = ls_aa-bankcode
                          bukrs    = ls_aa-bukrs
                          iban     = ls_aa-iban
                          branch   = ls_aa-branch
                          act_no   = ls_aa-act_no
                          act_date = ls_aa-act_date
                          act_time = ls_aa-act_time
                          acccount_no = ls_aa-acccount_no
                          %msg = <ls_reported_deep>-%msg ) TO reported-activities.

*          APPEND VALUE #(
*
*                 bankcode = ls_aa-bankcode
*                 bukrs    = ls_aa-bukrs
*                 iban     = ls_aa-iban
*                 branch   = ls_aa-branch
*                 act_no   = ls_aa-act_no
*                 act_date = ls_aa-act_date
*                 act_time = ls_aa-act_time
*                 acccount_no = ls_aa-acccount_no
*
*              ) TO failed-activities.

        ENDLOOP.
      ELSE.

        APPEND VALUE #(

           bankcode = ls_aa-bankcode
           bukrs    = ls_aa-bukrs
           iban     = ls_aa-iban
           branch   = ls_aa-branch
           act_no   = ls_aa-act_no
           act_date = ls_aa-act_date
           act_time = ls_aa-act_time
           acccount_no = ls_aa-acccount_no
           bapi_data   = CORRESPONDING #( lt_je_deep )
        ) TO tt_bapidata.

**        APPEND INITIAL LINE TO tt_keys  ASSIGNING FIELD-SYMBOL(<fs_temp_key>).
**        <fs_temp_key>-act = CORRESPONDING #( ls_aa ).
**        <fs_temp_key>-cid = lv_cid.
**        <fs_temp_key>-pid = VALUE #( ls_mapped_deep-journalentry[ %cid = lv_cid ]-%pid OPTIONAL ).

      ENDIF.
    ENDLOOP.

*    tt_act = CORRESPONDING #(  activities  ).
    CLEAR tt_act.
    LOOP AT activities ASSIGNING FIELD-SYMBOL(<fs_act>).

      IF NOT line_exists( failed-activities[

          bankcode = <fs_act>-bankcode
          bukrs    = <fs_act>-bukrs
          iban     = <fs_act>-iban
          branch   = <fs_act>-branch
          act_no   = <fs_act>-act_no
          act_date = <fs_act>-act_date
          act_time = <fs_act>-act_time
          acccount_no = <fs_act>-acccount_no
      ] ).
        tt_act = VALUE #(  BASE tt_act

                ( CORRESPONDING #( <fs_act> ) )
        ).
      ENDIF.
    ENDLOOP.
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

*        CLEAR tt_act
*        tt_act = CORRESPONDING #(  activities  ).
    lo_buffer->set_post( tt_act ).


    lo_buffer->set_reread(  tt_act = tt_act ).
    lo_buffer->set_bapi_data( tt_bapidata ).
    result = VALUE #( FOR activity IN activities
 (
                          bankcode = activity-bankcode
                          bukrs    = activity-bukrs
                          iban     = activity-iban
                          branch   = activity-branch
                          acccount_no = activity-acccount_no
                          act_date = activity-act_date
                          act_time = activity-act_time
                          act_no   = activity-act_no
                          %param   = activity

                           )
                                         ).

  ENDMETHOD.

ENDCLASS.

CLASS lsc_ZEHO_I_ACTIVITIES DEFINITION INHERITING FROM cl_abap_behavior_saver.
  PUBLIC SECTION.
    INTERFACES zeho_activity_if.


*    TYPES : BEGIN OF ty_act_with_key,
*              act TYPE zeho_s_activity,
*              cid TYPE abp_behv_cid,
*              pid TYPE abp_behv_pid,
*            END OF ty_act_with_key,
*            tty_act_with_key TYPE STANDARD TABLE OF ty_act_with_key WITH DEFAULT KEY.
*    TYPES  : ty_je_post  TYPE TABLE FOR ACTION IMPORT i_journalentrytp~post.
*
*    TYPES : BEGIN OF ty_bapi_data,
*              bankcode    TYPE zeho_i_activities-bankcode,
*              bukrs       TYPE zeho_i_activities-bukrs,
*              act_date    TYPE zeho_i_activities-act_date,
*              act_time    TYPE zeho_i_activities-act_time,
*              act_no      TYPE zeho_i_activities-act_no,
*              iban        TYPE zeho_i_activities-iban,
*              branch      TYPE zeho_i_activities-branch,
*              acccount_no TYPE zeho_i_activities-acccount_no,
*              bapi_data   TYPE ty_je_post,
*            END OF ty_bapi_data.

    TYPES : tty_bapi_data TYPE TABLE OF zeho_activity_if~ty_bapi_data.


    DATA : tt_act_key TYPE zeho_activity_if~tty_act_with_key.


  PROTECTED SECTION.

    METHODS finalize REDEFINITION.

    METHODS check_before_save REDEFINITION.

    METHODS save REDEFINITION.

    METHODS cleanup REDEFINITION.

    METHODS cleanup_finalize REDEFINITION.

*    METHODS adjust_numbers REDEFINITION.

ENDCLASS.

CLASS lsc_ZEHO_I_ACTIVITIES IMPLEMENTATION.

  METHOD finalize.
    DATA lt_temp_key TYPE zeho_activity_if~tty_act_with_key.
    DATA : tt_act TYPE zeho_tt_activities.
    DATA(lo_buffer) = lcl_buffer=>get_instance( ).
    DATA lt_failed TYPE TABLE FOR FAILED zeho_i_activities.
    DATA lt_reported TYPE TABLE FOR REPORTED zeho_i_activities.
    DATA lt_bapidata TYPE tty_bapi_data.


    lo_buffer->get_post( IMPORTING tt_act = tt_act ).

    DATA: lt_je_deep TYPE TABLE FOR ACTION IMPORT i_journalentrytp~post,
          lv_cid     TYPE abp_behv_cid,
          ls_je      TYPE STRUCTURE FOR ACTION IMPORT i_journalentrytp~post.
*          lt_gl      TYPE TABLE FOR ACTION IMPORT  i_journalentrytp~Post-
*          param      TYPE STRUCTURE FOR HIERARCHY d_journalentrypostparameter.
    DATA : cl_document TYPE REF TO zeho_cl_document_processing.



    IF tt_act IS NOT INITIAL.
      lo_buffer->get_bapi_data( IMPORTING tt_bapi_data = lt_bapidata  ).
      IF lt_bapidata IS NOT INITIAL.
        CLEAR lt_temp_key.
        lo_buffer->post_doc( EXPORTING tt_act = tt_act
                                       tt_bapidata = lt_bapidata
                      IMPORTING tt_keys = lt_temp_key
                                tt_failed = lt_failed
                                tt_reported = lt_reported ).
        lo_buffer->set_keys(  tt_keys = lt_temp_key ).
        reported-activities =  CORRESPONDING #( lt_reported ).
        CLEAR:  lt_je_deep , ls_je.
      ENDIF.

    ENDIF.



  ENDMETHOD.

  METHOD check_before_save.
  ENDMETHOD.

  METHOD save.
    TYPES: BEGIN OF ty_temp_key,
             cid TYPE abp_behv_cid,
             pid TYPE abp_behv_pid,
           END OF ty_temp_key,
           tt_temp_key TYPE STANDARD TABLE OF ty_temp_key WITH DEFAULT KEY.
    DATA : lt_log TYPE TABLE OF zeho_a_log.
    DATA:     ltt_temp_key TYPE zeho_activity_if~tty_act_with_key.
    DATA:     lt_reverse_key TYPE zeho_activity_if~tty_act_with_key.
    DATA : ls_Temp_key LIKE LINE OF ltt_temp_key.
*    DATA : lt_cid TYPE TABLE OF zeho_a_doc_cid.

    CLEAR  : lt_log, ltt_temp_key , lt_reverse_key.
    DATA(lo_buffer) = lcl_buffer=>get_instance( ).
    lo_buffer->get_keys(  IMPORTING tt_keys = ltt_temp_key  ).
    lo_buffer->get_reverse( IMPORTING tt_act = lt_reverse_key ).

    IF ltt_temp_key IS NOT INITIAL.

      LOOP AT ltt_temp_key ASSIGNING FIELD-SYMBOL(<fs_key>).
        CONVERT KEY OF i_journalentrytp
      FROM <fs_key>-pid
      TO FINAL(lv_root_key).
        <fs_key>-act-belnr = lv_root_key-AccountingDocument.
        <fs_key>-act-bukrs = lv_root_key-CompanyCode.
        <fs_key>-act-gjahr = lv_root_key-FiscalYear.


**        reported-activities = VALUE #( (
**                            bankcode = <fs_key>-act-bankcode
**                            bukrs    = <fs_key>-act-bukrs
**                            iban     = <fs_key>-act-iban
**                            branch   = <fs_key>-act-branch
**                            act_no   = <fs_key>-act-act_no
**                            act_date = <fs_key>-act-act_date
**                            act_time = <fs_key>-act-act_time
**                            acccount_no = <fs_key>-act-acccount_no
**
**                           %msg  = new_message_with_text(
**                                     severity = if_abap_behv_message=>severity-success
**                                     text     =  'Belge olusturuldu'
**                                   ) ) ) .




        APPEND INITIAL LINE TO reported-activities ASSIGNING FIELD-SYMBOL(<fs_reported>).
        <fs_reported> = CORRESPONDING #( <fs_key>-act ).
        <fs_reported>-%tky = CORRESPONDING #( <fs_key>-act ).
*            <fs_reported>-%state_area = 'SAVE'.
        <fs_reported>-%msg =  NEW zeho_cl_messages(
                         textid   = zeho_cl_messages=>document_created
                         severity = if_abap_behv_message=>severity-success
                         mv_bukrs  = <fs_key>-act-bukrs
                         mv_gjahr  = <fs_key>-act-gjahr
                         mv_belnr  = <fs_key>-act-belnr ).


      ENDLOOP.

      lt_log = VALUE #( FOR key IN ltt_temp_key
                 ( CORRESPONDING #( key-act MAPPING client = DEFAULT sy-mandt )


                 )

           ).

*       lt_cid = VALUE #( FOR key IN ltt_temp_key
*                 ( belnr = key-act-belnr
*                   bukrs = key-act-bukrs
*                   gjahr = key-act-gjahr
*                   client = sy-mandt
*                 )
*
*           ).


      MODIFY zeho_A_log FROM TABLE @lt_log.
*      MODIFY zeho_a_doc_cid FROM TABLe @lt_cid.

    ENDIF.

    IF  lt_reverse_key IS NOT INITIAL.

      LOOP AT lt_reverse_key ASSIGNING <fs_key>.
        CONVERT KEY OF i_journalentrytp FROM <fs_key>-pid TO DATA(lv_key).



        APPEND INITIAL LINE TO reported-activities ASSIGNING <fs_reported>.
        <fs_reported> = CORRESPONDING #( <fs_key>-act ).
        <fs_reported>-%tky = CORRESPONDING #( <fs_key>-act ).
        <fs_reported>-%msg =  NEW zeho_cl_messages(
                         textid   = zeho_cl_messages=>document_created
                         severity = if_abap_behv_message=>severity-success
                         mv_bukrs  = lv_key-CompanyCode
                         mv_gjahr  = lv_key-FiscalYear
                         mv_belnr  = lv_key-AccountingDocument ).



      ENDLOOP.

      lt_log = VALUE #( FOR key IN lt_reverse_key
          ( CORRESPONDING #( key-act MAPPING client = DEFAULT sy-mandt EXCEPT belnr gjahr )
          )
        ).
      MODIFY zeho_A_log FROM TABLE @lt_log.

    ENDIF.



  ENDMETHOD.

  METHOD cleanup.
    DATA(lo_buffer) = lcl_buffer=>get_instance( ).
    lo_buffer->clear_cache( ).
  ENDMETHOD.

  METHOD cleanup_finalize.
  ENDMETHOD.




ENDCLASS.

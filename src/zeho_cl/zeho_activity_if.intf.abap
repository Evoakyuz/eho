INTERFACE zeho_activity_if
  PUBLIC .
  TYPES : BEGIN OF ts_message,
            bankcode TYPE zeho_i_activities-bankcode,
            bukrs    TYPE zeho_i_activities-bukrs,
            act_date TYPE zeho_i_activities-act_date,
            act_time TYPE zeho_i_activities-act_time,
            act_no   TYPE zeho_i_activities-act_no,
            iban     TYPE zeho_i_activities-iban,
            branch   TYPE zeho_i_activities-branch,
            symsg    TYPE symsg,
            fields   TYPE string_table,
          END OF ts_message,

          ty_activities      TYPE STANDARD TABLE OF zeho_i_activities,
          tt_activity_in     TYPE TABLE FOR READ IMPORT zeho_i_activities,
          tt_activity_out    TYPE TABLE FOR READ RESULT zeho_i_activities,
          tt_activity_failed TYPE TABLE FOR FAILED zeho_i_activities,
          tt_message         TYPE STANDARD TABLE OF ts_message.
  TYPES: lt_entyry  TYPE TABLE FOR ACTION IMPORT i_journalentrytp~post,
         ls_entry   TYPE LINE OF lt_entyry,
         lty_glitem TYPE LINE OF ls_entry-%param-_glitems.

  TYPES  : ty_je_post  TYPE TABLE FOR ACTION IMPORT i_journalentrytp~post.

  TYPES : BEGIN OF ty_bapi_data,
            bankcode    TYPE zeho_i_activities-bankcode,
            bukrs       TYPE zeho_i_activities-bukrs,
            act_date    TYPE zeho_i_activities-act_date,
            act_time    TYPE zeho_i_activities-act_time,
            act_no      TYPE zeho_i_activities-act_no,
            iban        TYPE zeho_i_activities-iban,
            branch      TYPE zeho_i_activities-branch,
            acccount_no TYPE zeho_i_activities-acccount_no,
            bapi_data   TYPE ty_je_post,
          END OF ty_bapi_data.

  TYPES : tty_bapi_data TYPE TABLE OF ty_bapi_data.


  TYPES  : ty_je_reverse  TYPE TABLE FOR ACTION IMPORT i_journalentrytp~Reverse.

  TYPES : BEGIN OF ty_bapi_data_Rev,
            bankcode    TYPE zeho_i_activities-bankcode,
            bukrs       TYPE zeho_i_activities-bukrs,
            act_date    TYPE zeho_i_activities-act_date,
            act_time    TYPE zeho_i_activities-act_time,
            act_no      TYPE zeho_i_activities-act_no,
            iban        TYPE zeho_i_activities-iban,
            branch      TYPE zeho_i_activities-branch,
            acccount_no TYPE zeho_i_activities-acccount_no,
            belnr       TYPE zeho_i_activities-belnr,
            gjahr       TYPE zeho_i_activities-gjahr,
            bapi_data   TYPE ty_je_reverse,
          END OF ty_bapi_data_Rev.

  TYPES : tty_bapi_data_reverse TYPE TABLE OF ty_bapi_data_Rev.



  TYPES: ty_failed   TYPE TABLE FOR FAILED zeho_i_activities,
         ty_reported TYPE TABLE FOR REPORTED zeho_i_activities,
         ty_result   TYPE TABLE FOR READ RESULT zeho_i_activities.

  TYPES : BEGIN OF ty_act_with_key,
            act TYPE zeho_s_activity,
            cid TYPE abp_behv_cid,
            pid TYPE abp_behv_pid,
          END OF ty_act_with_key,
          tty_act_with_key TYPE STANDARD TABLE OF ty_act_with_key WITH DEFAULT KEY.


ENDINTERFACE.

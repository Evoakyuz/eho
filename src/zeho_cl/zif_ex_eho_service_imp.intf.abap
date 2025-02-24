INTERFACE zif_ex_eho_service_imp
  PUBLIC .


  INTERFACES if_badi_interface .

  methods get_request
  IMPORTING
   !rd_acc TYPE ref to  zeho_s_acc
   CHANGING
    request TYPE string.

    methods CREATE_REQ_PROCESSING
    importing
      !rd_acc type ref to zeho_s_acc
      !INPUT_XML type zeho_tt_reqm
    changing
      !XML type STRING .

        methods SEND_REQ_PROCESSING
    importing
      !rd_acc   TYPE REF TO zeho_s_acc
      !INPUT_XML type zeho_tt_serv
      !ACCOUNTS type zeho_tt_acc
      !BEGDATE type DATUM
      !ENDDATE type DATUM
    changing
      !HTTP_CLIENT type ref to IF_WEB_HTTP_CLIENT
      !request     TYPe ref to IF_WEB_HTTP_REQUEST .


    methods GET_RES_PROCESSING
    importing
      !RD_ACC type ref to zeho_s_acc
    changing
      !XML_TABLE type ZEHO_TT_XML_TABLE
      !XRESPONSE type XSTRING.
*      !T_ACC type ZSOA_EHO_TT_AA .

      methods NON_UNIQUE_PARSING default ignore
    importing
      !ACCOUNTS_T type zeho_tt_acc
      !RD_ACC type ref to zeho_s_acc.
*    changing
*      !ACC_ACTIVITY_T type ZSOA_EHO_TT_AA .



ENDINTERFACE.

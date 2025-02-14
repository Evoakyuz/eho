CLASS lhc_Bankcode DEFINITION INHERITING FROM cl_abap_behavior_handler.
  PRIVATE SECTION.

    METHODS get_global_authorizations FOR GLOBAL AUTHORIZATION
      IMPORTING REQUEST requested_authorizations FOR Bankcode RESULT result.
    METHODS get_instance_features FOR INSTANCE FEATURES
      IMPORTING keys REQUEST requested_features FOR Bankcode RESULT result.

ENDCLASS.

CLASS lhc_Bankcode IMPLEMENTATION.

  METHOD get_global_authorizations.
  ENDMETHOD.

  METHOD get_instance_features.


  READ ENTITIES OF ZEHO_I_BANK IN LOCAL MODE
     ENTITY Bankcode
       FIELDS ( Bankcode )
       WITH CORRESPONDING #( keys )
     RESULT DATA(Banks)
     FAILED failed.

      result = VALUE #( FOR bank IN Banks
                      ( %tky                   = bank-%tky

*                        %features-%update      = COND #( WHEN travel-OverallStatus = travel_status-accepted
*                                                        THEN if_abap_behv=>fc-o-disabled ELSE if_abap_behv=>fc-o-enabled   )
*                        %features-%delete      = COND #( WHEN travel-OverallStatus = travel_status-open
*                                                        THEN if_abap_behv=>fc-o-enabled ELSE if_abap_behv=>fc-o-disabled   )
                        %action-Edit           =  if_abap_behv=>fc-o-disabled
*                        %action-acceptTravel   = COND #( WHEN travel-OverallStatus = travel_status-accepted
*                                                          THEN if_abap_behv=>fc-o-disabled ELSE if_abap_behv=>fc-o-enabled   )
*                        %action-rejectTravel   = COND #( WHEN travel-OverallStatus = travel_status-rejected
*                                                          THEN if_abap_behv=>fc-o-disabled ELSE if_abap_behv=>fc-o-enabled   )
*                        %action-deductDiscount = COND #( WHEN travel-OverallStatus = travel_status-open
*                                                          THEN if_abap_behv=>fc-o-enabled ELSE if_abap_behv=>fc-o-disabled   )
                    ) ).



  ENDMETHOD.

ENDCLASS.

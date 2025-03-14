@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Consumption View For ZEHO_I_ACTIVITIES'
@Metadata.ignorePropagatedAnnotations: true
@Metadata.allowExtensions: true
define root view entity ZEHO_C_ACTIVITIES
  provider contract transactional_query
  as projection on ZEHO_I_ACTIVITIES
{
  key bankcode,
  key bukrs,
  key iban,
  key branch,
  key acccount_no,
          key act_date,
          key act_time,
  key act_no,
//      act_date,
//      act_time,
      description,
      shkzg,
      @Semantics.amount.currencyCode : 'waers'
      amount,
      @Semantics.amount.currencyCode : 'waers'
      instant_amount,
      waers,
      activity_type,
      activity_explanation,
      sender_iban,
      sender_vkn,
      debited_vkn,
      sender_name,
      sender_bank,
      customer_ref,
      hkont,
      belnr,
      gjahr,
      blart,
      lifnr,
      kunnr,
      name1,
      secondgl_acc,
      kostl,
      prctr,
      gsber,
      umskz,
      mwskz,
      cancel_process,
      customization_type,
      affacted_priority,
      @Semantics.amount.currencyCode : 'waers'
      local_amount
}

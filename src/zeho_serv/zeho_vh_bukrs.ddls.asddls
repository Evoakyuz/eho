@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Company Code Value Help'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
    serviceQuality: #X,
    sizeCategory: #S,
    dataClass: #MIXED
}
@ObjectModel.resultSet.sizeCategory: #XS 

define view entity ZEHO_VH_BUKRS  
//with parameters bank : bankk
as select from zeho_a_comp
{
    // key cast(bankcode as bankk preserving type ) as Bankcode,
     key bukrs as Bukrs
}
//}where bankcode = $parameters.bank

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
as select from zeho_a_comp

{   
    @UI.hidden: true
     key cast(bankcode as bankk preserving type ) as Bankcode,

     key bukrs as Bukrs ,
     comp_desc
}




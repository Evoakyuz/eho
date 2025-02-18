@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'BankCode Value Help'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
    serviceQuality: #X,
    sizeCategory: #S,
    dataClass: #MIXED
}
@ObjectModel.resultSet.sizeCategory: #XS 

define view  entity  ZEHO_VH_REQ_FIELDS 
 as select  distinct from  zeho_a_bank
          
{   
    
//    key bankcode,
//    @ObjectModel.virtualElementCalculatedBy:    'ABAP:ZCL_EHO_AMDP'
    key bankcode,
    @ObjectModel.virtualElementCalculatedBy:    'ABAP:ZCL_EHO_AMDP'
    cast('' as abap.char(30) ) as fieldname
    
  
}

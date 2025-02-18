@EndUserText.label: 'table function for CDS Meta Data'
@ClientHandling.type: #CLIENT_DEPENDENT
@ClientHandling.algorithm: #SESSION_VARIABLE

define table function ZEHO_TF_REQ_FIELDNAME

returns {
   mandt : abap.clnt;
   fieldname : abap.char( 30 );
  
}
implemented by method zcl_eho_amdp=>get_fieldnames;
@ObjectModel.query.implementedBy: 'ABAP:ZCL_CUR_EXCH_RATE_PROV'
define custom entity ZCE_CUR_EXCH_RATE
{
  key fcurr : abap.cuky( 5 );
  key tcurr : waers;
//      @Semantics.amount.currencyCode : 'currency_code'
      ukurs : abap.decfloat16;
}

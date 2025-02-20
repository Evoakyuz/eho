@AccessControl.authorizationCheck: #NOT_ALLOWED
@EndUserText.label: 'Consumption View For ZEHO_I_ACCT'
@Metadata.ignorePropagatedAnnotations: true
@Metadata.allowExtensions: true
define root view entity ZEHO_C_ACCT
  provider contract transactional_query
  as projection on ZEHO_I_ACCT
{

      @Consumption.valueHelpDefinition: [{ entity: {
       name: 'ZEHO_VH_BANKCODES',
       element: 'Bankcode'
       }  }]
      @Consumption.filter.multipleSelections: false
      @Consumption.filter.selectionType: #SINGLE
  key Bankcode,
      @Consumption.valueHelpDefinition: [{ entity:
      {
      name: 'ZEHO_VH_BUKRS',
      element: 'Bukrs'
      },
      distinctValues: true,
      additionalBinding: [{ usage: #FILTER  ,element: 'Bankcode' , localElement: 'Bankcode'   }]
      }]
  key Bukrs,
  key ActivityType,
      @Consumption.valueHelpDefinition: [{ entity: {
      name: 'ZEHO_VH_PRIORITY',
      element: 'Priority'
      }   }]
  key Priority,
      @Consumption.valueHelpDefinition: [{ entity: {
      name: 'ZEHO_VH_DEBIT_CREDIT',
      element: 'dc'
      }   }]
      //     @Consumption.filter.defaultValue: 'All'
      //     @Consumption.defaultValue: 'A'
      //    @UI.defaultValue: 'All'

      Dc,
      @Consumption.valueHelpDefinition: [{ entity: {
        name: 'ZEHO_VH_BLART',
        element: 'AccountingDocumentType'
        }   }]
      Blart,
      VendorControl,
      CustomerControl,
      VirementControl,

      @Consumption.valueHelpDefinition: [{ entity: {
      name: 'I_OffsettingAccount',
      element: 'OffsettingAccount'
      }   }]
      SecondglAcc,
      @Consumption.valueHelpDefinition: [{ entity: {
       name: 'I_CostCenterStdVH',
       element: 'CostCenter'
       }   }]
      Kostl,
      @Consumption.valueHelpDefinition: [{ entity: {
      name: 'I_ProfitCenterStdVH',
      element: 'ProfitCenter'
      }   }]

      Prctr,
     
      @Consumption.valueHelpDefinition: [{ entity: {
      name: 'I_BusinessAreaStdVH',
      element: 'BusinessArea'
      }   }]
      Gsber,
      CancelProcess
}

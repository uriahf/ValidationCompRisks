const ManipulateConfusionMatrixInputTables = function(radiobutton) {
      
      console.log('Radio Button Input Confusion Matrix')
      console.log(radiobutton)
      
      const radioButtonValue = radiobutton.value;
      console.log('radioButtonValue')
      console.log(radioButtonValue)
      
      console.log('radiobutton.name')
      console.log(radiobutton.name)
      
      const inputTableID = 'confusionMatrixInput'
      
      console.log('inputTableID')
      console.log(inputTableID)
      
    const CompetingAssumptionValueExcluded = document.getElementById('competing_assumption_radiobutton_excluded').checked
    
    const CensoredAssumptionValueExcluded = document.getElementById('censored_assumption_radiobutton_excluded').checked

    
    console.log('CompetingAssumptionValueExcluded')
    console.log(CompetingAssumptionValueExcluded)


    console.log('CensoredAssumptionValueExcluded')
    console.log(CensoredAssumptionValueExcluded)
    
    let filtervalue;
    
    if ( CompetingAssumptionValueExcluded === true && CensoredAssumptionValueExcluded === true ) {
    
      filtervalue = 'all_excluded'
    
    } else if ( CompetingAssumptionValueExcluded === true && CensoredAssumptionValueExcluded === false ) {
    
      filtervalue = 'competing_excluded_censored_adjusted'
    
    } else if ( CompetingAssumptionValueExcluded === false && CensoredAssumptionValueExcluded === true ) {
    
      filtervalue = 'competing_nonevent_censored_excluded'
    
    } else if ( CompetingAssumptionValueExcluded === false && CensoredAssumptionValueExcluded === false ) {
    
      filtervalue = 'competing_nonevent_censored_adjusted'
    
    }
    
    console.log('filtervalue')
    console.log(filtervalue)
      
      
      const setMetaForInputTable = (tableID) => {
        Reactable.setMeta(tableID, (prevMeta) => {
        
        console.log('prevMeta')
        console.log(prevMeta.exclusion_assumptions)
        
            return prevMeta;
        });
    };
    
    
    //console.log('reactableInputTable')
    //console.log(reactableInputTable)
    

    // setMetaForInputTable(inputTableID);
    
    Reactable.setFilter(inputTableID, 'type', filtervalue) 
    
    }

const ManipulateStratifiedBy = function(radioButton) {
    
      console.log('radioButton')
      console.log(radioButton)
      
      const rangeFilterlabel = document.querySelector(`label[for=\"cutoff_slider\"]`);
      
      console.log('rangeFilterlabel')
      console.log(rangeFilterlabel)
      
      console.log('rangeFilterlabel.textContent')
      console.log(rangeFilterlabel.textContent)
      
      if (radioButton.value === 'probability_threshold') {
        
        rangeFilterlabel.textContent = 'Probability Threshold'
      
      } else {
      
        rangeFilterlabel.textContent = 'PPCR (Predicted Positives Condition Rate)'
      
      }
      
      console.log('rangeFilterlabel')
      console.log(rangeFilterlabel)
      
      console.log('rangeFilterlabel.textContent')
      console.log(rangeFilterlabel.textContent)
    
      Reactable.setFilter(
      'confusion_matrix_for_display', 
      'stratified_by', radioButton.value)
    
    }
    
const ManipulateReactablesByRadioButtons = function(radiobutton) {
    const radioButtonValue = radiobutton.value;
    const inputTableID = radiobutton.name.replace('condition_checkbox-', '');
    const outputTableID = radiobutton.name.replace('condition_checkbox-confusionMatrixInput', 'confusion_matrix_for_display');
    
    const highlightedMetrics = {
        'pp': { TP: true, TN: false, FP: true, FN: false, N: false, 
                predicted_positives: true,
                predicted_negatives: false,
                real_positives: false,
                censored_predicted_positives: true,
                total_included_predicted_positives: true,
                real_negatives: false, total_predicted_positives: true, competing_predicted_negatives: false, competing_predicted_positives: true},
        'pn': { TP: false, TN: true, FP: false, FN: true, N: false, 
                predicted_positives: false,
                predicted_negatives: true,
                real_positives: false,
                censored_predicted_negatives: true,
                total_included_predicted_negatives: true,
                real_negatives: false, total_predicted_negatives: true, competing_predicted_negatives: true, competing_predicted_positives: false },
        'ao': { TP: true, TN: true, FP: true, FN: true, N: true, 
                predicted_positives: true, total_predicted: true,
                predicted_negatives: true,
                total_included_predicted_negatives: true,
                total_included_predicted_positives: true,
                total_included: true,
                censored_predicted_negatives: true,
                censored_predicted_positives: true,
                real_positives: true, real_competing: true,
                real_censored: true,
                real_negatives: true, competing_predicted_negatives: true, competing_predicted_positives: true, total_real_negatives: true, total_real_positives: true, total_real_competing: true, total_predicted_positives: true, total_predicted_negatives: true, total_obs: true },
        'rp': { TP: true, TN: false, FP: false, FN: true, N: false, real_positives: true, competing_predicted_negatives: false, competing_predicted_positives: false, total_real_positives: true },
        'rn': { TP: false, TN: true, FP: true, FN: false, N: false, real_negatives: true, competing_predicted_negatives: false, competing_predicted_positives: false, total_real_negatives: true }
    };

    const setMetaForTable = (tableID) => {
        Reactable.setMeta(tableID, (prevMeta) => {
            return { highlightedMetrics: highlightedMetrics[radioButtonValue] };
        });
    };

    setMetaForTable(inputTableID);
    setMetaForTable(outputTableID);
}   
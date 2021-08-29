library(httr)
library(jsonlite)
library(tidyverse)
library(stringr)

# Get RXCUI for Given NDC -------------------------------------------------

'input of eleven-digit NDCs'
input <- data.frame( eleven_digit_ndc=c("00003089431"
                                        , "00173064255"
                                        , "00054472825"
                                        , "00003089431"
                                        , "39822050004"
                                        , "31722072131"
                                        , "50474000148"
                                        ,"adkjsakjsaj"))

'function that accepts NDCs and returns tall man synonym, boxed warning and patient information'
function_get_ndc_info <- function(eleven_digit_ndc) {

  'get RXCUI from NDC by making API call'
  get_rxcui_from_ndc <- GET(paste0("https://rxnav.nlm.nih.gov//REST/ndcproperties.json?id=", eleven_digit_ndc))

  
    
          'gets content of response of call'
          content_rxcui_from_ndc<- content(get_rxcui_from_ndc, "text")
    
          'parse JSON from content'
          json_rxcui_from_ndc <- fromJSON(content_rxcui_from_ndc, flatten = TRUE)
          'from JSON, return cell that represents RXCUI (RxNorm ID) for the NDC'
          rxcui_from_ndc <- json_rxcui_from_ndc[["ndcPropertyList"]][["ndcProperty"]][["rxcui"]]
         
          'if the RXCUI is not NULL'
        if (is.null( rxcui_from_ndc)==FALSE){    
          # Properties for RXCUI to Get tall man Synonym -------------------------------------
          
          'api call to get the RXCUI from the NDC'
          get_rxnorm_properties_from_rxcui <- GET(paste0("https://rxnav.nlm.nih.gov/REST/rxcui/", rxcui_from_ndc , "/allProperties?prop=names"))
          'extract the content from the response'
          content_rxnorm_properties_from_rxcui <- content (get_rxnorm_properties_from_rxcui, "text")
          'parse json'
          json_rxnorm_properties_from_rxcui <- fromJSON(content_rxnorm_properties_from_rxcui, flatten = TRUE)
          'make a data frame of the name - synonym table, which contains the tall man lettering info'
          df_name_synonym <- json_rxnorm_properties_from_rxcui$propConceptGroup$propConcept
          
          'if a tall man synonym exists then..'
          if ('Tallman Synonym' %in% df_name_synonym$propName) {
          'return the last listed value - it is possible to have more than 1 tall man synonym'
              med_name <- as.character( df_name_synonym %>% filter(propName=='Tallman Synonym') %>% slice(last(row_number())) %>% select(propValue))
              tallmanYN <- 'Y'
              tallman_list <-  list(med_display_name = med_name, tallmanYN= tallmanYN)
              }else{
            'if tallman synonym does not exist, use the RxNorm Name'
              med_name <- as.character( df_name_synonym %>% filter(propName=='RxNorm Name') %>% slice(last(row_number())) %>% select(propValue))
              tallmanYN <- 'N'
              tallman_list <- list(med_display_name = med_name, tallmanYN= tallmanYN)
              }
  
          # Get FDA Info for Given RXCUI --------------------------------------------
          'API call to get the FDA information using the RXCUI from above'
          get_fda_info_from_rxcui<- GET(paste0("https://api.fda.gov/drug/label.json?search=openfda.rxcui:", rxcui_from_ndc))
          'extract content from the response'
          content_fda_info_from_rxcui <- content(get_fda_info_from_rxcui, "text")
          'parse json'
          json_fda_info_from_rxcui<- fromJSON(  content_fda_info_from_rxcui, flatten = TRUE)
          'if boxed warning exists as a field'
          if("boxed_warning" %in% names(json_fda_info_from_rxcui$results)) {
            'then extract boxed warning'
            boxed_warningYN <- 'Y'
            boxed_warning <- json_fda_info_from_rxcui[["results"]][["boxed_warning"]][[1]]
            boxed_warning_list <- list(boxed_warningYN = boxed_warningYN, boxed_warning= boxed_warning)
          } else{
            'otherwise say no boxed warning indicated' 
            boxed_warningYN <- 'N'
            boxed_warning <- '--'
            boxed_warning_list <- list(boxed_warningYN = boxed_warningYN, boxed_warning= boxed_warning)
          }
           'create a list of the med_name, information_for_patients and boxed_warning' 
    }
         else{
           tallman_list <- list(med_display_name= 'NDC Not Found', tallmanYN= 'NDC Not Found')
           rxcui_from_ndc <- 'NDC Not Found'
           boxed_warning_list <- list(boxed_warningYN = 'NDC Not Found', boxed_warning= 'NDC Not Found')
         } 
          
          list(tallman_list$med_display_name, tallman_list$tallmanYN, rxcui_from_ndc, boxed_warning_list$boxed_warningYN,boxed_warning_list$boxed_warning )
}

# Create Data Frame -------------------------------------------------------
'create output data frame with tallman synonym (or name), information for patients, and boxed warning, as applicable'
NDC_output_table <- cbind(input, t(sapply(input$eleven_digit_ndc, function_get_ndc_info)))
'rename columns to match above fields'
colnames(NDC_output_table) <- c('NDC','Medication Name', 'Tall Man YN','RXCUI', 'Boxed Warning YN','Boxed Warning' )
'set rownames to default'
row.names(NDC_output_table) <- NULL

NDC_output_table <- unnest(NDC_output_table, cols = c(colnames(NDC_output_table)))

RXCUI_output_table <- NDC_output_table %>% group_by_at(setdiff(names(NDC_output_table), "NDC")) %>% tally(n="NDC Count") %>% arrange(RXCUI, 'desc')

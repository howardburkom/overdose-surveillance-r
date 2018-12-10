
### Parameters
NAME_IN <- paste("EMSOUTREACH_WKENDING_", date_pulled, sep = "")
MASTER_RESULTS_INTERNAL <- gs_title("MASTER_RESULTS_INTERNAL")
MASTER_ALL_RECORDS_SCORED <- gs_title("MASTER_ALL_RECORDS_SCORED")
MASTER_RESULTS_BHH <- gs_title("MASTER_RESULTS_BHH")
bhh_rows <- nrow(gs_read(MASTER_RESULTS_BHH))
BHH_REPEAT_DATABASE <- gs_title("BHH_REPEAT_DATABASE")
INTERNAL_REPEAT_DATABASE <- gs_title("INTERNAL_REPEAT_DATABASE")
RECORD_TRACKING <- gs_title("RECORD_TRACKING")

od_pi_terms <- "od_pi_terms_2" %>% 
  gs_title() %>% 
  gs_read()

od_cn_words <- "od_cn_words_2" %>% 
  gs_title() %>% 
  gs_read()
###

### Register input sheeet and load as df with adjusted person/place names
input <- gs_title(NAME_IN)
input <- gs_read(input)
input$`Patient First Name (ePatient.03)` <- toupper(input$`Patient First Name (ePatient.03)`)
input$`Patient Last Name (ePatient.02)` <- toupper(input$`Patient Last Name (ePatient.02)`)
input$`CAD Scene Incident City Name (eScene.17)` <- gsub("City of ", "", input$`CAD Scene Incident City Name (eScene.17)`)
input$`Patient Home City Name (ePatient.06)` <- gsub("City of ", "", input$`Patient Home City Name (ePatient.06)`)
input$`CAD Scene Incident City Name (eScene.17)` <- gsub("Town of ", "", input$`CAD Scene Incident City Name (eScene.17)`)
input$`Patient Home City Name (ePatient.06)` <- gsub("Town of ", "", input$`Patient Home City Name (ePatient.06)`)
input$`CAD Scene Incident City Name (eScene.17)` <- stri_trans_general(input$`CAD Scene Incident City Name (eScene.17)`, id = "Title")
input$`Patient Home City Name (ePatient.06)` <- stri_trans_general(input$`Patient Home City Name (ePatient.06)`, id = "Title")
input_data <- unique(input)
###


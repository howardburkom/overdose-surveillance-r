### Update internal results with scored data /// INTERNAL USE ONLY (PHI)

input_scored <- input_scored[, c(1:13, 15:17, 21, 18:20, 22:23, 29, 24:26, 28, 27, 35:50, 30:34)]

gs_add_row(MASTER_ALL_RECORDS_SCORED, input = input_scored)
print("Full internal record push finshed")
beep(10)

recs <- data.frame(Date = date_pulled, Overdose = nrow(output_internal), BHH = nrow(output_BHH))
gs_add_row(RECORD_TRACKING, input = recs)
print("Record tracking push finished")

if(nrow(output_internal) > 0) {
  
  gs_add_row(INTERNAL_REPEAT_DATABASE, input = select(output_internal,
                                                      `Patient First Name (ePatient.03)`,
                                                      `Patient Last Name (ePatient.02)`,
                                                      `Patient Gender (ePatient.13)`,
                                                      `Patient Date Of Birth (ePatient.17)`))
  
  cumulative <- gs_read(INTERNAL_REPEAT_DATABASE) %>% 
    group_by(`Patient First Name`,
             `Patient Last Name`,
             Gender,
             `Patient DOB`) %>% 
    summarise(count = n()) %>% 
    filter(count > 1)
  
  output_internal <- output_internal %>% 
    mutate(`Possible Repeat OD` = ifelse(output_internal$`Patient First Name (ePatient.03)` %in% cumulative$`Patient First Name` &
                                           output_internal$`Patient Last Name (ePatient.02)` %in% cumulative$`Patient Last Name` &
                                           output_internal$`Patient Gender (ePatient.13)` %in% cumulative$Gender &
                                           output_internal$`Patient Date Of Birth (ePatient.17)` %in% cumulative$`Patient DOB`, "Yes", "No"))
  
  gs_add_row(MASTER_RESULTS_INTERNAL, input = output_internal)
  
  print("Internal push finished")
  
  ###
  
  ### Update BHH with outreach cases /// INTERNAL & BHH USE ONLY (PHI)
  
  if(nrow(output_BHH) > 0){
    
    gs_add_row(BHH_REPEAT_DATABASE, input = select(output_BHH,
                                                   `Patient First Name (ePatient.03)`,
                                                   `Patient Last Name (ePatient.02)`,
                                                   `Patient Gender (ePatient.13)`,
                                                   `Patient Date Of Birth (ePatient.17)`))
    
    cumulative <- gs_read(BHH_REPEAT_DATABASE) %>% 
      group_by(`Patient First Name`,
               `Patient Last Name`,
               Gender,
               `Patient DOB`) %>% 
      summarise(count = n()) %>% 
      filter(count > 1)
    
    output_BHH <- output_BHH %>% 
      mutate(`Possible Repeat OD` = ifelse(output_BHH$`Patient First Name (ePatient.03)` %in% cumulative$`Patient First Name` &
                                             output_BHH$`Patient Last Name (ePatient.02)` %in% cumulative$`Patient Last Name` &
                                             output_BHH$`Patient Gender (ePatient.13)` %in% cumulative$Gender &
                                             output_BHH$`Patient Date Of Birth (ePatient.17)` %in% cumulative$`Patient DOB`, "Yes", "No"))
    
    id_col <- seq(bhh_rows, bhh_rows + nrow(output_BHH) - 1)
    
    
    output_BHH <- cbind(`Event Number` = id_col,
                        WPCROI = "",
                        WPCEnr = "",
                        output_BHH)
    
    gs_add_row(MASTER_RESULTS_BHH, input = output_BHH)
    print("BHH push finished")
    
  } else {
    
    print("Zero rows in BHH - push finished")
    
  }
  ###
  
  
} else {
  
  print("Zero rows in Internal - push finshed")
  
}

### Update LiveStories sheet with cases /// INTERNAL USE ONLY (NON-PHI)

if(nrow(output_LS) > 0) {
  
  MASTER_LIVESTORIES_EXPORT <- gs_title("MASTER_LIVESTORIES_EXPORT") 
  
  output_LS <- output_LS %>% 
    select(Date, `Day of Week`, Month, Year, `Patient Gender (ePatient.13)`, `Age Category`, `Naloxone Administered`, `Scene Incident Location Type (eScene.09)`, `CAD Scene Incident City Name (eScene.17)`, State) %>% 
    mutate(`Scene Incident Location Type (eScene.09)` = ifelse(is.na(`Scene Incident Location Type (eScene.09)`) == TRUE, "Unspecified", `Scene Incident Location Type (eScene.09)`)) %>% 
    mutate(Year = as.character(Year))
  
  gs_add_row(MASTER_LIVESTORIES_EXPORT, input = output_LS)
  print("LiveStories sheet updated")
  
} else {
  
  print("No records for LiveStories update")
  
}

###

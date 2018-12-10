### Functions
logitreg <- function(x) {
  
  intC <- -9.24550
  piC <- 0.65278
  cnwC <- 0.12398
  cndC <- 3.15435
  
  prob <- (1 / (1 + exp(-(as.numeric(x["pi_score"]) * piC + 
                            as.numeric(x["cn_d_score"]) * cndC +
                            as.numeric(x["cn_w_score"]) * cnwC + intC))))
                          
  return(prob)
  
}
piS <- function(x) {
  
  
  if(x %in% od_pi_terms$Primary.Impression){
    
    score <- od_pi_terms[which(od_pi_terms$Primary.Impression == x), 9]
    
  } else {
    
    score <- 0
    
  }
  
  return(score)
  
}
odScore <- function(x, df, num){
  
  
  temp <- data_frame(text = as.character(x)) # make each CC a df
  temp_words <- unnest_tokens(temp, word, text, token = "ngrams", n = num) # tokenize each df
  z <- sum(df$score[df$word %in% temp_words$word])
  
  return(z)
  
}
deathFx <- function(x){
  
  a <- sum(grepl("death", x))
  b <- sum(grepl("dead", x))
  d <- sum(grepl("field determination", x))
  
  if(a + b + d > 0){
    
    return(1)
    
  } else {
    
    return(0)
    
  }
  
}
diabFx <- function(x){
  
  a <- sum(grepl("diabet", x))
  b <- sum(grepl("glyc", x))
  c <- sum(grepl("insulin", x))
  
  if(a + b + c > 0){
    
    return(1)
    
  } else {
    
    return(0)
    
  }
  
}
###

### Narcan field wrangling
input_data <- input_data %>% 
  mutate(`Naloxone Administered` = ifelse(`Medication Given Description (eMedications.03)` == "Naloxone", "Yes", "No")) %>%
  mutate(`Medication Administered Date Time (eMedications.01)` = ifelse(`Naloxone Administered` == "Yes", `Medication Administered Date Time (eMedications.01)`, NA)) %>% 
  mutate(Doses = NA) %>% 
  mutate(Improved = NA) %>% 
  mutate(`Medication Administered Route (eMedications.04)` = ifelse(`Naloxone Administered` == "Yes", `Medication Administered Route (eMedications.04)`, NA)) %>%
  mutate(`Medication Response (eMedications.07)` = ifelse(`Naloxone Administered` == "Yes", `Medication Response (eMedications.07)`, NA)) %>% 
  unique() %>% 
  mutate(tempid = seq(1:nrow(.))) 

narc <- filter(input_data, `Naloxone Administered` == "Yes")

if(nrow(narc) > 0){
  
  print("Naloxone use identified")
  
  input_data <- input_data %>% 
    filter(!`CAD Incident Number (eResponse.03)` %in% narc$`CAD Incident Number (eResponse.03)`) %>% 
    mutate(`Naloxone Administered` = "No")
  
  narcan_uses_table <- narc %>% 
    group_by(`CAD Incident Number (eResponse.03)`) %>% 
    summarise(narcan_uses = n()) %>% 
    ungroup()

  narcan_outcome <- narc %>% 
    mutate(Improved = grepl("Improved", `Medication Response (eMedications.07)`)) %>% 
    group_by(`CAD Incident Number (eResponse.03)`, `Patient First Name (ePatient.03)`) %>% 
    summarise(outcome = max(Improved)) %>% 
    mutate(outcome = ifelse(outcome == 1, "Yes", NA)) %>% 
    pull(outcome)
  
  ids <- unique(narc$`CAD Incident Number (eResponse.03)`)
  nums <- vector("list")
  
  for (i in 1:length(ids)) {
    
    temp <- filter(narc, `CAD Incident Number (eResponse.03)` == ids[i])
    nums[[i]] <- max(temp$tempid)
    
  }
  
  input_data <- narc %>% 
    filter(tempid %in% unlist(nums)) %>% 
    full_join(., narcan_uses_table, by = "CAD Incident Number (eResponse.03)") %>% 
    mutate(Doses = narcan_uses) %>% 
    select(-narcan_uses) %>% 
    mutate(Improved = narcan_outcome) %>% 
    bind_rows(input_data) %>% 
    select(-tempid, -`Medication Dosage (eMedications.05)`) %>% 
    mutate(`Medication Administered Date Time (eMedications.01)` = ifelse(`Medication Given Description (eMedications.03)` == "Naloxone", `Medication Administered Date Time (eMedications.01)`, NA)) %>% 
    mutate(`Medication Given Description (eMedications.03)` = ifelse(`Medication Given Description (eMedications.03)` == "Naloxone", `Medication Given Description (eMedications.03)`, NA)) %>%
    unique()

} else {
  
  print("No narcan use identified")
  
  input_data <- input_data %>%
    select(-tempid, -`Medication Dosage (eMedications.05)`) %>% 
    mutate(`Medication Administered Date Time (eMedications.01)` = NA) %>% 
    mutate(`Medication Given Description (eMedications.03)` = NA) %>%
    mutate(`Medication Response (eMedications.07)` = NA) %>% 
    mutate(`Naloxone Administered` = "No") %>%
    mutate(Doses = NA) %>% 
    mutate(Improved = NA) %>% 
    unique()
  
}

###

### Initial data formatting
input_data$`Situation Primary Complaint Statement List (eSituation.04)` <- tolower(input_data$`Situation Primary Complaint Statement List (eSituation.04)`) 
input_data$`Patient Care Report Narrative (eNarrative.01)` <- tolower(input_data$`Patient Care Report Narrative (eNarrative.01)`)
input_data$`Situation Primary Complaint Statement List (eSituation.04)` <- gsub("heroine", "heroin", input_data$`Situation Primary Complaint Statement List (eSituation.04)`) 
input_data$`Patient Care Report Narrative (eNarrative.01)` <- gsub("heroine", "heroin", input_data$`Patient Care Report Narrative (eNarrative.01)`)
input_data$`Patient Age (ePatient.15)` <- as.numeric(gsub( " .*$", "", input_data$`Patient Age (ePatient.15)`))
input_data["Date"] <- gsub( " .*$", "", input_data$`Incident Date Time`)
input_data$Date <- as.Date(input_data$Date, "%m/%d/%Y")

drug_terms <- data.frame(word = c("heroin", "fentanyl", "methadone", "narcan", "overdose", " od "),
                         score = c(1, 1, 1, 1, 1, 1))
###

### Application of scoring functions
input_data["pi_score"] <- as.numeric(sapply(input_data$`Situation Provider Primary Impression (eSituation.11)`, function(x) piS(x)))
input_data["cn_w_score"] <- sapply(input_data$`Patient Care Report Narrative (eNarrative.01)`, function(x) odScore(x, od_cn_words, 1), USE.NAMES = FALSE)
input_data["cn_d_score"] <- sapply(input_data$`Patient Care Report Narrative (eNarrative.01)`, function(x) odScore(x, drug_terms, 1), USE.NAMES = FALSE)
input_data["doa"] <- sapply(input_data$`Patient Care Report Narrative (eNarrative.01)`, function(x) deathFx(x), USE.NAMES = FALSE) 
input_data["diab_pi"] <- sapply(input_data$`Situation Provider Primary Impression (eSituation.11)`, function(x) diabFx(x), USE.NAMES = FALSE) 
input_data["diab_cc"] <- sapply(input_data$`Situation Primary Complaint Statement List (eSituation.04)`, function(x) diabFx(x), USE.NAMES = FALSE) 
input_data["diab_cn"] <- sapply(input_data$`Patient Care Report Narrative (eNarrative.01)`, function(x) diabFx(x), USE.NAMES = FALSE) 
input_data["ti"] <- sapply(input_data$`Situation Provider Primary Impression (eSituation.11)`, function(x) sum(grepl("tramatic", x)), USE.NAMES = FALSE)
input_data["etoh"] <- sapply(input_data$`Situation Primary Complaint Statement List (eSituation.04)`, function(x) sum(grepl("etoh", x)), USE.NAMES = FALSE)
input_data["alcohol"] <- sapply(input_data$`Situation Primary Complaint Statement List (eSituation.04)`, function(x) sum(grepl("alcohol", x)), USE.NAMES = FALSE)
###

### Logit application
pi_colnum <- which(colnames(input_data) == "pi_score")
cn_d_colnum <- which(colnames(input_data) == "cn_d_score")
cn_w_colnum <- which(colnames(input_data) == "cn_w_score")

probs <- apply(input_data, 1, logitreg)

input_data <- input_data %>% 
  mutate(probability = probs)
###

### Threshold application and filtering
input_scored <- input_data %>% 
  mutate(od_pred = ifelse(probability > 0.02, 1, 0)) %>% 
  mutate(Doses = ifelse(is.na(Doses) == TRUE, 0, Doses))

output_internal <- input_scored %>% 
  mutate(od_pred = ifelse(is.na(Improved) == TRUE, od_pred, 1)) %>% 
  mutate(od_pred = ifelse(Doses > 1, 1, od_pred)) %>% 
  filter(od_pred == 1) %>%
  mutate(adj_pred = od_pred) %>% 
  mutate(adj_pred = ifelse(diab_pi + diab_cc + diab_cn > 0, 1, adj_pred)) %>% 
  mutate(adj_pred = ifelse(ti == 1, 0, adj_pred)) %>% 
  mutate(adj_pred = ifelse(etoh == 1 | alcohol == 1, 0, adj_pred)) %>%
  mutate(adj_pred = ifelse(`Patient Age (ePatient.15)` < 18, 0, adj_pred)) %>%
  mutate(adj_pred = ifelse(od_pred == 1 & grepl("care facility", `Patient Care Report Narrative (eNarrative.01)`) == TRUE, 0, adj_pred)) %>%  
  mutate(adj_pred = ifelse(od_pred == 1 & grepl(" snf ", `Patient Care Report Narrative (eNarrative.01)`) == TRUE, 0, adj_pred)) %>% 
  mutate(adj_pred = ifelse(od_pred == 1 & grepl("charcoal", `Patient Care Report Narrative (eNarrative.01)`) == TRUE, 0, adj_pred)) %>% 
  mutate(adj_pred = ifelse(od_pred == 1 & grepl("rehabilitation hospital", `Patient Care Report Narrative (eNarrative.01)`) == TRUE, 0, adj_pred)) %>%  
  mutate(adj_pred = ifelse(od_pred == 1 & grepl("rehab hospital", `Patient Care Report Narrative (eNarrative.01)`) == TRUE, 0, adj_pred))  %>% 
  mutate(adj_pred = ifelse(doa == 1 | `Disposition Incident Patient Disposition (eDisposition.12)` == "Dead on Scene, No Transport" | `Patient Age (ePatient.15)` < 18, 0, adj_pred)) %>% 
  mutate(doa = ifelse(doa == 1, "Yes", "No")) %>% 
  mutate(TO_BHH = ifelse(adj_pred == 0, "No", "Yes")) %>%
  mutate(TO_BHH = ifelse(grepl("quentin", `Patient Care Report Narrative (eNarrative.01)`) == TRUE | `Patient Home City Name (ePatient.06)` == "San Quentin", "No", TO_BHH)) %>% 
  mutate(TO_BHH = ifelse(is.na(`Patient Home City Name (ePatient.06)`) == TRUE, "Yes", TO_BHH)) %>%
  mutate(`Date Pulled` = date_pulled) %>% 
  mutate(adj_pred, ifelse(adj_pred == 1, "Yes", "No"))

email_table <- output_internal %>% 
  select(Date, `Patient Age (ePatient.15)`, `Situation Provider Primary Impression (eSituation.11)`, `Patient Care Report Narrative (eNarrative.01)`, Doses, `Improved`, `Patient Home City Name (ePatient.06)`, 
         `CAD Scene Incident City Name (eScene.17)`, doa, TO_BHH) %>% 
  rename(Age = `Patient Age (ePatient.15)`, Death = doa, `BHH Notified` = TO_BHH, `Home City` = `Patient Home City Name (ePatient.06)`,
         `Narcan Doses` = Doses, `Incident City` = `CAD Scene Incident City Name (eScene.17)`)

output_LS <- output_internal %>% 
  select(Date, `Patient Age (ePatient.15)`, `Naloxone Administered`, `Patient Gender (ePatient.13)`, `Scene Incident Location Type (eScene.09)`, `CAD Scene Incident City Name (eScene.17)`) %>% 
  mutate(`Naloxone Administered` = ifelse(is.na(`Naloxone Administered`) == TRUE, "No", `Naloxone Administered`)) %>% 
  mutate(`Day of Week` = wday(Date, label = TRUE, abbr = FALSE)) %>% 
  mutate(Month = month(Date, label = TRUE, abbr = FALSE)) %>% 
  mutate(Year = year(Date)) %>% 
  filter(`Patient Age (ePatient.15)` >= 18) %>% 
  mutate(`Age Category` = ifelse(`Patient Age (ePatient.15)` > 18 & `Patient Age (ePatient.15)` <= 30, "18-30 years",
                                 ifelse(`Patient Age (ePatient.15)` > 30 & `Patient Age (ePatient.15)` <= 64, "31-64 years", "65+ years"))) %>% 
  mutate(State = "California")

output_internal <- output_internal %>% 
  select(`Date Pulled`, Date, `Patient First Name (ePatient.03)`, `Patient Last Name (ePatient.02)`, `Patient Gender (ePatient.13)`, `Patient Age (ePatient.15)`, `Patient Date Of Birth (ePatient.17)`,
         `Situation Provider Primary Impression (eSituation.11)`, `Situation Primary Complaint Statement List (eSituation.04)`, `Patient Care Report Narrative (eNarrative.01)`, `Naloxone Administered`, Doses, Improved,
         `Scene Incident Street Address (eScene.15)`, `Patient Home City Name (ePatient.06)`, `Patient Home Phone Number (ePatient.18)`, `Patient Mobile Phone Number (ePatient.18)`, `Patient Home Postal Code (ePatient.09)`,`CAD Scene Incident Postal Code (eScene.19)`, 
         `Disposition Destination Name Delivered Transferred To (eDisposition.01)`, adj_pred, doa, TO_BHH, Latitude = `Scene GPS Latitude (eScene.11)`, Longitude = `Scene GPS Longitude (eScene.11)`) 

output_BHH <- output_internal %>%
  filter(TO_BHH == "Yes") %>% 
  select(`Date Pulled`, Date, `Patient First Name (ePatient.03)`, `Patient Last Name (ePatient.02)`, `Patient Gender (ePatient.13)`, `Patient Age (ePatient.15)`, `Patient Date Of Birth (ePatient.17)`,
         `Situation Provider Primary Impression (eSituation.11)`, `Situation Primary Complaint Statement List (eSituation.04)`, `Patient Care Report Narrative (eNarrative.01)`, `Naloxone Administered`, Doses, Improved,
         `Scene Incident Street Address (eScene.15)`, `Patient Home City Name (ePatient.06)`, `Patient Home Phone Number (ePatient.18)`, `Patient Mobile Phone Number (ePatient.18)`, `CAD Scene Incident Postal Code (eScene.19)`, 
         `Disposition Destination Name Delivered Transferred To (eDisposition.01)`) 
###

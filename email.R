
email_table <- email_table %>% 
  rename(Narrative = `Patient Care Report Narrative (eNarrative.01)`,
         Impression = `Situation Provider Primary Impression (eSituation.11)`)

res <- melt(gs_read(RECORD_TRACKING))

plot <- ggplot(data = res, aes(x = `Date Pulled`, y = value, color = variable)) +
  geom_point(size = 3) +
  geom_line(aes(group = variable),lwd = 1.5) +
  scale_color_discrete(name = "") +
  geom_hline(yintercept = mean(filter(res, variable == "Overdose")$value), lwd = 2, linetype = 6) +
  ggtitle("Identified Opioid Overdoses - Counts, Sent to BHH, and Average") +
  xlab("Date Pulled") +
  ylab("Count") +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 90, hjust = 1,vjust = 0.5),
        panel.grid.minor = element_blank())


res <- gs_read(MASTER_RESULTS_INTERNAL) %>%
  mutate(current = ifelse(`Date Pulled` == date_pulled, "Current", "Previous")) %>% 
  group_by(`Patient City`, current) %>% 
  summarise(count = n()) %>% 
  melt()
  
plot2 <- ggplot(data = res, aes(x = `Patient City`, y = value, fill = current)) +
  geom_col() +
  scale_fill_manual(name = "", values = c("#F75C03", "#00CC66")) +
  xlab("Patient City") +
  ylab("Count") +
  ggtitle("Identified Opioid Overdoses - by Patient City") +
  theme(plot.title = element_text(hjust = 0.5),
        panel.grid.minor = element_blank()) +
  coord_flip()

cn_wraped <- strwrap(email_table$Narrative, width = 200, simplify = FALSE)
cn_new <- sapply(cn_wraped, paste, collapse = "\n")
email_table <- mutate(email_table, Narrative = cn_new)

if(nrow(email_table) == 0) {
  
  dummy_row <- as.data.frame(t(rep("x", 10)))
  colnames(dummy_row) <- names(email_table)
  email_table <- rbind(email_table, dummy_row)
  
}
  
ttt <- ttheme_default()
ttt$core[2]$fg_params[3] <- 8
table <- tableGrob(email_table, rows = NULL, theme = ttt)
title <- textGrob(paste("Identified Opioid Overdose Events: Week of ", date_pulled, sep = ""),
                  gp = gpar(fontsize = 18))
padding <- unit(5, "mm")

table <- gtable_add_rows(
  table, 
  heights = grobHeight(title) + padding,
  pos = 0)

table <- gtable_add_grob(
  table, 
  title, 
  1, 1, 1, ncol(table))

if(nrow(email_table) >= 12){
  
  grid.arrange(table, plot)
  go_ahead <- menu(c("Yes", "No"), title = paste("Does this look correct?"))
  
  if(go_ahead == 0){
    
    stop("Potential graphics problem")
    
  }
  
} 

pdf(paste("Weekly Update_", date_pulled, ".pdf", sep = ""), width = 20, height = 8 + nrow(email_table)) # Open a new pdf file
grid.arrange(
  
  grobs = list(table, plot, plot2),
  widths = c(1, 1),
  layout_matrix = rbind(c(1, 1),
                        c(2, 3))
)
dev.off() # Close the file

###

### Basic statistics
res <- gs_read(MASTER_RESULTS_INTERNAL)
total_num_od <- nrow(res)

week_num_od <- res %>% 
  group_by(`Date Pulled`) %>% 
  summarise(count = n()) %>% 
  pull(count) %>% 
  mean() %>% 
  round(2)

od_stat <- res %>% 
  group_by(`Date Pulled`) %>% 
  summarise(count = n()) %>% 
  filter(`Date Pulled` != date_pulled)

avg_od <- round(mean(od_stat$count), 2)
sd_od <- round(sd(od_stat$count), 2)

if(nrow(output_internal) >= avg_od + 2 * sd_od){
  
  comparison_od <- "high"
  
}  else if(nrow(output_internal) <= avg_od - 2 * sd_od){
  
  comparison_od <- "low"
  
} else {
  
  comparison_od <- "normal"
  
}

w_fatalities <- nrow(filter(output_internal, doa == "Yes"))
t_fatalities <- nrow(filter(res, Death == "Yes"))

if(w_fatalities == 1){
  
  fs <- " was 1 fatality. "
  
} else {
  
  fs <- paste(" were ", w_fatalities, " fatalities. ", sep = "")
  
}

if(t_fatalities == 1){
  
  ft <- "1 overdose fatality "
  
} else {
  
  ft <- paste(t_fatalities, " overdose fatalities ", sep = "")
  
}

if(nrow(output_internal) == 1){
  
  w_od <- "was 1 overdose "
  
} else {
  
  w_od <- paste("were ", nrow(output_internal), " overdoses ", sep = "")
  
}

if(nrow(filter(email_table, `Narcan Doses` > 0)) == 1){
  
  w_nar <- " 1 patient.  "
  
} else {
  
  w_nar <- paste(" ", nrow(filter(email_table, `Narcan Doses` > 0)), " patients. ", sep = "")
  
}

if(nrow(output_BHH) == 1){
  
  bhh <- "1 record. "
  
} else {
  
  bhh <- paste(nrow(output_BHH), " records. ", sep = "")
  
}
###  

### Create paragraph
counts <- paste("For the week preceding ", date_pulled, " there ", w_od,
                "identified. BHH was sent ", bhh, 
                "Narcan was used by EMS on", w_nar,
                "There", fs, "Marin County is averaging ", week_num_od, " identified overdoses per week. ",
                "This week is ", comparison_od, " compared to the average. ", 
                "Since 4/29/2018 there have been ", total_num_od, " identified overdoses and ", ft, "in Marin County.", 
                sep = "")
###

### Email PHI)
html_msg <- mime() %>%
  to(c()) %>%
  ### EDIT EMAIL ADDRESS BELOW ###
  from() %>%
  ################################
  subject(paste("Confidential PHI - Internal Overdose Statistics - Week of: ", date_pulled)) %>% 
  html_body(counts)  %>%
  attach_part(counts) %>%
  attach_file(paste("Weekly Update_", date_pulled, ".pdf", sep = "")) -> email_msg_marin

print(counts)

go_ahead <- menu(c("Yes", "No"), title = "Do you want to send this to Marin?")

if(go_ahead == 1){
  
  send_message(email_msg_marin)
  print("Email sent to Marin and summary sheet uploaded to drive")
  drive_upload(paste("Weekly Update_", date_pulled, ".pdf", sep = ""), path = "Weekly Updates/", type = "pdf")
  file.remove(paste("Weekly Update_", date_pulled, ".pdf", sep = ""))
  
} else {
  
  file.remove(paste("Weekly OD Update_", date_pulled, ".pdf", sep = ""))
  print("Email NOT sent to Matt and summary sheet NOT uploaded")
  
}

###

### Email to BHH (NON-PHI)

 if(nrow(output_BHH) == 1){
   
   w_bhh <- " was 1 record "
   
 } else{
   
   w_bhh <- paste(" were ", nrow(output_BHH), " records ", sep = "")
   
 }
 
bhh_counts <- paste("The cases for the week prior to ", date_pulled, " have been acquired. There",
                     w_bhh, "added to the table.", sep = "")
 
mime() %>%
   to(c()) %>%
  ### EDIT EMAIL ADDRESS BELOW ###
  from() %>%
  ################################
   subject(paste("EMS Overdose Referral - Week of: ", date_pulled)) %>% 
   html_body(bhh_counts) -> email_msg_bhh
 
print(bhh_counts)
 
go_ahead <- menu(c("Yes", "No"), title = "Do you want to send this to BHH?")
 
if(go_ahead == 1){
   
   send_message(email_msg_bhh)
   print("Email sent to BHH")
   
 } else {
   
   print("Email NOT sent to BHH")
   
 }

###  

## Email to LiveStories (NON-PHI)
if(nrow(output_LS) == 1) {
  
  ls_text <- "LiveStories sheet updated. 1 record added."
  
} else {
  
  ls_text <- paste("LiveStories sheet updated. ", nrow(output_LS), " records added.", sep = "")
  
}

mime() %>%
  to(c()) %>%
  ### EDIT EMAIL ADDRESS BELOW ###
  from() %>%
  ################################
  subject(paste("LiveStories Overdose Update - Week of: ", date_pulled)) %>% 
  html_body(ls_text) -> email_msg_ls

print(ls_text)

send_message(email_msg_ls)
print("LiveStories update email sent")
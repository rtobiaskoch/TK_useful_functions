
#INPUT AND NOTES
#this handles up to 4 plates, can be more if you edit the grepl statement 
fn_pcr = location of your pcr output from quant studio
fn_platemap = location of your platemaps that correspond to the output from quantstudio

#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#pcr data
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#get list of file paths for all files in the pcr output from quantstudio
fn_path = list.files(path = fn_pcr, #to be informative it is a good idea to name your files something with p*|plate *|plate_*
                     full.names = T,
                     ignore.case = T)  

pcr_input = fn_path %>% 
  map(~ read_excel(.x, col_names = TRUE, sheet = "Results") %>% #iteratively read through all file names in path
        mutate(file_name = .x) %>% #add a column that is the file name where the observation came from
        filter(`Block Type` %in% c("Well", as.character(1:96))) #keep only the 96 wells as the output as superfluous BS
  ) %>%
  bind_rows() #put everything together

colnames(pcr_input) = pcr_input[1,] #replace the nonsensical names with the colnames that are in row one but keep file_name

#put file_name colname back by matching the fn_path that are rows in file_name
colnames(pcr_input) = if_else(colnames(pcr_input) %in% fn_path, 
                              "file_name", 
                              colnames(pcr_input))


pcr = pcr_input %>%
  clean_names() %>%
  filter(well_position != "Well Position") %>% #remove the column names from the dataframe observations
  mutate(plate = case_when( #create a new column that is a standardized name of your plate number or however many you might have
    grepl("plate 1|p1|plate_1", file_name, ignore.case = TRUE) ~ "plate_1",
    grepl("plate 2|p2|plate_2", file_name, ignore.case = TRUE) ~ "plate_2",
    grepl("plate 3|p3|plate_3", file_name, ignore.case = TRUE) ~ "plate_3",
    grepl("plate 4|p4|plate_4", file_name, ignore.case = TRUE) ~ "plate_4",
    TRUE ~ "unknown"
  )
  ) %>%
  mutate(ct = if_else(str_detect(ct, "Undetermined"), "55.55", ct)) %>% # convert "undetermined to numeric 55.55 to avoid errors
  mutate(cq = round(as.numeric(ct), 2),
         well = as.numeric(well), #convert to numeric to sort
         copies = if_else(cq == 55.55, 0, round(as.numeric(quantity),2))
         #ct_threshold = as.numeric(ct_threshold)
  ) %>%
  arrange(plate, well) %>%
  # select(well_position, task, target_name, cq, ct_threshold, copies, plate) %>%
  select(well_position, target_name, cq, ct_threshold, copies, plate) %>%
  pivot_wider(names_from = target_name, 
              values_from = c(copies,cq)) %>%
  rename(cq = cq_WNV) # rename to match database and rest of code

#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#Platemap
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#get the list of platemaps from your 
fn_path = list.files(path = fn_platemap,
                     full.names = T,
                     ignore.case = T)  

platemap = fn_path %>%
  map(~read_excel(.x, col_names = T, 
                  col_types = "text", 
                  range = "A1:M9",
                  sheet = "pcr") %>%
        rename(row = "...1") %>%
        pivot_longer(cols = -row, 
                     names_to = "column", 
                     values_to = "csu_id") %>%
        mutate(file_name = .x) %>%
        mutate(plate = case_when(
          grepl("plate 1|p1|plate_1", file_name, ignore.case = TRUE) ~ "plate_1",
          grepl("plate 2|p2|plate_2", file_name, ignore.case = TRUE) ~ "plate_2",
          grepl("plate 3|p3|plate_3", file_name, ignore.case = TRUE) ~ "plate_3",
          grepl("plate 4|p4|plate_4", file_name, ignore.case = TRUE) ~ "plate_4",
          TRUE ~ "unknown")) %>%
        mutate(well_position = paste0(row, column)) %>%
        mutate(well_position = str_remove(well_position, "\\.0")) %>% #remove the column ##.0 that is getting imported
        select(well_position, csu_id, plate)
  ) %>%
  bind_rows()

#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#merge
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
cq_data = left_join(pcr, platemap, by = c("well_position", "plate"))  %>%
  filter(!is.na(csu_id))


#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#---------------- C H E C K   S T D S  ----------------------
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# stds = cq_data %>%
#   filter(task == "STANDARD"|grepl("std", csu_id, ignore.case = T))

#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#---------------- C H E C K   C O N T R O L S  ----------------------
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
pos = cq_data %>% 
  filter(grepl("pos", csu_id, ignore.case = T))

if(any(pos$copies_WNV < copy_threshold|pos$copies_SLEV < copy_threshold)) {
  warning(paste0("one of your positive extraction controls have < ",copy_threshold, " copies"))
}


neg = cq_data %>% 
  filter(grepl("neg", csu_id, ignore.case = T))

if(any(neg$copies_WNV > copy_threshold|neg$copies_SLEV > copy_threshold)) {
  warning(paste0("one of your negative extraction controls have > ",copy_threshold, " copies"))
}

#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#---------------- C H E C K   S A M P L E   I D S  ----------------------
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#add check for sample id format

# if(any(!str_detect(data_input$csu_id, "-"))) {
#   stop("you have samples id without a -")
# }


temp = cq_data %>%
  filter(grepl("^CSU|^BOU|^CDC", csu_id, ignore.case = T))

if(nrow(get_dupes(temp, csu_id)) >0) {
  stop("Alert! You have duplicate id's in your pcr plate run. Double check your plates to ensure this was intentional. ")
} else(
  print("no duplicate sample ids that start with CSU|BOU|CDC")
)


if(any(cq_data$ct_threshold < rn_threshold)) {
  stop(paste0("Ct baseline thresholds are RN below ", rn_threshold, "check Quantstudio (pcr) output file."))
} else {
  
  write.csv(cq_data, fn_cq_out, row.names = F)
}





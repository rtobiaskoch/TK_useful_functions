
mugration_clean_fun = function(discreet_state, #discrete states from your mdata to help slice the mugration_key
                               mugration_key,  #mugration key output from the mugration function from timetree
                               mugration_data, #mugration matrix data from the mugration function from timetree
                               col_name){      #new colname for your discrete state of inferest
  
  n = length(unique(discreet_state)) #for slice
  
  mugration_key = mugration_key %>%
    dplyr::slice(1:n) %>%
    separate(`Map from character to field name`, into = c("key", "inferred_state"), sep = ":") %>%
    mutate(key = row_number())
  
  
  colnames(mugration_data) <- as.character(c("node",1:n))
  
  mugration_data %>%
    pivot_longer(cols = -node, names_to = "key", values_to = "confidence") %>%
    mutate(key = as.integer(key)) %>%
    group_by(node) %>%
    filter(confidence == max(confidence)) %>%
    left_join(mugration_key, by = "key") %>%
    mutate(inferred_state = gsub("\t", "", inferred_state)) %>%
    dplyr::select(-key) %>%
    dplyr::rename(!!col_name := inferred_state)
}
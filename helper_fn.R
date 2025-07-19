### HELPER FUNCTION ###

# function to clean address column
clean_address <- function(data, address_col) {
  
  if (!is.character(address_col)) {
    stop("Please supply the column name in a character data type")
  }
  
  address_vector <- data[, address_col]
  
  # replace "\n" with space 
  address_vector <- gsub(pattern = "\n", replacement = " ", x = address_vector)
  
  # replace punctuation with space
  address_vector <- gsub(pattern = "[[:punct:]]", replacement = "", x = address_vector)
  
  # parse address to lowercase
  address_vector <- tolower(address_vector)
  
  # possible clean up address
  # level, unit, PO Box, Apt., Flat, Suite
  pattern_vec <- c("level\\s[0-9]+", "unit\\s[0-9]+", "po\\sbox\\s",
                   "apt\\s[0-9]+", "flat\\s[0-9]+", "suite\\s[0-9]+")
  
  clean_address_vector <- c()
  
  for (i in seq_along(address_vector)) {
    
    target_address <- address_vector[i]
    
    for (j in seq_along(pattern_vec)) {
      
      target_pattern <- pattern_vec[j]
      
      if (grepl(pattern = target_pattern, x = target_address, ignore.case = TRUE)) {
        
        target_address <- gsub(pattern = target_pattern, 
                               replacement = " ", 
                               x = target_address,
                               ignore.case = TRUE)
      } 
    }
    
    # Normalize whitespace and trim
    target_address <- gsub("\\s+", " ", target_address)
    target_address <- trimws(target_address)
    
    clean_address_vector <- c(clean_address_vector, target_address)
  }
  
  # capture the zip code at the end of the address
  zip_code <- regmatches(clean_address_vector, regexpr("\\s[0-9]+$", clean_address_vector, ignore.case = TRUE))
  
  # remove zip code from address
  clean_address_vector <- gsub("\\s[0-9]+$", replacement = "", x = clean_address_vector, ignore.case = TRUE)
  
  # re-normalize vectors
  zip_code <- gsub("\\s+", "", zip_code)
  zip_code <- trimws(zip_code)
  zip_code <- as.numeric(zip_code)
  
  clean_address_vector <- gsub("\\s+", " ", clean_address_vector)
  clean_address_vector <- trimws(clean_address_vector)
  
  address_data <- data.frame(address_street_clean = clean_address_vector,
                             zip_code = zip_code)
  
  return(address_data)
}

# function to clean name and DOB
clean_name_dob <- function(data) {
  
  # sense check for targeted columns
  col_name <- colnames(data)
  
  if (!any(col_name %in% c("name", "date_of_birth"))) {
    stop("The 'name' and 'date_of_birth' column does not exist in the dataframe.")
  }
  
  data_clean <- data %>% 
    separate(name, into = c("first_name", "second_name"), sep = "-", remove = FALSE) %>% 
    separate(first_name, into = c("first", "second"), sep = " ", remove = FALSE) %>% 
    select(-name, -first_name) %>% 
    rename("first_name" = first, 
           "second_name" = second,
           "last_name" = second_name) %>% 
    mutate(across(1:3, ~ toupper(.x))) %>% 
    mutate(across(1:3, ~ gsub("[[:punct:]]", .x, replacement = ""))) %>% 
    mutate(year = as.numeric(year(date_of_birth)),
           month = as.numeric(month(date_of_birth)),
           day = as.numeric(day(date_of_birth))) %>% 
    relocate(year, month, day, .after = date_of_birth)

  return(data_clean)
}


# # function for diagnostic check
# diag_check <- function(dfA, dfB, matches_pair) {
#   
#   dfA <- dfA %>% mutate(row_id = row_number(), dataset = "A")
#   dfB <- dfB %>% mutate(row_id = row_number(), dataset = "B")
#   
#   
#   
#   
# }
# 
# dfA <- educ_new_clean %>% mutate(row_id = row_number(), dataset = "A")
# dfB <- health_new_clean %>% mutate(row_id = row_number(), dataset = "B")
# 
# matched_eval <- left_join(dfA, matches_output_first, by = join_by("row_id" == "inds.a")) %>% 
#   rename("true_id_a" = true_id) %>% 
#   left_join(., dfB, by = join_by("inds.b" == "row_id")) %>% 
#   rename("true_id_b" = true_id)
# 
# matched_eval2 <- out_temp$matches %>% 
#   left_join(dfA %>% select(row_id, true_id), by = join_by("inds.a" == "row_id")) %>% 
#   rename("true_id_a" = true_id) %>% 
#   left_join(dfB %>% select(row_id, true_id), by = join_by("inds.b" == "row_id")) %>% 
#   rename("true_id_b" = true_id)
# 
# 
# matched_eval2 <- matched_eval2 %>% 
#   mutate(result = ifelse(true_id_a == true_id_b, "TP", "FP"))
# 
# TP <- sum(matched_eval2$result == "TP")
# FP <- sum(matched_eval2$result == "FP")
# 
# true_matches <- dfA %>%
#   inner_join(dfB, by = "true_id") %>%
#   mutate(result = "FN")  # all true links
# 
# predicted_pairs <- matched_eval2 %>%
#   mutate(pair_key = paste(inds.a, inds.b, sep = "_"))
# 
# true_pairs <- dfA %>%
#   mutate(dfA.row = row_number()) %>%
#   inner_join(dfB %>% mutate(dfB.row = row_number()), by = "true_id") %>%
#   mutate(pair_key = paste(dfA.row, dfB.row, sep = "_"))
# 
# FN <- sum(!(true_pairs$pair_key %in% predicted_pairs$pair_key))
# 
# # Precision, Recall, FNR, FDR
# FDR <- round(FP / (FP + TP), 3)
# FNR <- round(FN / (TP + FN), 3)
# PRE <- 1 - FDR
# REC <- 1 - FNR
# 
# df1 <- out_temp$matches
# df2 <- df1 %>% filter(inds.a != inds.b)
# 
# data_A <- dfA %>% mutate(row_id_a = row_number())
# data_B <- dfB %>% mutate(row_id_b = row_number())
# 
# merge1 <- left_join(df2, data_A, by = join_by("inds.a" == "row_id_a"))
# merge2 <- left_join(merge1, data_B, by = join_by("inds.b" == "row_id_b"))
# 
# 
# 
# 
# out_temp <- fastLink(dfA = educ_new_clean, dfB = health_new_clean,
#                      varnames = c("first_name","second_name", "last_name", "gender","address_street_clean", "zip_code"),
#                      stringdist.match = c("first_name", "second_name", "last_name", "address_street_clean"),
#                      partial.match = c("first_name", "second_name", "last_name", "address_street_clean"),
#                      numeric.match = c( "zip_code"),
#                      cut.a.num = 1.25,
#                      cut.p.num = 2.5,
#                      stringdist.method = "jw",
#                      cut.a = 0.94,
#                      cut.p = 0.85,
#                      n.cores = 4,
#                      threshold.match = 0.95,
#                      return.all = TRUE)
# 
# mean(out_temp$posterior)
# 
# confusion(out_temp)
# 
# linked_results <- out_temp$matches %>%
#   left_join(educ_new_clean %>% mutate(inds.a = row_number()), by = "inds.a") %>%
#   left_join(health_new_clean %>% mutate(inds.b = row_number()), by = "inds.b") %>% 
#   mutate(true_match = true_id.x == true_id.y)


###############################################################################
#  BLOCKING PASSES FOR TAX - EDUC LINKAGE
#  BLOCK = GENDER
#  LINKING = First Name, Second Name, Last Name, Birth Year, Month, Day

# clean dataset
tax_clean <- clean_name_dob(test_tax)

educ_clean <- clean_name_dob(test_educ)

tax_address_clean <- clean_address(tax_clean, "address_street")
educ_address_clean <- clean_address(educ_clean, "address_street")

educ_new_clean <- cbind(educ_new, educ_address_clean) %>% 
  select(-address_street) %>% 
  relocate(address_street_clean, zip_code, .after = gender)

tax_new_clean <- cbind(tax_clean, tax_address_clean) %>% 
  select(-address_street) %>% 
  relocate(address_street_clean, zip_code, .after = gender)

# check for how many duplicates in the dataset
check_dups <- function(data){
 data_dups <-  data %>% 
    group_by(first_name, second_name, date_of_birth) %>% 
    summarise(n = n()) %>% 
    filter(n > 1)
}

# tax duplicates
tax_dups <- check_dups(tax_new_clean)

# education duplicates
educ_dups <- check_dups(educ_new_clean)

# dedupe tax data
# due to the longitudinal frame, you can disregard variables
# that are varies overtime. time invariant variables such as name, DOB and 
# perhaps address (but this could be varied due to mobiliity)

# first blocking candidate: gender
block_gender <- blockData(dfA = tax_new_clean, dfB = tax_new_clean, varnames = "gender")

results_dedupe_tax <- list()
link_dedupe_model <- list()

for (i in 1:length(block_gender)) {
  
  data_temp <- tax_new_clean[block_gender[[i]]$dfA.inds, ]
  
  hide <- capture.output(out_temp <- fastLink(dfA = data_temp, 
                       dfB = data_temp,
                       varnames = c("first_name", "second_name", "year", 
                                    "month", "day", "address_street_clean", "zip_code"),
                       stringdist.match = c("first_name", "second_name"),
                       stringdist.method = "jw",
                       numeric.match = c("year", "month", "day", "zip_code"),
                       partial.match = c("first_name", "second_name"),
                       dedupe.matches = FALSE,
                       return.all = TRUE))
  
  record_temp <- getMatches(dfA = data_temp,
                            dfB = data_temp,
                            fl.out = out_temp,
                            combine.dfs = FALSE)
  
  results_dedupe_tax[[i]] <- record_temp
  link_dedupe_model[[i]] <- out_temp
}


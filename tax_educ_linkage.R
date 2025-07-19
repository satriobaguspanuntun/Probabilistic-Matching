###############################################################################
#  BLOCKING PASSES FOR TAX - EDUC LINKAGE
#  BLOCK = GENDER
#  LINKING = First Name, Second Name, Last Name, Birth Year, Month, Day

# clean dataset
tax_clean <- clean_name_dob(test_tax)

educ_clean <- clean_name_dob(test_educ)

tax_address_clean <- clean_address(tax_clean, "address_street")
educ_address_clean <- clean_address(educ_clean, "address_street")

educ_new_clean <- cbind(educ_clean, educ_address_clean) %>% 
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
# instead of deduping the tax data first, you can just collate the unique id's

tax_new_clean_unique <- tax_new_clean %>% 
  group_by(first_name, second_name,
           date_of_birth, year, month, day, gender) %>% 
  summarise(n = n()) %>% 
  mutate(soundex_fname = substr(soundex(first_name), 1, 2))


# first blocking candidate: gender
block_gender <- blockData(dfA = tax_new_clean_unique, dfB = tax_new_clean_unique, varnames = "gender")

results_dedupe_tax <- list()
link_dedupe_model <- list()

for (i in 1:length(block_gender)) {
  
  data_temp <- tax_new_clean[block_gender[[i]]$dfA.inds, ]
  
  hide <- capture.output(out_temp <- fastLink(dfA = data_temp, 
                       dfB = data_temp,
                       varnames = c("first_name", "second_name", "year", 
                                    "month", "day"),
                       stringdist.match = c("first_name", "second_name"),
                       stringdist.method = "jw",
                       numeric.match = c("year", "month", "day", "zip_code"),
                       partial.match = c("first_name", "second_name"),
                       dedupe = FALSE,
                       return.all = TRUE))
  
  record_temp <- getMatches(dfA = data_temp,
                            dfB = data_temp,
                            fl.out = out_temp)
  
  results_dedupe_tax[[i]] <- record_temp
  link_dedupe_model[[i]] <- out_temp
}

df_res <- do.call(rbind, results_dedupe_tax)

length(unique(df_res$dedupe.ids))

# second blocking candidate: kmeans on birth year
block_kmeans <- blockData(dfA = tax_new_clean_unique,
                          dfB = tax_new_clean_unique,
                          varnames = c("year"),
                          window.block = "year",
                          window.size = 4)

results_dedupe_tax <- list()
link_dedupe_model <- list()

for (i in 1:length(block_kmeans)) {
  
  data_temp <- tax_new_clean_unique[block_kmeans[[i]]$dfA.inds, ]
  
  hide <- capture.output(out_temp <- fastLink(dfA = data_temp, 
                                              dfB = data_temp,
                                              varnames = c("first_name", "second_name", "year", 
                                                           "month", "day", "soundex_fname"),
                                              stringdist.match = c("first_name", "second_name"),
                                              stringdist.method = "jw",
                                              numeric.match = c("year", "month", "day"),
                                              partial.match = c("first_name", "second_name"),
                                              dedupe = FALSE,
                                              return.all = TRUE))
  
  record_temp <- getMatches(dfA = data_temp,
                            dfB = data_temp,
                            fl.out = out_temp)
  
  record_temp$dedupe.ids <- paste0("K", i, "-", record_temp$dedupe.ids)
  
  results_dedupe_tax[[i]] <- record_temp
  link_dedupe_model[[i]] <- out_temp
}

df_res <- do.call(rbind, results_dedupe_tax)

length(unique(df_res$dedupe.ids))

###############################################################################
#  BLOCKING PASSES FOR EDUCATION - HEALTH LINKAGE
#  FIRST PASS: 
#  BLOCK = GENDER
#  LINKING = First Name, Second Name, Last Name, Birth Year, Month, Day
#
#  SECOND PASS:
#  Block = Birth Year
#  Linking = First Name, Second Name, Last Name, address street, zip code
#
#  THIRD PASS: (maybe?)
#  Block = Soundex first and second name
#  Linking = First Name, Second Name, Last Name, Birth Year, Month, Day, address street, zip code

# clean dataset
educ_new <-  test_educ %>% 
  separate(name, into = c("first_name", "second_name"), sep = "-", remove = FALSE) %>% 
  separate(first_name, into = c("first", "second"), sep = " ", remove = FALSE) %>% 
  select(-true_id, -name, -first_name) %>% 
  rename("first_name" = first, 
         "second_name" = second,
         "last_name" = second_name) %>% 
  mutate(across(1:3, ~ toupper(.x))) %>% 
  mutate(across(1:3, ~ gsub("[[:punct:]]", .x, replacement = ""))) %>% 
  mutate(year = as.numeric(year(date_of_birth)),
         month = as.numeric(month(date_of_birth)),
         day = as.numeric(day(date_of_birth))) %>% 
  relocate(year, month, day, .after = date_of_birth)

health_new <- test_health %>% 
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

# clean address 
health_address_clean <- clean_address(health_new, "address_street")
educ_address_clean <- clean_address(educ_new, "address_street")

educ_new_clean <- cbind(educ_new, educ_address_clean) %>% 
  select(-address_street) %>% 
  relocate(address_street_clean, zip_code, .after = gender)

health_new_clean <- cbind(health_new, health_address_clean) %>% 
  select(-address_street) %>% 
  relocate(address_street_clean, zip_code, .after = gender)

###############################################################################
# FIRST PASS
# Record linkage block on gender #

block_gender <- blockData(dfA = educ_new_clean, dfB = health_new_clean, varnames = c("gender"))

results_gender <- list()
aggregate_link_model_first <- list()

for (block in seq_along(block_gender)) {
  
  data_temp_a <- educ_new_clean[block_gender[[block]]$dfA.inds, ]
  data_temp_b <- health_new_clean[block_gender[[block]]$dfB.inds, ]
  
  out_temp <- fastLink(dfA = data_temp_a, dfB = data_temp_b,
                       varnames = c("first_name","second_name", "last_name", "year", "month", "day"),
                       stringdist.match = c("first_name", "second_name", "last_name"),
                       partial.match = c("first_name", "second_name", "last_name"),
                       numeric.match = c( "year", "month", "day"),
                       cut.a.num = 1.25,
                       cut.p.num = 2.5,
                       stringdist.method = "jw",
                       cut.a = 0.94,
                       cut.p = 0.85,
                       n.cores = 4,
                       threshold.match = 0.90)
  
  record_temp <- getMatches(dfA = data_temp_a,
                            dfB = data_temp_b,
                            fl.out = out_temp)
  
  aggregate_link_model_first[[block]] <- out_temp
  results_gender[[block]] <- record_temp  
}

final_output_first <- do.call(rbind, results_gender)

agg_out_first <- aggregateEM(em.list = aggregate_link_model_first)

summary(agg_out_first)


###############################################################################
# SECOND PASS
# Record linkage block on Birth year #

block_birth_year <- blockData(dfA = educ_new_clean, dfB = health_new_clean, varnames = c("year"))

results_year <- list()
aggregate_link_model_second <- list()

for (block in seq_along(block_birth_year)) {
  
  data_temp_a <- educ_new_clean[block_birth_year[[block]]$dfA.inds, ]
  data_temp_b <- health_new_clean[block_birth_year[[block]]$dfB.inds, ]
  
  out_temp <- fastLink(dfA = data_temp_a, dfB = data_temp_b,
                       varnames = c("first_name","second_name", "last_name", "gender","address_street_clean", "zip_code"),
                       stringdist.match = c("first_name", "second_name", "last_name", "address_street_clean"),
                       partial.match = c("first_name", "second_name", "last_name", "address_street_clean"),
                       numeric.match = c( "zip_code"),
                       cut.a.num = 1.25,
                       cut.p.num = 2.5,
                       stringdist.method = "jw",
                       cut.a = 0.94,
                       cut.p = 0.85,
                       n.cores = 4,
                       threshold.match = 0.90)
  
  record_temp <- getMatches(dfA = data_temp_a,
                            dfB = data_temp_b,
                            fl.out = out_temp)
  
  aggregate_link_model_second[[block]] <- out_temp
  results_year[[block]] <- record_temp  
}

final_output_second <- do.call(rbind, results_year)

agg_out_second <- aggregateEM(em.list = aggregate_link_model_second)

summary(agg_out_second)

###############################################################################
# THIRD PASS
# Record linkage by zip code #

block_zip <- blockData(dfA = educ_new_clean, dfB = health_new_clean, varnames = c("zip_code"))

results_zip <- list()
aggregate_link_model_third <- list()

for (block in seq_along(block_zip)) {
  
  data_temp_a <- educ_new_clean[block_zip[[block]]$dfA.inds, ]
  data_temp_b <- health_new_clean[block_zip[[block]]$dfB.inds, ]
  
  out_temp <- fastLink(dfA = data_temp_a, dfB = data_temp_b,
                       varnames = c("first_name","second_name", "last_name", "gender","address_street_clean", "zip_code"),
                       stringdist.match = c("first_name", "second_name", "last_name", "address_street_clean"),
                       partial.match = c("first_name", "second_name", "last_name", "address_street_clean"),
                       numeric.match = c( "zip_code"),
                       cut.a.num = 1.25,
                       cut.p.num = 2.5,
                       stringdist.method = "jw",
                       cut.a = 0.94,
                       cut.p = 0.85,
                       n.cores = 4,
                       threshold.match = 0.90)
  
  record_temp <- getMatches(dfA = data_temp_a,
                            dfB = data_temp_b,
                            fl.out = out_temp)
  
  aggregate_link_model_third[[block]] <- out_temp
  results_zip[[block]] <- record_temp  
}

final_output_third <- do.call(rbind, results_zip)

agg_out_third <- aggregateEM(em.list = aggregate_link_model_third)

summary(agg_out_third)


test_final <- rbind(final_output_first[, 1:24], final_output_second[, 1:24]) %>% distinct()

agg_out_final <- aggregateEM(em.list = c(aggregate_link_model_first, aggregate_link_model_second))

summary(agg_out_final)

length(unique(test_final %>% mutate(row = row_number()) %>% pull(row)))

educ_new_clean2 <- educ_new_clean %>% rownames_to_column()
health_new_clean2 <- health_new_clean %>% rownames_to_column()

matches <- out_third$matches %>% mutate(across(everything(), as.character))

complete <- left_join(educ_new_clean2, matches, by = join_by("rowname" == "inds.a"))

complete <- left_join(complete, health_new_clean2, by = join_by("inds.b" == "rowname"))

educ_matched <- educ_new_clean2[matches$inds.a, ]
health_matched <- health_new_clean2[matches$inds.b, ]

educ_non_matched <- educ_new_clean2[!educ_new_clean2$rowname %in% matches$inds.a, ]
health_non_matched <- health_new_clean2[!health_new_clean2$rowname %in% matches$inds.b, ]

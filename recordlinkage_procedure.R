library(tidyverse)
library(fastLink)
library(RecordLinkage)
library(lubridate)
library(stringr)

data <- RLdata10000 %>% mutate(rec_id = row_number(),
                               ent_id = identity.RLdata10000)

# count the number of ID's
length(unique(data$ent_id))

# count NA's in each columns or fields (this help which fields should we pick)
# the rule of thumb is to ensure each targeted field to have missing data no less that 30%
# otherwise, this would create an issue but it can be avoided with other methods later on
na_fields <- data %>% 
  select(-ent_id) %>% 
  summarise(across(1:ncol(data) - 1, ~ sum(is.na(.x)))) %>% 
  pivot_longer(cols = 1:8, names_to = "var", values_to = "value") %>% 
  mutate(total_row = nrow(data),
         missingness = value/total_row * 100)

print(na_fields)

# fname_c2 and lcname_c2 columns have 95%+ rate of missing values.

# linkage fields
linkageFields <- c("fname_c1", "lname_c1", "by", "bm", "bd")

## exact matching
exact.match <- merge(data, data, by = linkageFields)

## number of non-self matches 
sum(exact.match$rec_id.x != exact.match$rec_id.y)

## 16 non match who are they?
exact.match[exact.match$rec_id.x != exact.match$rec_id.y, linkageFields]


#### FastLink ####
## fields that we will compare based on
## a string dissimilarity measure
stringDistFields <- c("fname_c1", "lname_c1")

# Fields for which we have 3 possible agreement
# {Agree, Partially Agree, Disagree}
partialMatchFields <- c("fname_c1", "lname_c1")

out <- fastLink(dfA = data, 
                dfB = data,
                varnames = linkageFields,
                stringdist.match = stringDistFields,
                partial.match = partialMatchFields,
                cut.a = 0.94,
                cut.p = 0.84,
                dedupe = FALSE)

recordsfL <- getMatches(dfA = data, dfB = data, fl.out = out)
length(unique(recordsfL$dedupe.ids))

# duplicated data 
head(recordsfL, 4)

recordsfL[recordsfL$ent_id == 20, ]

recordsfL[recordsfL$ent_id == 77, ]

# contigency table
# true duplicat indicator
recordsfL$duptrue <- ifelse(duplicated(recordsfL$ent_id), 
                            "Duplicated", "Not Duplicated")

# duplicate from fastlink
recordsfL$dupfl <- ifelse(duplicated(recordsfL$dedupe.ids),
                          "Duplicated", "Not Duplicated")

confusion <- table("FL" = recordsfL$dupfl,
                   "True" = recordsfL$duptrue)

# True positive, false positive, and false negative
TP <- confusion[1, 1]
FP <- confusion[1, 2]
FN <- confusion[2, 1]

# False negative rate
FDR <- round(FP/(FP + TP), 3)
FDR

# false negative rate
FNR <- round(FN/1000, 3)
FNR

# precision 
PRE <- 1 - FDR
PRE

# recall
REC <- 1 - FNR
REC

## we can add numeric comparisons using dissimilarity
numeric_match_fields <- c("by")

## make sure these are of class numeric
data$by <- as.numeric(data$by)

# fastlink with numeric comparisons
out2 <- fastLink(dfA = data, 
                 dfB = data,
                 varnames = linkageFields,
                 stringdist.match = stringDistFields,
                 cut.a = 0.94,
                 cut.p = 0.84,
                 numeric.match = numeric_match_fields,
                 cut.a.num = 1.5,
                 partial.match = partialMatchFields,
                 threshold.match = 0.90,
                 dedupe = FALSE)

recordsfL2 <- getMatches(dfA = data, dfB = data, fl.out = out2)
length(unique(recordsfL2$dedupe.ids))

diag_fl <- function(fastlink_data) {

  data <- fastlink_data
  
  # true duplicat indicator
  data$duptrue <- ifelse(duplicated(data$ent_id), 
                         "Duplicated", "Not Duplicated")
  
  # duplicate from fastlink
  data$dupfl <- ifelse(duplicated(data$dedupe.ids),
                       "Duplicated", "Not Duplicated")
  
  confusion <- table("FL" = data$dupfl,
                     "True" = data$duptrue)
  print(confusion)
  # True positive, false positive, and false negative
  TP <- confusion[1, 1]
  FP <- confusion[1, 2]
  FN <- confusion[2, 1]
  
  # False discoveru rate
  FDR <- round(FP/(FP + TP), 3)
  FDR
  
  # false negative rate
  FNR <- round(FN/1000, 3)
  FNR
  
  # precision 
  PRE <- 1 - FDR
  PRE
  
  # recall
  REC <- 1 - FNR
  REC
  
  diag_table <- data.frame(true_positve = TP,
                           false_postive = FP,
                           false_negative = FN,
                           false_discovery_rate = FDR,
                           false_negative_rate = FNR,
                           precision = PRE,
                           recall = REC)
  
  diag_table <- pivot_longer(diag_table, cols = 1:ncol(diag_table), names_to ="Indicator" , values_to = "Values")
  
  return(list("data" = data, "diag_table" = diag_table, "confusion" = confusion))
}

diag_fl(recordsfL)

## Blocking ##
# Traditional Blocking # 
# block by birth year
data$by2 <- data$by
data$by2[data$by < 1924] <- 1923
data$by2[data$by > 2008] <- 2009

## Traditional Blocking:
blockby <- blockData(dfA = data, dfB = data, varname = "by2")

linkageFields2 <- c("fname_c1", "lname_c1", "bm", "bd")

results <- list()

for (i in 1:length(blockby)) {
  data.temp <- data[blockby[[i]]$dfA.inds, ]
  
  out.temp <- fastLink(dfA = data.temp, dfB = data.temp,
                       varnames = linkageFields2,
                       stringdist.match = stringDistFields,
                       partial.match = partialMatchFields,
                       cut.a = 0.92, cut.p = 0.84,
                       threshold.match = 0.90,
                       dedupe = FALSE)
  
  records.temp <- getMatches(dfA = data.temp,
                             dfB = data.temp,
                             fl.out = out.temp)
  
  records.temp$dedupe.ids <- paste0("B", i, "-", records.temp$dedupe.ids)
  
  results[[i]] <- records.temp
}

result_test <- do.call(rbind, results)

length(unique(result_test$dedupe.ids))

diag_fl(result_test)

# alternative approach of blocking is to utilise k-means algorithms to do the clustering automatically
blockbyK <- blockData(data, data, varnames = "by",
                      kmeans.block = "by", nclusters = 3)

resultsK <- list()
aggregate_link_model <- list()

for (k in 1:length(blockbyK)) {
  data.temp <- data[blockbyK[[k]]$dfA.inds, ] %>% select(-by2)
  print(head(data.temp, 10))
  out.temp <- fastLink(dfA = data.temp, dfB = data.temp,
                       varnames = linkageFields,
                       stringdist.match = stringDistFields,
                       partial.match = partialMatchFields,
                       cut.a = 0.92,
                       cut.p = 0.84,
                       threshold.match = 0.90,
                       dedupe = FALSE)
  
  records.temp <- getMatches(dfA = data.temp,
                             dfB = data.temp,
                             fl.out = out.temp)
  
  records.temp$dedupe.ids <- paste0("K", k, "-", records.temp$dedupe.ids)
  
  aggregate_link_model[[k]] <- out.temp
  resultsK[[k]] <- records.temp  
}

result_testK <- do.call(rbind, resultsK)

length(unique(result_testK$dedupe.ids))

diag_fl(result_testK)

agg.out <- aggregateEM(em.list = aggregate_link_model)

summary(agg.out)


################################################################################
################################################################################

## quick test on health and educ (surprisingly slow)
test_out <- fastLink(dfA = test_educ, dfB = test_health,
                     varnames = c("name", "date_of_birth", "gender", "address_street"),
                     stringdist.match = c("name", "date_of_birth", "address_street"),
                     partial.match = c("name", "date_of_birth", "address_street"),
                     cut.a = 0.92,
                     cut.p = 0.85,
                     threshold.match = 0.90)

test_matches <- getMatches(dfA = test_educ, dfB = test_health, fl.out = test_out)

summary(test_out)

pattern_match <- test_out$patterns

## function to produce different column a and column b side by side in one dataframe
col_combine_output <- function(linkage_data_output, dfa, dfb) {
  
  # filter linkage data for selected columns
  df <- data.frame(linkage_data_output["posterior"], linkage_data_output[["matches"]][["inds.a"]], linkage_data_output[["matches"]][["inds.b"]])
  
  names(df1) <- c("prob", "link_a_id", "link_b_id")
  
  df2 <- df1 %>% filter(link_a_id != link_b_id)
  
  data_A <- dfa %>% mutate(row_id_a = row_number())
  data_B <- dfb %>% mutate(row_id_b = row_number())
  
  merge1 <- inner_join(df2, data_A, by = join_by("link_a_id" == "row_id_a"))
  merge2 <- inner_join(merge1, data_B, by = join_by("link_b_id" == "row_id_b"))
  
  return(merge2)
} 

## quick test by blocking gender and seperating full name into first name and last name
## dissect address into home_address, zip code
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

###############################################################################
# Record linkage by gender #

block_gender <- blockData(dfA = educ_new, dfB = health_new, varnames = c("gender"))

resultsK <- list()
aggregate_link_model <- list()

for (block in seq_along(block_gender)) {
  
  data_temp_a <- educ_new[block_gender[[block]]$dfA.inds, ]
  data_temp_b <- health_new[block_gender[[block]]$dfB.inds, ]
  
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
  
  aggregate_link_model[[block]] <- out_temp
  resultsK[[block]] <- record_temp  
}

final_output <- do.call(rbind, resultsK)

agg.out <- aggregateEM(em.list = aggregate_link_model)

summary(agg.out)


###############################################################################
## K-means clustering blocking
kmeans_blocks <- blockData(dfA = educ_new, dfB = health_new, kmeans.block = c("year", "month"), 
                           varnames = c("year", "month"), nclusters = 5)

results_kmeans <- list()
aggregate_link_model_kmeans <- list()

for (block in seq_along(kmeans_blocks)) {
  
  data_temp_a <- educ_new[kmeans_blocks[[block]]$dfA.inds, ]
  data_temp_b <- health_new[kmeans_blocks[[block]]$dfB.inds, ]
  
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
  
  aggregate_link_model_kmeans[[block]] <- out_temp
  results_kmeans[[block]] <- record_temp  
}

final_output_kmeans <- do.call(rbind, results_kmeans)

agg.out.kmeans <- aggregateEM(em.list = aggregate_link_model_kmeans)

summary(agg.out.kmeans)


###############################################################################
# use address and zip code for matching #

health_address_clean <- clean_address(health_new, "address_street")
educ_address_clean <- clean_address(educ_new, "address_street")

educ_new_clean <- cbind(educ_new, educ_address_clean) %>% 
  select(-address_street) %>% 
  relocate(address_street_clean, zip_code, .after = gender)

health_new_clean <- cbind(health_new, health_address_clean) %>% 
  select(-address_street) %>% 
  relocate(address_street_clean, zip_code, .after = gender)


block_gender <- blockData(dfA = educ_new_clean, dfB = health_new_clean, varnames = c("gender"))

results_new <- list()
aggregate_link_model_new <- list()

for (block in seq_along(block_gender)) {
  
  data_temp_a <- educ_new_clean[block_gender[[block]]$dfA.inds, ]
  data_temp_b <- health_new_clean[block_gender[[block]]$dfB.inds, ]
  
  out_temp <- fastLink(dfA = data_temp_a, dfB = data_temp_b,
                       varnames = c("first_name","second_name", "last_name", "address_street_clean", "zip_code", "year", "month", "day"),
                       stringdist.match = c("first_name", "second_name", "last_name" ,"address_street_clean"),
                       partial.match = c("first_name", "second_name", "last_name"),
                       numeric.match = c( "year", "month", "day", "zip_code"),
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
  
  aggregate_link_model_new[[block]] <- out_temp
  results_new[[block]] <- record_temp  
}

final_output_new <- do.call(rbind, results_new)

agg_out <- aggregateEM(em.list = aggregate_link_model_new)

summary(agg_out)


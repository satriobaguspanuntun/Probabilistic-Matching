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

## quick test on health and educ
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
col_combine_output <- function(linkage_data_output) {
  
  # filter linkage data for selected columns
  df <- data.frame(linkage_data_output["posterior"], linkage_data_output[["matches"]][["inds.a"]], linkage_data_output[["matches"]][["inds.b"]])
  
  
} 

df1 <- data.frame(test_out["posterior"], test_out[["matches"]][["inds.a"]], test_out$matches$inds.b)

names(df1) <- c("prob", "link_a_id", "link_b_id")

df2 <- df1 %>% filter(link_a_id != link_b_id)

sd3 <- df1 %>% filter(link_a_id == link_b_id)

data_A <- test_educ %>% mutate(row_id_a = row_number())
data_B <- test_health %>% mutate(row_id_b = row_number())

merge1 <- inner_join(df2, data_A, by = join_by("link_a_id" == "row_id_a"))
merge2 <- inner_join(merge1, data_B, by = join_by("link_b_id" == "row_id_b"))

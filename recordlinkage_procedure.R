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







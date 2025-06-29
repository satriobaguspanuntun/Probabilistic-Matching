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









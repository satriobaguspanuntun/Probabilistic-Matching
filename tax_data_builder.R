# Probabilistic Linkage Project
# OBJECTIVE:
# 1. Generate fake datasets ranging from Tax, Health, Education, Migration and Economic Surveys
# 2. Building Spines as a central linkage IDs
# 3. Create insights and the benefits of having intergrated micro level dataset

# Fake Administrative Datasets Generator for Probabilistic Linkage
# Using charlatan package for realistic fake data generation
# Author: Data Linkage Project
# Date: 2025

library(tidyverse)
library(lubridate)
library(charlatan)

set.seed(12345)  # For reproducibility

# Parameters
n_individuals <- 10000  # Base population size

# Initialize charlatan providers
fraudster_cl <- fraudster("en_NZ")

# MASTER POPULATION 
generate_master_population <- function(n){
  
  data.frame(
    true_id = 1:n,
    name = fraudster_cl$name(n),
    date_of_birth = sample(seq(as.Date("1920-01-01"), as.Date("2008-12-31"), by = "day"), size = n, replace = TRUE),
    gender = sample(c("M", "F", "Other"),size = n, replace = TRUE, prob = c(0.40, 0.50, 0.10)),
    address_street = fraudster_cl$address(n),
    phone_number <- fraudster_cl$phone_number(n),
    email <- fraudster_cl$email(n)
  ) %>% 
    rename("phone_number" = `phone_number....fraudster_cl.phone_number.n.`,
           "email" = `email....fraudster_cl.email.n.`)
  
}

test <- generate_master_population(n_individuals)

# GENERATE TAX DATA 
generate_tax_data <- function(data, coverage = 0.85) {
  
  year_range <- paste0(seq(2015, 2024, by = 1))
  
  coverage_n <- floor(nrow(test) * 0.85) 
  id_randomiser <- sample(test$true_id, size = coverage_n)
  
  tax_builder <- test %>%
    filter(true_id %in% id_randomiser) %>% 
    mutate(tax_id  = as.numeric(paste0(fraudster_cl$integer(n = n(), min = 10000000, max = 99999999))),
           filling_status = sample(c("Single", "Married Filling Jointly", "Married Filling Seperately", "Head of Household"), size = n(), replace = TRUE, prob = c(0.4, 0.35, 0.10, 0.15)),
           num_dependents = sample(0:5, size = n(), replace = TRUE, prob = c(0.3, 0.25, 0.2, 0.15, 0.07, 0.03))
    )
  
  data_container <- list()
  
  for(i in seq_along(year_range)) {
    
    tax_per_year <- tax_builder %>% 
      mutate(tax_year = paste0(year_range[i]),
             gross_income = fraudster_cl$integer(n = n(), min = 5000, max = 3000000),
             tax_income = case_when(gross_income <= 35000 ~ gross_income * 0,
                                    gross_income > 35000 & gross_income <= 75000 ~ gross_income * 0.28,
                                    gross_income > 75000 & gross_income <= 98000 ~ gross_income * 0.30,
                                    gross_income > 98000 & gross_income <= 150000 ~ gross_income * 0.33,
                                    gross_income > 150000 ~ gross_income * 0.40,
                                    .default = NA))
    
    
    add_error_n <- floor(nrow(data)) * 0.05
    error_rows <- sample(1:add_error_n, size = add_error_n, replace = TRUE)
    
    for (k in error_rows) {
      
      error_type <- sample(1:3, 1)
      
      # address error
      if (error_type == 1) {
        tax_per_year$address_street[k] <- fraudster_cl$address(n = 1)
        
        # missing tax value 
      } else if (error_type == 2) {
        tax_per_year$gross_income[k] <- NA
        tax_per_year$tax_income[k] <- NA
        
        # typos in name 
      } else if (error_type == 3) {
        affected_name <- tax_per_year$name[i]
        prob_more_2_errors <- sample(1:2, size = 1, prob = c(0.85, 0.15))
        
        if (prob_more_2_errors == 1) {
          pos <- sample(2:(nchar(affected_name) - 1), 1)
          substr(affected_name, pos, pos) <- sample(letters, 1)
          tax_per_year$name[k] <- affected_name
          
        } else {
          pos <- sample(2:(nchar(affected_name) - 1), 2)
          tax_per_year$name[k] <- sapply(affected_name, function(x){
            chars <- strsplit(x, "")[[1]]
            chars[pos] <- sample(letters, length(pos), replace = TRUE)
            paste0(chars, collapse = "")
          })
        }
      }
    }
    
    data_container[[i]] <- tax_per_year
  }
  
  return(merge_data <- Reduce(bind_rows, data_container))
}

test_tax <- generate_tax_data(test)

# SOCIAL BENEFIT RECORDS
geneate_social_benefit <- function(data, coverage = 0.25) {
  
  
  
  
  
  
  
  
  
  
}

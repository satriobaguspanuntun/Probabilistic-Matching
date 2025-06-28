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
  
  coverage_n <- floor(nrow(data) * 0.85) 
  id_randomiser <- sample(data$true_id, size = coverage_n)
  
  tax_builder <- data %>%
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
        affected_name <- tax_per_year$name[k]
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
generate_social_benefit <- function(data, coverage = 0.25) {
  
  coverage_n <- floor(nrow(data) * 0.25) 
  id_randomiser <- sample(data$true_id, size = coverage_n)
  
  benefit_programs <- c("unemployment", "disability", "food assitance", "housing support")
  benefit <- c(700, 800, 1500, 2000)
  
  social_benefit_data <- data %>% 
    filter(true_id %in% id_randomiser) %>%
    mutate(
      program_enrolled = sample(benefit_programs, size = n(), prob = c(0.2, 0.1, 0.15, 0.55), replace = TRUE),
      benefit_amount = sample(benefit, size = n(), prob = c(0.2, 0.1, 0.15, 0.55), replace = TRUE),
      start_date = sample(seq.Date(from = ymd("2015-01-01"), to = ymd("2024-12-31"), by = "day"), size = n(), replace = TRUE),
      end_date = start_date + sample(30:1080, size = n(), replace = TRUE)
    ) %>%
    select(name, date_of_birth, gender, address_street, program_enrolled, benefit_amount, start_date, end_date)
  
  # add realistic error
  add_error_n <- floor(nrow(data)) * 0.05
  error_rows <- sample(1:add_error_n, size = add_error_n, replace = TRUE)
  
  for (k in error_rows) {
    
    error_type <- sample(1:2, 1)
      
      # missing benefit value 
    if (error_type == 1) {
      social_benefit_data$benefit_amount[k] <- NA
      
      # typos in name 
    } else if (error_type == 2) {
      affected_name <- social_benefit_data$name[k]
      prob_more_2_errors <- sample(1:2, size = 1, prob = c(0.85, 0.15))
      
      if (prob_more_2_errors == 1) {
        pos <- sample(2:(nchar(affected_name) - 1), 1)
        substr(affected_name, pos, pos) <- sample(letters, 1)
        social_benefit_data$name[k] <- affected_name
        
      } else {
        pos <- sample(2:(nchar(affected_name) - 1), 2)
        social_benefit_data$name[k] <- sapply(affected_name, function(x){
          chars <- strsplit(x, "")[[1]]
          chars[pos] <- sample(letters, length(pos), replace = TRUE)
          paste0(chars, collapse = "")
        })
      }
    }
  }
  return(social_benefit_data)
}

test_social <- generate_social_benefit(test)

# Health data include hospital region and hospital addressees
generate_health_data <- function(data, coverage = 0.75) {
  
  coverage_n <- floor(nrow(data) * coverage) 
  id_randomiser <- sample(data$true_id, size = coverage_n)
  
  # Define common diagnoses and procedures
  diagnosis_codes <- c("I10.9", "E11.9", "J45.909", "K21.9", "N18.9", "Z00.00", "R51", "M54.5") # Common ICD-10 like codes
  procedures <- c("Physical Exam", "Blood Test", "X-Ray", "MRI Scan", "Vaccination", "Minor Surgery")
  hospital_region <- c("Northland", "Auckland", "Waikato", "Bay of Plenty", "Gisborne", "Hawke's Bay",
                       "Taranaki", "Manawatū-Whanganui", "Wellington", "Tasman", "Nelson", "Marlborough", 
                       "West Coast", "Canterbury", "Otago", "Southland")
  hospital_name <- paste0(hospital_region, " ", "Hospital")
  healthcare_insurance <- c("Public Healthcare", "AIA", "Allianz", "Southern Cross", "AA Insurance", "NIB")
  
  health_data <- data %>% 
    filter(true_id %in% id_randomiser) %>% 
    mutate(patient_id = as.numeric(paste0(fraudster_cl$integer(n = n(), min = 100000, max = 999999))),
           diagnosis_type = sample(diagnosis_codes, size = n(), replace = TRUE, prob = c(0.2, 0.15, 0.1, 0.1, 0.05, 0.2, 0.1, 0.1)),
           procedure = sample(procedures, size = n(), replace = TRUE, prob = c(0.3, 0.25, 0.15, 0.1, 0.1, 0.1)),
           hospital_region = sample(hospital_region, size = n(), replace = TRUE, prob = c(0.05, 0.2, 0.05, 0.05, 0.05, 0.05,
                                                                                        0.03, 0.07, 0.1, 0.05, 0.02, 0.08, 0.05, 0.05, 0.04, 0.06)),
           hospital_name = sample(hospital_name, size = n(), replace = TRUE,  prob = c(0.05, 0.2, 0.05, 0.05, 0.05, 0.05,
                                                                                       0.03, 0.07, 0.1, 0.05, 0.02, 0.08, 0.05, 0.05, 0.04, 0.06)),
           insurance = sample(healthcare_insurance, size = n(), replace = TRUE, prob = c(0.7, 0.05, 0.05, 0.1, 0.05, 0.05)),
           visit_date = sample(seq.Date(from = ymd("2015-01-01"), to = ymd("2024-12-31"), by = "day"),size = n(), replace = TRUE)
           ) %>% 
    select(-true_id)
  
  # add realistic error
  add_error_n <- floor(nrow(data)) * 0.05
  error_rows <- sample(1:add_error_n, size = add_error_n, replace = TRUE)
  
  for (k in error_rows) {
    
    error_type <- sample(1:2, 1)
    
    # dob error
    if (error_type == 1) {
      dob_error_type <- sample(1:2, 1, prob = c(0.4, 0.6))
      
      # error in month
      if (dob_error_type == 1) {
        dob_row <- health_data$date_of_birth[k]
        print(k)
        print(dob_row)
        current_row_dob <- month(ymd(dob_row))
        print(current_row_dob)
        months_seq <- seq(from = 1, to = 12, by = 1)
        months_seq <- months_seq[!months_seq %in% current_row_dob]
        months_seq <- sapply(months_seq, FUN = function(x){ifelse(nchar(x) < 2, paste0("0", x), x)})
        new_dob_month <- sample(months_seq, size = 1, replace = TRUE)
        new_row_dob <- str_replace(dob_row, pattern = "-(\\d{2})-", paste0("-", new_dob_month, "-"))
        health_data$date_of_birth[k] <- ymd(new_row_dob)
        
      # error on the entirety of DOB
      } else {
        dob_row <- health_data$date_of_birth[k]
        date_seq <- seq(as.Date("1920-01-01"), as.Date("2008-12-31"), by = "day")
        date_seq <- date_seq[!date_seq %in% dob_row]
        new_row_dob <- sample(date_seq, replace = TRUE, size = 1)
        health_data$date_of_birth[k] <- new_row_dob
        
      }
      
      # typos in name 
    } else if (error_type == 2) {
      affected_name <- health_data$name[k]
      prob_more_2_errors <- sample(1:2, size = 1, prob = c(0.85, 0.15))
      
      if (prob_more_2_errors == 1) {
        pos <- sample(2:(nchar(affected_name) - 1), 1)
        substr(affected_name, pos, pos) <- sample(letters, 1)
        health_data$name[k] <- affected_name
        
      } else {
        pos <- sample(2:(nchar(affected_name) - 1), 2)
        health_data$name[k] <- sapply(affected_name, function(x){
          chars <- strsplit(x, "")[[1]]
          chars[pos] <- sample(letters, length(pos), replace = TRUE)
          paste0(chars, collapse = "")
        })
      }
    }
  }
  return(health_data)
}

test_health <- generate_health_data(test)

# education data generator
generate_education_data <- function(data, coverage = 0.90) {
  
  coverage_n <- floor(nrow(data) * coverage) 
  id_randomiser <- sample(data$true_id, size = coverage_n)
  
  # education information
  educ_level <-  c("Elementary", "Junior High", "Senior High", "Bachelor", "Master", "PhD")
  educ_field_bachelor_above <- c("Science", "Engineering", "Arts", "Commerce", "Law", "Medicine")
  educ_field_below <- c("Social Science", "Natural Science")
  scholarship <- c("Yes", "No")
  
  education_data <- data %>% 
    filter(true_id %in% id_randomiser) %>% 
    mutate(educ_id = paste0("EDUC_", fraudster_cl$integer(n = n(), min = 10000, max = 99999)),
           educ_levels = sample(educ_level, size = n(), replace = TRUE, prob = c(0.1, 0.1, 0.3, 0.2, 0.2, 0.01)),
           study_field_bachelor_above = ifelse(educ_levels %in% c("Bachelor", "Master", "PhD"), 
                                               sample(educ_field_bachelor_above, size = n(), replace = TRUE, 
                                                      prob = c(0.1, 0.2, 0.2, 0.3, 0.1, 0.1)),
                                               NA),
           study_field_below = ifelse(educ_levels %in% c("Elementary", "Junior High", "Senior High"), 
                                      sample(educ_field_below, size = n(), replace = TRUE, prob = c(0.6, 0.4)),
                                      NA),
           receive_scholarship = sample(scholarship, size = n(), replace = TRUE, prob = c(0.2, 0.8)))
  
  
  
  return(education_data)
}

test_educ <- generate_education_data(test)

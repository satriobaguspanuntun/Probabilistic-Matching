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

# Dedupe Tax Data









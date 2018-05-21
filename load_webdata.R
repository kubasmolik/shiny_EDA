data_source <- url("https://archive.ics.uci.edu/ml/machine-learning-databases/statlog/german/german.data")


read.table(data_source, header = F, sep = " ", stringsAsFactors = F) -> dane

names(dane) <- c("check_acc_status", "duration", "credit_history", "purpose",
                 "credit_amount", "savings", "unemployment", "ins_inc_ratio", 
                 "personal_status_sex", "other_debtors", "residence since", 
                 "property", "age", "other_ins_plans", "housing", 
                 "credits_number", "job", "liable_people", "telephone",
                 "foreign_worker", "default")

dane %>%
  mutate(check_acc_status = case_when(
    check_acc_status == "A11" ~ "less_0",
    check_acc_status == "A12" ~ "0_200",
    check_acc_status == "A13" ~ "200_more", 
    check_acc_status == "A14" ~ "no_account"  
  )) %>%
  mutate(credit_history = case_when(
    credit_history == "A30" ~ "no_credits",
    credit_history == "A31" ~ "past_paid_duly" ,
    credit_history == "A32" ~ "existing_paid_duly" ,
    credit_history == "A33" ~ "past_delays" ,
    credit_history == "A34" ~ "critical_account" 
  )) %>%
  mutate(purpose = case_when(
    purpose == "A40" ~ "new_car" ,
    purpose == "A41" ~ "used_car" ,
    purpose == "A42" ~ "furniture", 
    purpose == "A43" ~ "radio_tv" ,
    purpose == "A44" ~ "domestic_appliances" ,
    purpose == "A45" ~ "repairs" ,
    purpose == "A46" ~ "education" ,
    purpose == "A47" ~ "vacation" ,
    purpose == "A48" ~ "retraining", 
    purpose == "A49" ~ "business" ,
    purpose == "A410" ~ "others" 
  )) %>%
  mutate(savings = case_when(
    savings == "A61" ~ "less_100",
    savings == "A62" ~ "100_500",
    savings == "A63" ~ "500_1000", 
    savings == "A64" ~ "1000_more",
    savings == "A65" ~ "unknown"
  )) %>%
  mutate(unemployment = case_when(
    unemployment == "A71" ~ "unemployed", 
    unemployment == "A72" ~ "less_1" ,
    unemployment == "A73" ~ "1_4" ,
    unemployment == "A74" ~ "4_7" ,
    unemployment == "A75" ~ "7_more"
  )) %>%
  mutate(personal_status_sex = case_when(
    personal_status_sex == "A91" ~ "male_div_sep",
    personal_status_sex == "A92" ~ "female_div_sep_mar" ,
    personal_status_sex == "A93" ~ "male_sin",
    personal_status_sex == "A94" ~ "male_mar_wid" ,
    personal_status_sex == "A95" ~ "female_sin" 
  )) %>%
  mutate(other_debtors = case_when(
    other_debtors == "A101" ~ "none" ,
    other_debtors == "A102" ~ "coapplicant" ,
    other_debtors == "A103" ~ "guarantor"
  )) %>%
  mutate(property = case_when(
    property == "A121" ~ "real_estate",
    property == "A122" ~ "life_insurance" ,
    property == "A123" ~ "car_other" ,
    property == "A124" ~ "unknown" 
  )) %>%
  mutate(other_ins_plans = case_when(
    other_ins_plans == "A141" ~ "bank", 
    other_ins_plans == "A142" ~ "stores",
    other_ins_plans == "A143" ~ "none"
  )) %>%
  mutate(housing = case_when(
    housing == "A151" ~ "rent",
    housing == "A152" ~ "own", 
    housing == "A153" ~ "for_free" 
  )) %>%
  mutate(job = case_when(
    job == "A171" ~ "unemployed", 
    job == "A172" ~ "unskilled", 
    job == "A173" ~ "skilled_employee", 
    job == "A174" ~ "management" 
  )) %>%
  mutate(telephone = case_when(
    telephone == "A191" ~ "no", 
    telephone == "A192" ~ "yes"
  )) %>%
  mutate(foreign_worker = case_when(
    foreign_worker == "A201" ~ "yes", 
    foreign_worker == "A202" ~ "no"
  )) %>%
  mutate(default = default - 1) %>%
  select(default, check_acc_status:foreign_worker) -> dane

data.table::fwrite(dane, 
                   file = "C:/BD/german_credit/german_credit.csv", 
                   sep = "|",
                   row.names = F)


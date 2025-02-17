setwd("~/Library/CloudStorage/OneDrive-UniversityofStrathclyde/Fifth Year/MS984/Datasets")

library(tidyverse)

# 2021 data

df2021 <- read.table(("shes21i_eul.tab"), sep = "\t", header = T) %>% select(Cpseriala, SYear, psu, Strata, int21wt, cint21wt, bmi_adj, SlfHtDV_adj , SlfWtDV_adj, bmivg5_adj, CBMIg5_new_SR, age, Sex, simd20_sga, Ethnic05, totinc, eqv5_15, hedqul08)

names(df2021) <- c("id", "year", "psu", "strata", "int_wt", "cint_wt", "bmi", "ht", "wt", "bmi_class", "child_bmi_class", "age", "sex", "imd", "ethnic", "income", "eqv_income", "educ")

# 2020 data

# Telephone survey lots missing

df2020 <- read.table(("shes20_tel_w1_eul_15092021.tab"), sep = "\t", header = T) %>% select(CPSerialA, PSU, Strata, int20wt, bmi_adj, SlfHtDV_adj, SlfWtDV_adj, bmivg5_adj, age90, Sex, SIMD20_SGa, Ethnic05_t20)

names(df2020) <- c("id", "psu", "strata", "int_wt", "bmi", "ht", "wt", "bmi_class", "age", "sex", "imd", "ethnic")

df2020$year <- 13

df2020$cint_wt <- 1

df2020$income <- NA

df2020$eqv_income <- NA

df2020$educ <- NA

df2020$child_bmi_class <- NA


# 2019 data

df2019 <- read.table("shes19i_eul.tab", sep = "\t", header = T) %>% select(CPSerialA, SYear, PSU, Strata, int19wt, cint19wt, bmival, htval, wtval, BMIvg5, CBMIg5_new, age, Sex, SIMD20_SGa, Ethnic05, totinc, eqv5_15, hedqul08)

names(df2019) <- c("id", "year", "psu", "strata", "int_wt", "cint_wt", "bmi", "ht", "wt", "bmi_class", "child_bmi_class", "age", "sex", "imd", "ethnic", "income", "eqv_income", "educ")

# 2018 data

df2018 <- read.table("shes_18i_eul_v3.tab", sep = "\t", header = T) %>% select(CPSerialA, SYear,PSU, Strata, int18wt, cint18wt, bmival, htval, wtval, bmivg5, CBMIg5_new, age, Sex, SIMD16_SGa, Ethnic05, totinc, eqv5_15, hedqul08)

names(df2018) <- c("id", "year", "psu", "strata", "int_wt", "cint_wt", "bmi", "ht", "wt", "bmi_class", "child_bmi_class", "age", "sex", "imd", "ethnic", "income", "eqv_income", "educ")

# 2017 data

df2017 <- read.table("shes_17i_archive_v1.tab", sep = "\t", header = T) %>% select(CPSerialA, SYear,PSU, Strata, int17wt, cint17wt, bmival, htval, wtval, bmivg5, CBMIg5_new, age, Sex, SIMD16_SGa, Ethnic05, totinc, eqv5_15, hedqul08)

names(df2017) <- c("id", "year", "psu", "strata", "int_wt", "cint_wt", "bmi", "ht", "wt", "bmi_class", "child_bmi_class", "age", "sex", "imd", "ethnic", "income", "eqv_income", "educ")

# 2016 data

df2016 <- read.table("shes16i_archive_v1.tab", sep = "\t", header = T) %>%  select(cpserialA, SYear,PSU, Strata, int16wt, cint16wt, bmival, htval, wtval, bmivg5, CBMIg5_new, age, Sex, SIMD16_SGa, Ethnic05, totinc, eqv5_15, hedqul08)

names(df2016) <- c("id", "year", "psu", "strata", "int_wt", "cint_wt", "bmi", "ht", "wt", "bmi_class", "child_bmi_class", "age", "sex", "imd", "ethnic", "income", "eqv_income", "educ")

# 2015 data

df2015 <- read.table("shes15i_archive_v1.tab", sep = "\t", header = T)  %>% select(cpserialA, SYear,psu, STRATA, int15wt, cint15wt, bmival, htval, wtval, bmivg5, CBMIg5_new, age, Sex, SIMD5_SGa, Ethnic05, totinc, eqv5_15, hedqul08)

names(df2015) <- c("id", "year", "psu", "strata", "int_wt", "cint_wt", "bmi", "ht", "wt", "bmi_class", "child_bmi_class", "age", "sex", "imd", "ethnic", "income", "eqv_income", "educ")

# 2014 data

df2014 <- read.table("shes14i_archive_v6.tab", sep = "\t", header = T)  %>% select(cpserialA, SYear,psu, Strata, int14wt, cint14wt, bmival, htval, wtval, bmivg5, CBMIg5_new, age, Sex, SIMD5_SGa, Ethnic05, totinc, eqv5_15, hedqul08)

names(df2014) <- c("id", "year", "psu", "strata", "int_wt", "cint_wt", "bmi", "ht", "wt", "bmi_class", "child_bmi_class", "age", "sex", "imd", "ethnic", "income", "eqv_income", "educ")

# 2013 data

df2013 <- read.table("shes13i_archive_v6.tab", sep = "\t", header = T)  %>% select(CpserialA, SYear,psu, STRATA, int13wt, cint13wt, bmival, htval, wtval, bmivg5, CBMIg5_new, age, Sex, SIMD5_SGa, Ethnic12, totinc, eqv5_15, hedqul08)

names(df2013) <- c("id", "year", "psu", "strata", "int_wt", "cint_wt", "bmi", "ht", "wt", "bmi_class", "child_bmi_class", "age", "sex", "imd", "ethnic", "income", "eqv_income", "educ")

# 2012 data
# there are issue with the bmival variable in this year where some individuals have valid height and weight but no bmi, so we need to recalculate it

df2012 <- read.table("shes12i_archive_v5.tab", sep = "\t", header = T)  %>% select(cpserialA, SYear,psu, Strata, int12wt, cint12wt, bmival, htval, wtval, bmivg5, CBMIg5_new, age, Sex, SIMD5_SGa, Ethnic12, totinc, eqv5_15, hedqul08)

names(df2012) <- c("id", "year", "psu", "strata", "int_wt", "cint_wt", "bmi", "ht", "wt", "bmi_class", "child_bmi_class", "age", "sex", "imd", "ethnic", "income", "eqv_income", "educ")

df2012$bmi <- case_when( (df2012$wt > 0 & df2012$ht>0) ~ df2012$wt/df2012$ht/df2012$ht*10000,
                         TRUE ~ -1)

# 2011 data

# no survey year

df2011 <- read.table("shes11i_v7.tab", sep = "\t", header = T)  %>% select(pserialA,psu, strata, int11wt, cint11wt, bmival, htval, wtval, bmivg5, CBMIg5, age, Sex, SIMD5_SG, Ethnic09, totinc, eqv5_15, hedqul08)

df2011$year <- 4

names(df2011) <- c("id", "psu", "strata", "int_wt", "cint_wt", "bmi", "ht", "wt", "bmi_class", "child_bmi_class", "age", "sex", "imd", "ethnic", "income", "eqv_income", "educ", "year")

# 2010 data

df2010 <- read.table("shes10i_v5.tab", sep = "\t", header = T)  %>% select(pserialA,psu, strata, int10wt, cint10wt, bmival, htval, wtval, bmivg5, CBMIg5, age, Sex, SIMD5_SG, ethgroup, totinc, eqv5_15, hedqul08)

df2010$year <- 3

names(df2010) <- c("id", "psu", "strata", "int_wt", "cint_wt", "bmi", "ht", "wt", "bmi_class", "child_bmi_class", "age", "sex", "imd", "ethnic", "income", "eqv_income", "educ", "year")

# 2009 data

df2009 <- read.table("shes09i_v6.tab", sep = "\t", header = T)  %>% select(pserialA,psu, strata, int09wt, cint09wt, bmival, htval, wtval, bmivg5, CBMIg5, age, Sex, SIMD5_SG, Ethnic09, totinc, eqv5_15, hedqul08)

df2009$year <- 2

names(df2009) <- c("id", "psu", "strata", "int_wt", "cint_wt", "bmi", "ht", "wt", "bmi_class", "child_bmi_class", "age", "sex", "imd", "ethnic", "income", "eqv_income", "educ", "year")

# 2008 data

df2008 <- read.table("shes08i_v12.tab", sep = "\t", header = T)  %>% select(pserialA,psu, strata, int08wt, cint08wt, bmival, htval, wtval, bmivg5, CBMIg5, age, Sex, SIMD5_SG, EthnicI, totinc, eqv5_15, hedqul08)

df2008$year <- 1

names(df2008) <- c("id", "psu", "strata", "int_wt", "cint_wt", "bmi", "ht", "wt", "bmi_class", "child_bmi_class", "age", "sex", "imd", "ethnic", "income", "eqv_income", "educ", "year")

# 2003 data

# no children BMI
df2003 <- read.table("shs03i_revised.tab", sep = "\t", header = T)  %>% select(PSERIAL,psu, strata, int_wt, cint_wt, bmival, htval, wtval, bmivg5, age, Sex, simd5, EthnicI, totinc, eqv5, hedqual)

df2003$year <- -4

df2003$child_bmi_class <- -2

names(df2003) <- c("id", "psu", "strata", "int_wt", "cint_wt", "bmi", "ht", "wt", "bmi_class", "age", "sex", "imd", "ethnic", "income", "eqv_income", "educ", "year", "child_bmi_class")

# 1998 data

# no split between children and adult weight, bmi grouped in 6 categories, carstair index instead of multiple deprivation, no income, no education

df1998 <- read.table("shs98i.tab", sep = "\t", header = T)  %>% 
  select(archsn,psu, region,  bmival, htval, wtval, bmivg6, age, sex, carstg5, ethnic)

df1998$year <- -9

df1998$int_wt <- 1

df1998$cint_wt <- 1

df1998$income <- NA

df1998$eqv_income <- NA

df1998$educ <- NA

df1998$child_bmi_class <- NA

names(df1998) <- c("id", "psu", "strata", "bmi", "ht", "wt", "bmi_class", "age", "sex", "imd", "ethnic", "year", "int_wt", "cint_wt", "income", "eqv_income", "educ", "child_bmi_class")

# 1995 Data

# no derived weight and height variables

df1995 <- read.table("shs95i.tab", sep = "\t", header = T)  %>% 
  select(serialx,psu, region, bmiok, bmi, htok, wtok, height, weight, bmiag1, respage, respsex, cargp5, ethnic)

df1995$htval <- ifelse(df1995$htok == 1, df1995$height, -1)

df1995$wtval <- ifelse(df1995$htok == 1, df1995$weight, -1)

df1995$bmival <- ifelse(df1995$bmiok == 1, df1995$bmi, -1)

df1995 <- df1995 %>% select(serialx, psu, region, bmival, htval, wtval, bmiag1, respage, respsex, cargp5, ethnic)

names(df1995) <- c("id", "psu", "strata", "bmi", "ht", "wt", "bmi_class", "age", "sex", "imd", "ethnic")

df1995$year <- -12

df1995$int_wt <- 1

df1995$cint_wt <- 1

df1995$income <- NA

df1995$eqv_income <- NA

df1995$educ <- NA

df1995$child_bmi_class <- NA

# Combined data

combined_df <- bind_rows(df2021, df2020, df2019, df2018, df2017, df2016, df2015, df2014, df2013, 
                         df2012, df2011, df2010, df2009, df2008, df2003, df1998, df1995)

combined_df <- combined_df %>%
  mutate(year = year + 2007)

combined_df[combined_df < 0] <- NA

write.csv(combined_df, "~/Library/CloudStorage/OneDrive-UniversityofStrathclyde/Fifth Year/MS984/Datasets/shes9821.csv", row.names = FALSE)


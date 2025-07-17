library(pacman)
p_load("tidyverse",
       "here",
       "survey",
       "rineq")
#Load the data####
df <- read.csv(here("data","Ayushper.csv"))
df1 <- read.csv(here("data","79AyushHH.csv"))
# selecting the variables
df1_s <- df1 %>%
  select(st, dc, strm, fsu, su, sd, sss, ssu, bl04i10,mult)
df_s <- df |> 
  select(st, dc, strm, fsu, su, sd, sss, ssu,bl03c04,
         bl03c05,bl03c06, bl03c07,religion,sec,social,
         bl03c09,bl03c10)
#Merging data set####
df2 <- df_s %>%
  left_join(df1_s, by = c("st", "dc", "strm", "fsu", "su", "sd", "sss", "ssu"))

#view####
glimpse(df2)

# Missing values
missing_counts <- colSums(is.na(df2))

# Filter to only those columns with missing values
missing_columns <- missing_counts[missing_counts > 0]

# Display list of columns with missing values and their counts
print(missing_columns)

#Data wrangling####
names(df2)
df2 |> count(bl03c04)
df2 |> count(bl03c06)


##Gender####
df2 <- df2 |> 
  mutate ( gender = case_when(
    bl03c04 == 1 ~ "Male",
    bl03c04 == 2 ~ "Female",
    bl03c04 == 3 ~ "others"))
##Marital status#### 
df2 <- df2 |>
  mutate(marital = case_when(
    bl03c06 == 1 ~ "unmarried",
    bl03c06 == 2 ~ "married",
    bl03c06 %in% c(3, 4) ~ "others"
  ))
##Age category####
df2 <- df2 |> 
  mutate(age_cat = case_when(
    bl03c05 < 5 ~ "Below 5 years",
    bl03c05 >=5 & bl03c05 <= 14 ~ " 5-14 years",
    bl03c05 >=15 & bl03c05 <= 29 ~ " 15- 29 years",
    bl03c05 >=30 & bl03c05 <= 44 ~ " - 29 years",
    bl03c05 >=44 & bl03c05 <= 64 ~ " 15- 29 years",
    bl03c05 >= 65 ~ "Above 60 years"), .before =  bl03c05 )

#count
df2 |> count(age_cat)

##Education level####
df2 <- df2 |> 
  mutate(edu_cat = case_when(
    bl03c07 <= 2 ~ "No formal Education",
    bl03c07 > 2 & bl03c07 <= 5 ~ " Primary ",
    bl03c07 >= 6 & bl03c07 <=10  ~ " Secondary",
    bl03c07 >= 11  ~ "Tertiary"), .before =  bl03c07 )

df2 |> count(edu_cat)

## Religion####
df2<- df2 |>
  mutate(reli = case_when(
    religion == 1 ~ "Hindu",
    religion == 2 ~ "Islam",
    religion== 3 ~ "Christianity",
    TRUE ~ "others"
  ), .before = religion) 
## MPCE cat####

summary(df2$bl04i10)
df2 <- df2 %>%
  mutate(
    mpce = case_when(
      bl04i10 < 8333 ~ "Lowest",
      bl04i10 >= 8333 & bl04i10 < 11867 ~ "Second",
      bl04i10 >= 11867 & bl04i10 < 13899 ~ "Middle",
      bl04i10 >= 13899 & bl04i10 < 17190 ~ "Fourth",
      bl04i10 >= 17190 ~ "Highest"
    ),
    mpce = factor(mpce, levels = c("Lowest", "Second", "Middle", "Fourth", "Highest"))
  )


df2 <- df2 |>
  mutate(
    mpce = ntile(bl04i10, 5),  # Divides into 5 equal groups
    mpce= factor(mpce,
                levels = 1:5,
                labels = c("Lowest", "Second", "Middle", "Fourth", "Highest"))
  ) 
## Regions####
 df2 <- df2 |> 
  mutate(region = case_when(
    st %in% c(1, 2, 3, 6, 7, 4,8,5,37) ~ "North",         # Jammu & Kashmir, Himachal, Punjab, Haryana, Delhi, Chandigarh
    st %in% c(9, 10, 20, 21, 22, 23) ~ "Central",   # UP, Bihar, Jharkhand, Odisha, Chhattisgarh, MP
    st %in% c(18, 19) ~ "East",                    # Assam, West Bengal
    st %in% c(24, 27, 29, 30,25) ~ "West",            # Gujarat, Maharashtra, Karnataka, Goa
    st %in% c(11, 12, 13, 14, 15, 16, 17) ~ "Northeast",  # NE states: Arunachal, Nagaland, Manipur, etc.
    st %in% c(28, 31, 32, 33, 34, 36, 35) ~ "South"  # TN, Kerala, AP, Telangana, Puducherry, Lakshadweep
  ),.before = st)

df2 |> count(region)

##Ayush Used#### 
df2 <- df2 %>%
  mutate(ayush_used = ifelse(bl03c10 == 1, 1, 0))

# gender
# rank
df2 <- df2 |> 
    arrange(bl04i10) |> 
   mutate(rank = cumsum(as.numeric(mult)) / sum(as.numeric(mult)))

df2 <- df2 |>
  arrange(bl04i10) |> 
  mutate(
    rank = cumsum(as.numeric(mult)) / sum(as.numeric(mult)),     # weighted cumulative rank
    weight_norm = mult / sum(mult),      # normalized weights
    y = ayush_used                      # the health variable (e.g., 0/1)
  )

# CI formula: 2 * cov(y, rank) / mean(y)
ci <- 2 * cov(df2$y, df2$rank, use = "complete.obs") / mean(df2$y, na.rm = TRUE)






#Data labelling####

# df2 <- df2 %>%
#   labelled::set_variable_labels(
#     bl04i10 = "Monthly per-capita expenditure (MPCE)",
#     religion = "Religion",
#     social = "Social group",
#     sector = "Residence",
#     bl03c04 = "Gender",
#     bl03c05 = "Age",
#     bl03c06 = "Marital status",
#     bl03c07 = "Highest educational level",
#     st = "State")

#Exploring the variables####
table(df2$bl03c10)
# relveling
df2$bl03c09 <- factor(df2$bl03c09)                # Ensure it's a factor
df2$bl03c09 <- relevel(df2$bl03c09, ref = "2")

df2$gender <- relevel(factor(df2$gender), ref = "Male")
df2$age_cat <- relevel(factor(df2$age_cat), ref = "Below 5 years")
df2$edu_cat <- relevel(factor(df2$edu_cat), ref = "No formal Education")
df2$reli<- relevel(factor(df2$reli), ref = "Hindu")
df2$sec <- relevel(factor(df2$sec), ref = "1")
df2$region <- relevel(factor(df2$region), ref = "Northeast")
df2$social <- relevel(factor(df2$social), ref = "1")
df2$marital <- relevel(factor(df2$marital), ref = "unmarried")


design <- svydesign(ids = ~1, data = df2, weights = ~mult)


#Descriptive Statistics###
###weighted proportion- ayush used####
svymean(~ayush_used, design)

svymean(~factor(region),design)
svymean(~factor(age_cat),design)
svymean(~factor(mpce),design)
glimpse(df2)

vars <- c("mpce", "gender", "age_cat", "marital", "edu_cat",
          "reli", "sec", "region", "social", "bl03c09")

for (var in vars) {
  cat("\n---Weighted proportions for:", var, "\n---")
  print(svymean(as.formula(paste0("~factor(", var, ")")), design, na.rm = TRUE))
}

### count for all variables #### 
for (var in vars) {
  cat("\n---Frequency table for:", var, "---\n")
  print(table(df2[[var]], useNA = "ifany"))
}


## 2*2 table percentage#####
svyby( ~factor(ayush_used),~mpce, design, svymean)
svyby( ~factor(ayush_used),~factor(gender), design, svymean)
svyby( ~factor(ayush_used),~age_cat, design, svymean)
svyby(~factor(ayush_used),~marital,  design, svymean)
svyby( ~factor(ayush_used),~edu_cat, design, svymean)
svyby( ~factor(ayush_used),~reli, design, svymean)
svyby(~factor(ayush_used), ~sec, design, svymean)
svyby(~factor(ayush_used),~region, design, svymean)
svyby(~factor(ayush_used), ~social, design, svymean)
svyby(~factor(ayush_used), ~ bl03c09, design, svymean)


### alternative ####
group_vars <- c("mpce", "gender", "age_cat", "marital", "edu_cat",
                "reli", "sec", "region", "social", "bl03c09")

for (grp in group_vars) {
  cat("\nWeighted proportion of ayush_used by:", grp, "\n")
  result <- svyby(
    formula = ~factor(ayush_used),
    by = as.formula(paste0("~", grp)),
    design = design,
    FUN = svymean,
    na.rm = TRUE
  )
  print(result)
}

## Chisquare#####
svychisq(~ayush_used + sec, design)
svychisq(~ayush_used + social, design)

# List of predictor variable names
predictors <- c("mpce", "gender", "age_cat", "marital", "edu_cat",
                "reli", "sec", "region", "social", "bl03c09")

# Loop to run svychisq for each predictor
for (var_name in predictors) { 
  
  # Create formula: ayush_used + variable
  chi_formula <- as.formula(paste("~ayush_used +", var_name))
  
  # Run survey-weighted chi-square test
  cat("\n--- Chi-square Test for:", var_name, "---\n")
  print(svychisq(chi_formula, design = design))
}

##Univariate Analysis####
m1 <- svyglm(ayush_used ~ bl03c09, design = design, family = quasibinomial())
summary(m1)
exp(coef(m1))
exp(confint(m1))

m2 <- svyglm(ayush_used ~ region, design = design, family = quasibinomial())
summary(m2)
exp(coef(m2))
exp(confint(m2))

m3 <- svyglm(ayush_used ~ sec, design = design, family = quasibinomial())
summary(m3)
exp(coef(m3))
exp(confint(m3))


m4 <- svyglm(ayush_used ~ reli, design = design, family = quasibinomial())
summary(m4)
exp(coef(m4))
exp(confint(m4))


m5 <- svyglm(ayush_used ~ social, design = design, family = quasibinomial())
summary(m5)
exp(coef(m5))
exp(confint(m5))

m6 <- svyglm(ayush_used ~ mpce, design = design, family = quasibinomial())
summary(m6)
exp(coef(m6))
exp(confint(m6))

m7 <- svyglm(ayush_used ~ edu_cat, design = design, family = quasibinomial())
summary(m7)
exp(coef(m7))
exp(confint(m7))

m8 <- svyglm(ayush_used ~ age_cat, design = design, family = quasibinomial())
summary(m8)
exp(coef(m8))
exp(confint(m8))

m9 <- svyglm(ayush_used ~ marital, design = design, family = quasibinomial())
summary(m9)
exp(coef(m9))
exp(confint(m9))

m10 <- svyglm(ayush_used ~ gender, design = design, family = quasibinomial())
summary(m10)
exp(coef(m10))
exp(confint(m10))

### Alternate for univariate analysis####
predictors <- c("mpce", "gender", "age_cat", "marital", "edu_cat",
                "reli", "sec", "region", "social", "bl03c09")

for (var in predictors) {
  cat("\nLogistic regression for predictor:", var, "\n")
  
  # Create model formula: ayush_used ~ var
  formula <- as.formula(paste0("ayush_used ~ ", var))
  
  # Fit the model using svyglm
  model <- svyglm(formula, design = design, family = quasibinomial())
  
  # Print model summary
  print(summary(model))
  
  # Print Odds Ratios
  cat("\nOdds Ratios (exp(coef)):\n")
  print(exp(coef(model)))
  
  # Print 95% Confidence Intervals
  cat("\n95% CI:\n")
  print(exp(confint(model)))
}

## Multivarible model- poison model#####

poisson_model <- svyglm(
  ayush_used ~ gender + age_cat + mpce + edu_cat + marital + reli + sec + region + social,
  design = design,
  family = poisson(link = "log")
)
summary(poisson_model)
exp(coef(poisson_model))        
exp(confint(poisson_model))  

df2 |> count(region)
###CI#####

 ci_result_1 <- ci(
   ineqvar = df2$rank,  # MPCE used for ranking
   outcome = df2$ayush_used, # Proper numeric 0/1 variable
  weights = df2$mult,           # Survey weights
   type = "CIw",                 # Wagstaff Index for binary outcomes
    method = "linreg_delta",      # Accurate method with SE
    robust_se = TRUE,             # Optional robust SE
 rse_type = "HC3"              # Default SE type
  )
summary(ci_result_1)
plot(ci_result_1)

##stratified by sector

 ci_2 <- df2 |>
     group_split(sec) |>
      map(~ ci(
          ineqvar = .$mpce,
         outcome = .$ayush_used,
         weights = .$mult,
          type = "CIw",
          method = "linreg_delta",
          robust_se = TRUE
       ))
  summary(ci_2)
  summary(ci_2[[1]])

plot(ci_2[[1]])

summary(ci_2[[2]])

plot(ci_2[[2]])


# stratified by region
ci_3 <- df2 |>
  group_split(region) |>
  map(~ ci(
    ineqvar = .$mpce,
    outcome = .$ayush_used,
    weights = .$mult,
    type = "CIw",
    method = "linreg_delta",
    robust_se = TRUE
  ))
summary(ci_3)
#north
summary(ci_3[[3]])

plot(ci_3[[3]])

 
 # Cross tab #####
 tbl_summary(
   df2,by = bl03c10,
   include = c( gender,
                age_cat,
                edu_cat,
                mpce,
                marital,
                reli,
                sec,
                region,
                social, bl03c09
   ),statistic = list(all_categorical() ~ "{n} ({p}%)"))
#Descriptive Statistics####
 library(gtsummary)
# Cross tab #####
 tbl_summary(
   df2,by = bl03c10,
   include = c( gender,
                age_cat,
                edu_cat,
                mpce,
                marital,
                reli,
                sec,
                region,
                social
   ),
   statistic = list(all_categorical() ~ "{n} ({p}%)"))


 
 

####----------------------------------------------------------------------------
#### CULTURAL GENOCIDE - A GLOBAL PERSPECTIVE
####
#### ANALYSIS
#### 
#### Jan
####
#### 01-02/2025
####----------------------------------------------------------------------------

##---- PACKAGES ----------------------------------------------------------------
library(tidyverse)
library(ggplot2)
library(ggthemes)
library(psych)
library(sandwich)
library(lmtest)
##------------------------------------------------------------------------------

##---- DATA --------------------------------------------------------------------
df_cg <- readxl::read_xlsx("CG_policy_years.xlsx")%>%
  rename_with(~ paste0("rev_", .), .cols = 4:12) %>%
  mutate(across(4:12, ~ replace_na(., 0))) %>%
  mutate(any_cg = as.integer(if_any(4:12, ~ . == 1)))
df_nbp <- haven::read_dta("NBP_groups_final.dta")
df_polity <- readxl::read_xlsx("POLITY5_annual.xlsx") %>%
  select(c(iso3c, Year, Polity2)) %>%
  rename(iso3 = iso3c)
df_cc <- countrycode::codelist %>%
  select(c(country.name.en, iso3c, un.region.name, un.regionintermediate.name, un.regionsub.name)) %>%
  rename(iso3 = iso3c)
df_pwt <- readxl::read_excel("pwt1001.xlsx", sheet = 3) %>%
  select(c(countrycode, year, pop, rgdpe, rgdpo, rgdpna)) %>%
  rename(iso3 = countrycode,
         Year = year)
##------------------------------------------------------------------------------

##---- DATA PREPARATION --------------------------------------------------------
summary(df_cg)
summary(df_nbp)
summary(df_polity)
summary(df_cc)


df_nbpcg <- df_nbp %>%
  left_join(df_cg, by = c("Country", "Group", "Year"))

df_complete <- df_nbpcg %>%
  left_join(df_polity, by = c("iso3", "Year")) %>%
  distinct()

summary(df_complete)

df_complete <- df_complete %>%
  left_join(df_cc, by = "iso3")

df_complete <- df_complete %>%
  mutate(
    un.region.name = case_when(
      iso3 == "CSK" ~ "Europe",
      iso3 == "DDR"  ~ "Europe",
      iso3 == "XKX" ~ "Europe",
      iso3 == "YPR" ~ "Asia",
      iso3 == "YAR" ~ "Asia",
      iso3 == "VDR" ~ "Asia",
      iso3 == "TWN" ~ "Asia",
      iso3 == "SUN" ~ "Europe",
      iso3 == "YUG" ~ "Europe",
      TRUE ~ un.region.name
    ),
    un.regionsub.name = case_when(
      iso3 == "CSK" ~ "Eastern Europe",
      iso3 == "DDR"  ~ "Western Europe",
      iso3 == "XKX" ~ "Southern Europe",
      iso3 == "YPR" ~ "Western Asia",
      iso3 == "YAR" ~ "Western Asia",
      iso3 == "VDR" ~ "South-eastern Asia",
      iso3 == "TWN" ~ "Eastern Asia",
      iso3 == "SUN" ~ "Eastern Europe",
      iso3 == "YUG" ~ "Southern Europe",
      TRUE ~ un.regionsub.name  
    )
  )

summary(df_complete)

df_complete <- df_complete %>%
  mutate(across(starts_with("Arrived"), ~ifelse(is.na(.), 0, .)),
         any_cg = ifelse(is.na(any_cg), 0, any_cg))

summary(as.factor(df_complete$any_cg))

df_complete <- df_complete %>%
  group_by(un.region.name, Year) %>%
  mutate(reg_cg_occurrence = sum(any_cg)) %>%
  ungroup()

df_complete <- df_complete %>%
  left_join(df_pwt, by = c("iso3", "Year"))

#create CG onset flag

df_complete <- df_complete %>%
  arrange(Country, Group, Year)

df_complete <- df_complete %>%
  group_by(Country, Group) %>%
  mutate(any_cg_onset_flag = ifelse(any_cg == 1 & lag(any_cg, default = 0) == 0, 1, 0)) %>%
  ungroup()

df_test <- df_complete %>%
  select(c(Country, Group, Year, any_cg, any_cg_onset_flag)) %>%
  filter(any_cg == 1)

df_complete <- df_complete %>%
  mutate(SDM = ifelse(SDM == 99, 0, SDM))

df_complete <- df_complete %>%
  arrange(Country, Group, Year) %>%
  group_by(Country, Group) %>%
  mutate(SDM_lag = lag(SDM, order_by = Year),
         SDM_2y_lag = lag(SDM, order_by = Year, n = 2)) %>%
  ungroup()

##---- EDA ---------------------------------------------------------------------

#---- CHECK VARS ---------------------------------------------------------------

summary(as.factor(df_complete$IndigGp))
summary(as.factor(df_complete$SpatialConc))

summary(as.factor(df_complete$ViolenceAgainstGroup))
summary(as.factor(df_complete$ViolenceDuringWar))
print(df_complete$ViolenceDuringWar)
summary(as.factor(df_complete$LowLevelViolence))
print(df_complete$LowLevelViolence)
summary(as.factor(df_complete$MassViolence))
print(df_complete$MassViolence)

summary(as.factor(df_complete$SDM))
print(df_complete$SDM)

summary(as.factor(df_complete$any_cg))

summary(as.factor(df_complete$Relocation))
print(df_complete$Relocation)

df_complete <- df_complete %>%
  mutate(relocation_dummy = case_when(Relocation == 0 ~ 0,
                                      Relocation > 0 ~ 1))

summary(as.factor(df_complete$relocation_dummy))

summary(as.factor(df_complete$SpatialSeg))

#---- BIVARIATE TESTS ----------------------------------------------------------

#contingency tables
ct1 <- table(df_complete$any_cg, df_complete$IndigGp)
print(ct1)

ct2 <- table(df_complete$any_cg, df_complete$SpatialConc)
print(ct2)

ct3 <- table(df_complete$any_cg, df_complete$MigrantBackground)
print(ct3)

ct4 <- table(df_complete$any_cg, df_complete$ViolenceAgainstGroup)
print(ct4)

ct5 <- table(df_complete$any_cg, df_complete$SDM)
print(ct5)

ct6 <- table(df_complete$any_cg, df_complete$Relocation)
print(ct6)

ct7 <- table(df_complete$any_cg, df_complete$SpatialSeg)
print(ct7)

ct8 <- table(df_complete$any_cg_onset_flag, df_complete$SDM)
print(ct8)

ct9 <- table(df_complete$any_cg_onset_flag, df_complete$SDM_lag)
print(ct9)

ct10 <- table(df_complete$any_cg_onset_flag, df_complete$SDM_2y_lag)
print(ct10)

#Phi coeffs
c_tables <- list(ct1 = ct1, ct2 = ct2, ct3 = ct3, ct4 = ct4, ct5 = ct5, ct7 = ct7, ct8 = ct8, ct9 = ct9, ct10 = ct10)

phi_coeffs <- list()

for(table_name in names(c_tables)) {
    phi_coeffs[[table_name]] <- phi(c_tables[[table_name]])
    
  }

for(table_name in names(phi_coeffs)) {
  print(paste("Phi coeffs. for", table_name, ":", round(phi_coeffs[[table_name]], 3)))
}



#---- PLOT ---------------------------------------------------------------------
ggplot(df_complete, aes(x = Year, y = reg_cg_occurrence, color = un.region.name)) +
  geom_point() +
  geom_line() +
  labs(
    title = "Regional CG Occurrence",
    x = "Year",
    y = "Sum Incidence CG",
    color = "Region"
  ) +
  theme_clean() +
  theme(legend.position = "bottom")

#---- INITIAL MODELS -----------------------------------------------------------

m1 <- glm(any_cg ~ SDM_lag +
            MigrantBackground +
            IndigGp +
            SpatialConc +
            SizeEst +
            Polity2 +
            log(pop) +
            log(rgdpe),
          data = df_complete,
          family = binomial())
summary(m1)

m1_rse <- coeftest(m1, vcov = vcovHC(m1, type = "HC0"))
print(m1_rse)

m1_cse <- coeftest(m1, vcov = vcovCL(m1, cluster = df_complete$Country, type = "HC0"))
print(m1_cse)

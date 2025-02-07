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
##------------------------------------------------------------------------------

##---- DATA PREPARATION --------------------------------------------------------
summary(df_cg)
summary(df_nbp)
summary(df_polity)


df_nbpcg <- df_nbp %>%
  left_join(df_cg, by = c("Country", "Group", "Year"))

df_complete <- df_nbpcg %>%
  left_join(df_polity, by = c("iso3", "Year")) %>%
  distinct()

##---- EDA ---------------------------------------------------------------------
####----------------------------------------------------------------------------
#### CULTURAL GENOCIDE - A GLOBAL PERSPECTIVE
#### 
#### Jan
####
#### 01/2025
####----------------------------------------------------------------------------

##---- PACKAGES ----------------------------------------------------------------
library(tidyverse)
##------------------------------------------------------------------------------

##---- DATA --------------------------------------------------------------------
df_cg <- readxl::read_xlsx("CG_recoded_final.xlsx")
df_nbp <- haven::read_dta("NBP_groups_final.dta")
##------------------------------------------------------------------------------

##---- DATA PREPARATION --------------------------------------------------------

#from periods to policy-group-country-year format

expand_periods <- function(periods) {
  periods <- gsub("\\*", "", periods) 
  period_list <- unlist(strsplit(periods, ";\\s*")) 
  
  all_years <- unlist(lapply(period_list, function(period) {
    if (grepl("-", period)) { 
      range <- as.numeric(unlist(strsplit(period, "-")))
      if (length(range) == 2 && all(!is.na(range))) {
        return(seq(range[1], range[2]))
      }
    } else {
      year <- as.numeric(period)
      if (!is.na(year)) {
        return(year)
      }
    }
    return(NULL)  
  }))
  
  return(unique(all_years))  
}

# expand dataframe
df_cg_long <- df_cg %>%
  rowwise() %>%
  mutate(Year = list(expand_periods(Periods))) %>%
  unnest(Year) %>%
  mutate(Policy = as.factor(Policy), Value = 1) %>%
  select(Country, Group, Year, Policy, Value)

# observation period

df_cg_long1945to2020 <- df_cg_long %>%
  filter(Year >= 1945,
         Year <= 2020)

# policies as columns

policy_names <- unique(df_cg$Policy)

print(policy_names)

df_cg_policies <- df_cg_long1945to2020 %>%
  pivot_wider(names_from = Policy, values_from = Value)

openxlsx::write.xlsx(df_cg_policies, "CG_policies_annual.xlsx")




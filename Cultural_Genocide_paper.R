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

ggplot(df_complete, aes(x = Year, y = any_cg, color = un.region.name)) +
  stat_summary(fun = mean, geom = "line", size = 1) +  
  stat_summary(fun = mean, geom = "point", size = 2) + 
  labs(
    title = "Occurrence of any_cg Over Time by Region",
    x = "Year",
    y = "Proportion of any_cg",
    color = "Region"
  ) +
  theme_clean() +
  scale_x_continuous(breaks = seq(min(df_complete$Year), max(df_complete$Year), by = 5)) +
  theme(legend.position = "bottom")


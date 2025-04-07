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
library(stringr)
library(ggplot2)
library(ggthemes)
library(fmsb)
library(psych)
library(sandwich)
library(lmtest)
library(logistf)
library(brglm2)
library(fixest)
library(car)
##------------------------------------------------------------------------------

##---- DATA --------------------------------------------------------------------
df_cg <- readxl::read_xlsx("CG_policy_years_dec24.xlsx")%>%
  rename_with(~ paste0("rev_", .), .cols = 4:13) %>%
  mutate(across(4:13, ~ replace_na(., 0))) %>%
  mutate(any_cg = as.integer(if_any(4:13, ~ . == 1)))
df_nbp <- haven::read_dta("NBP_groups_final.dta")
df_polity <- readxl::read_xlsx("POLITY5_annual.xlsx") %>%
  select(c(iso3c, Year, Polity2)) %>%
  rename(iso3 = iso3c)
#df_cc <- countrycode::codelist %>%
#  select(c(country.name.en, iso3c, un.region.name, un.regionintermediate.name, un.regionsub.name)) %>%
#  rename(iso3 = iso3c)
df_pwt <- readxl::read_excel("pwt1001.xlsx", sheet = 3) %>%
  select(c(countrycode, year, pop, rgdpe, rgdpo, rgdpna)) %>%
  rename(iso3 = countrycode,
         Year = year)
df_vdem_regions <- readRDS("V-Dem-CY-Full+Others-v15.rds") %>%
  select(c(country_text_id, year, e_regionpol_7C)) %>%
  rename(iso3 = country_text_id,
         Year = year)

##------------------------------------------------------------------------------

##---- DATA PREPARATION --------------------------------------------------------
summary(df_cg)
summary(df_nbp)
summary(df_polity)
#summary(df_cc)
summary(df_vdem_regions)

df_nbpcg <- df_nbp %>%
  left_join(df_cg, by = c("Country", "Group", "Year"))

df_complete <- df_nbpcg %>%
  left_join(df_polity, by = c("iso3", "Year")) %>%
  distinct()

summary(df_complete)

#df_complete <- df_complete %>%
#  left_join(df_cc, by = "iso3")

# df_complete <- df_complete %>%
#   mutate(
#     un.region.name = case_when(
#       iso3 == "CSK" ~ "Europe",
#       iso3 == "DDR"  ~ "Europe",
#       iso3 == "XKX" ~ "Europe",
#       iso3 == "YPR" ~ "Asia",
#       iso3 == "YAR" ~ "Asia",
#       iso3 == "VDR" ~ "Asia",
#       iso3 == "TWN" ~ "Asia",
#       iso3 == "SUN" ~ "Europe",
#       iso3 == "YUG" ~ "Europe",
#       TRUE ~ un.region.name
#     ),
#     un.regionsub.name = case_when(
#       iso3 == "CSK" ~ "Eastern Europe",
#       iso3 == "DDR"  ~ "Western Europe",
#       iso3 == "XKX" ~ "Southern Europe",
#       iso3 == "YPR" ~ "Western Asia",
#       iso3 == "YAR" ~ "Western Asia",
#       iso3 == "VDR" ~ "South-eastern Asia",
#       iso3 == "TWN" ~ "Eastern Asia",
#       iso3 == "SUN" ~ "Eastern Europe",
#       iso3 == "YUG" ~ "Southern Europe",
#       TRUE ~ un.regionsub.name  
#     )
#   )
# 
# summary(df_complete)

df_complete <- df_complete %>%
  left_join(df_vdem_regions, by = c("iso3", "Year"))

df_complete <- df_complete %>%
  rename(vdem_region = e_regionpol_7C)

df_complete <- df_complete %>%
  mutate(vdem_region = case_when(
    iso3 == "CSK" ~ 1,             # Eastern Europe
    iso3 == "DDR" ~ 5,             # Western Europe and North America
    iso3 == "SUN" ~ 1,             # Eastern Europe
    iso3 == "VDR" ~ 6,             # East Asia and the Pacific
    iso3 %in% c("XKX", "YUG") ~ 1,  # Eastern Europe
    iso3 %in% c("YAR", "YPR") ~ 3,  # Middle East and North Africa
    TRUE ~ vdem_region
  )
  )

df_complete <- df_complete %>%
  mutate(vdem_region_name = case_when(
    vdem_region == 1 ~ "Eastern Europe",
    vdem_region == 2 ~ "Latin America and the Caribbean",
    vdem_region == 3 ~ "The Middle East and North Africa",
    vdem_region == 4 ~ "Sub-Saharan Africa",
    vdem_region == 5 ~ "Western Europe and North America",
    vdem_region == 6 ~ "East Asia and the Pacific",
    vdem_region == 7 ~ "South and Central Asia"
  ))


df_complete <- df_complete %>%
  mutate(across(starts_with("Arrived"), ~ifelse(is.na(.), 0, .)),
         any_cg = ifelse(is.na(any_cg), 0, any_cg))

summary(as.factor(df_complete$any_cg))

df_complete <- df_complete %>%
  group_by(vdem_region_name, Year) %>%
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

#create CG types vars ------ NOT WORKING YET
df_complete <- df_complete %>%
  group_by(Country, Group) %>%
  mutate(across(
    starts_with("rev_"),
    ~ ifelse(.x == 1 & lag(.x, default = 0) == 0, 1, 0),
    .names = "{.col}_onset_flag_part"
  )) %>%
  ungroup()

df_test2 <- df_complete %>%
  select(c(Country, Group, Year, ends_with("onset_flag_part")))

df_complete %>%
  summarise(across(
    ends_with("_onset_flag_part"),
    ~ sum(.x, na.rm = TRUE)
  ))

onset_summary <- df_complete %>%
  summarise(across(
    ends_with("_onset_flag_part"),
    ~ sum(.x, na.rm = TRUE)
  )) %>%
  pivot_longer(
    cols = everything(),
    names_to = "Type",
    values_to = "Onset_Count"
  )
###### BELOW WORKING AGAIN

df_complete <- df_complete %>%
  mutate(SDM = ifelse(SDM == 99, 0, SDM))

df_complete <- df_complete %>%
  arrange(Country, Group, Year) %>%
  group_by(Country, Group) %>%
  mutate(SDM_lag = lag(SDM, order_by = Year),
         SDM_2y_lag = lag(SDM, order_by = Year, n = 2)) %>%
  ungroup()

df_nochina <- df_complete %>%
  filter(iso3 != "CHN")

df_noextr <- df_nochina %>%
  filter(iso3 != "SUN")

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

ct11 <- table(df_complete$any_cg, df_complete$ViolenceAgainstGroup)
print(ct11)

phi(ct11)

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
ggplot(df_complete, aes(x = Year, y = reg_cg_occurrence, color = vdem_region_name)) +
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

ggplot(df_complete, aes(x = as.factor(any_cg), y = Polity2)) +
  geom_boxplot() +
  labs(x = "any_cg", y = "Polity2") +
  theme_clean()

ggplot(df_complete, aes(x = as.factor(any_cg_onset_flag), y = Polity2)) +
  geom_boxplot() +
  labs(x = "any_cg", y = "Polity2") +
  theme_clean()

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

influencePlot(m1, main = "Influence Plot")


m1_re <- feglm(any_cg ~ SDM_lag +
                  #MigrantBackground +
                  IndigGp +
                  SpatialConc +
                  SizeEst +
                  Polity2 +
                  log(pop) +
                  log(rgdpe) | Country,
                data = df_complete,
                family = binomial(),
                method = "brglmFit")

summary(m1_re)

modelsummary::modelsummary(m1_re, output = "m1_re_test2.html", statistic = "{p.value}")


m2_re <- feglm(any_cg_onset_flag ~ SDM_lag +
                 MigrantBackground +
                 IndigGp +
                 SpatialConc +
                 SizeEst +
                 Polity2 +
                 log(pop) +
                 log(rgdpe) | Country,
               data = df_complete,
               family = binomial(),
               method = "brglmFit")

summary(m2_re)


m3_re <- feglm(ViolenceAgainstGroup ~ SDM_lag +
                 MigrantBackground +
                 IndigGp +
                 SpatialConc +
                 SizeEst +
                 Polity2 +
                 log(pop) +
                 log(rgdpe) | Country,
               data = df_complete,
               family = binomial(),
               method = "brglmFit")

summary(m3_re)


m3_re <- feglm(any_cg ~ ViolenceAgainstGroup +
                 SDM_lag +
                 MigrantBackground +
                 IndigGp +
                 SpatialConc +
                 SizeEst +
                 Polity2 +
                 log(pop) +
                 log(rgdpe) | Country,
               data = df_complete,
               family = binomial(),
               method = "brglmFit")

summary(m3_re)


modelsummary::modelsummary(m3_re, output = "m1_re_test3.html", statistic = "{p.value}")


## 


##---- MAPPING CULTURAL GENOCIDE -----------------------------------------------

#---- NUMBERS

#no. of countries and groups
df_complete$Country <- str_squish(df_complete$Country)
n_distinct(df_complete$Country)

n_distinct(df_cg$Country) # CG occurred in 43 Countries

n_distinct(paste(df_cg$Country, df_cg$Group, sep = "_")) # 106 Groups experienced CG


#no. of events per type

# a) total incidence for each type:

l_cg_incidence <- colSums(df_cg[, 4:13], na.rm = TRUE)
df_cg_incidence <- cg_data <- as.data.frame(t(l_cg_incidence))

cg_data <- rbind(
  rep(max(df_cg_incidence) * 1.1, length(df_cg_incidence)),  
  rep(0, length(df_cg_incidence)),                  
  df_cg_incidence                                    
)


radarchart(
  cg_data,
  axistype = 1,
  pcol = "firebrick",
  pfcol = rgb(1, 0, 0, 0.3),
  plwd = 2,
  cglcol = "grey",
  cglty = 1,
  axislabcol = "grey",
  caxislabels = NULL,
  vlcex = 0.8
)

title("Total Incidence of Cultural Genocide Types")

png("p4_CG_types_incidence.png", width = 800, height = 600)


#cases of each type

df_cg_long <- df_cg %>%
  pivot_longer(cols = 4:13, 
               names_to = "genocide_type", 
               values_to = "numbers")

df_cg_long <- df_cg_long %>%
  arrange(Country, Group, genocide_type, Year)

df_cg_long <- df_cg_long %>%
  group_by(Country, Group, genocide_type) %>%
  mutate(
    lag_val = lag(numbers, default = 0),
    new_episode = numbers == 1 & (lag_val == 0 | Year - lag(Year, default = first(Year)) > 1),
    episode_id = cumsum(replace_na(new_episode, 0))
  ) %>%
  ungroup()

cg_counts <- df_cg_long %>%
  filter(numbers == 1 & new_episode) %>%
  count(genocide_type, name = "n_cases")

print(cg_counts)

cg_counts %>%
  ggplot(aes(x = reorder(genocide_type, n_cases), y = n_cases)) +
  geom_col(fill = "firebrick") +
  coord_flip() +
  labs(
    title = "CG episodes by Type",
    x = "Type",
    y = "Number"
  ) +
  theme_clean()
ggsave("CG_types_count.png", width = 12, height = 6)


# radar chart 

cg_counts_types <- cg_counts$n_cases
genocide_types <- cg_counts$genocide_type

# Convert the counts into a transposed data frame
df_cg_types <- as.data.frame(t(cg_counts_types))

# Add rows for min and max scaling
df_cg_types <- rbind(
  rep(max(df_cg_types) * 1.1, length(df_cg_types)),  # Max row (for scaling)
  rep(0, length(df_cg_types)),  # Min row (0 for scaling)
  df_cg_types  # Actual data row for the counts
)

colnames(df_cg_types) <- c(genocide_types)

# Create the radar chart
radarchart(
  df_cg_types,
  axistype = 1,
  pcol = "darkblue",  # Line color
  pfcol = rgb(0, 0, 1, 0.3),  # Fill color
  plwd = 2,  # Line width
  cglcol = "grey",  # Grid line color
  cglty = 1,  # Grid line type (solid)
  axislabcol = "grey",  # Axis label color
  caxislabels = NULL,  # Remove axis labels
  vlcex = 0.8  # Variable label size
)

title("Cases per Type of Cultural Genocide Types")





#---- ONSETS PER DECADE

df_complete <- df_complete %>%
  mutate(decade = case_when(
    Year >= 1945 & Year <= 1949 ~ "1945–1949",
    Year >= 1950 & Year <= 1959 ~ "1950–1959",
    Year >= 1960 & Year <= 1969 ~ "1960–1969",
    Year >= 1970 & Year <= 1979 ~ "1970–1979",
    Year >= 1980 & Year <= 1989 ~ "1980–1989",
    Year >= 1990 & Year <= 1999 ~ "1990–1999",
    Year >= 2000 & Year <= 2009 ~ "2000–2009",
    Year >= 2010 & Year <= 2020 ~ "2010–2020",
    TRUE ~ NA_character_
  ))

df_cg_onsets_by_decade <- df_complete %>%
  group_by(decade) %>%
  summarise(num_onsets = sum(any_cg_onset_flag, na.rm = TRUE)) %>%
  arrange(decade)

ggplot(df_cg_onsets_by_decade, aes(x = decade, y = num_onsets)) +
  geom_col(fill = "firebrick") +
  labs(
    title = "CG Onsets by Decade",
    x = "Decade",
    y = "Number of Onsets"
  ) +
  theme_clean()
ggsave("CG_onset_decades.png", width = 8, height = 6)

#---- REGIONAL BREAKDOWN

#No. of cases per region



#No. of type per region



#No. of cases per region relative to number of group
df_complete <- df_complete %>%
  group_by(vdem_region_name, Year) %>%
  mutate(reg_number_groups = n_distinct(Group)) %>%
  ungroup()

summary(df_complete$reg_number_groups)

df_complete <- df_complete %>%
  mutate(reg_cg_relative = reg_cg_occurrence / reg_number_groups)

summary(df_complete$reg_cg_relative)

ggplot(df_complete, aes(x = Year, y = reg_cg_relative, color = vdem_region_name)) +
  geom_point() +
  geom_line() +
  labs(
    title = "Regional CG incidence rel. to no. groups",
    x = "Year",
    y = "Relative Incidence CG",
    color = "Region"
  ) +
  theme_clean() +
  theme(legend.position = "bottom",
        legend.text = element_text(size = 8))
ggsave("CG_incidence_regional_rel.png", width = 10, height = 6)

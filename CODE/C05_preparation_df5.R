#### FIRST LOOK of df_5 ####

str(df_5_camp_cat)
summary(df_5_camp_cat)

#### START CLEANING df_5 ####

df_5_camp_cat_clean <- df_5_camp_cat %>%
                        mutate(ID_CAMP = as.character(ID_CAMP)) %>%
                        mutate(TYP_CAMP = as.factor(TYP_CAMP)) %>%
                        mutate(CHANNEL_CAMP = as.factor(CHANNEL_CAMP))

str(df_5_camp_cat_clean)
summary(df_5_camp_cat_clean)

#### CLEANING LOW VARIANCE in df_5 ####

df_5_camp_cat_clean <- df_5_camp_cat_clean %>%
  select(-CHANNEL_CAMP)

#### FINAL REVIEW df_5_clean ####

str(df_5_camp_cat_clean)
summary(df_5_camp_cat_clean)

#### REMOVAL OF INTERMEDIATE DATASETS
rm(df_5_camp_cat)
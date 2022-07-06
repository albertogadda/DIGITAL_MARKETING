#### FIRST LOOK of df_4 ####

str(df_4_cli_privacy)
summary(df_4_cli_privacy)

#### START CLEANING df_4 ####

df_4_cli_privacy_clean <- df_4_cli_privacy

#### CLEANING DUPLICATE VALUES in df_4 ####

## check for duplicates
df_4_cli_privacy_clean %>%
  summarise(TOT_ID_CLIs = n_distinct(ID_CLI)
            , TOT_ROWs = n())

#!!! NOTE:  no duplicates !!!#

#### CLEANING DATA TYPES in df_4 ####

## formatting column types ##
df_4_cli_privacy_clean <- df_4_cli_privacy_clean %>%
  mutate(FLAG_PRIVACY_1 = as.factor(FLAG_PRIVACY_1)) %>%
  mutate(FLAG_PRIVACY_2 = as.factor(FLAG_PRIVACY_2)) %>%
  mutate(FLAG_DIRECT_MKT = as.factor(FLAG_DIRECT_MKT)) %>%
  mutate(ID_CLI = as.character(ID_CLI))

#### CONSISTENCY CHECK ID_CLI in df_1/df_4 ####

cons_idcli_df1_df4 <- df_1_cli_fid_clean %>%
  select(ID_CLI) %>%
  mutate(is_in_df_1 = 1) %>%
  distinct() %>%
  full_join(df_4_cli_privacy_clean %>%
              select(ID_CLI) %>%
              mutate(is_in_df_4 = 1) %>%
              distinct()
            , by = "ID_CLI"
  ) %>%
  group_by(is_in_df_1, is_in_df_4) %>%
  summarise(NUM_ID_CLIs = n_distinct(ID_CLI)) %>%
  as.data.frame()

cons_idcli_df1_df4

#!!! NOTE: all ID_CLI in df_1 are also in df_4 and vice-versa !!!#

#### EXPLORE COLUMNS of df_4 ####

#### ???? TO DO df_4 ???? ####
# EXPLORE the df_4_cli_privacy_clean relevant variables

str(df_4_cli_privacy_clean)

#### variable FLAG_PRIVACY_1 ####

table(df_4_cli_privacy_clean$FLAG_PRIVACY_1) / sum(table(df_4_cli_privacy_clean$FLAG_PRIVACY_1))

#!!! NOTE: the first privacy setting applies to 65.6% of customers !!!#

#### variable FLAG_PRIVACY_2 ####

table(df_4_cli_privacy_clean$FLAG_PRIVACY_2) / sum(table(df_4_cli_privacy_clean$FLAG_PRIVACY_2))

#!!! NOTE: the second privacy setting applies to 93.6% of customers !!!#
## this variable may not be very useful due to its low variance ##

cor(as.integer(df_4_cli_privacy_clean$FLAG_PRIVACY_1), as.integer(df_4_cli_privacy_clean$FLAG_PRIVACY_2))

#!!! NOTE: there is a slight negative correlation between the two privacy settings (-4%) !!!#

#### variable FLAG_DIRECT_MKT ####

table(df_4_cli_privacy_clean$FLAG_DIRECT_MKT) / sum(table(df_4_cli_privacy_clean$FLAG_DIRECT_MKT))

#!!! NOTE: FLAG_DIRECT_MKT is set to 'yes' for 67.1% of customers !!!#

#### FINAL REVIEW df_4_clean ####

str(df_4_cli_privacy_clean)
summary(df_4_cli_privacy_clean)

#### REMOVAL OF INTERMEDIATE DATASETS
rm(df_4_cli_privacy)
rm(cons_idcli_df1_df4)
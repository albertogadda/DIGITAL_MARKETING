#### FIRST LOOK of df_2 ####

str(df_2_cli_account)
summary(df_2_cli_account)

#### START CLEANING df_2 ####

df_2_cli_account_clean <- df_2_cli_account

#### CLEANING DUPLICATE VALUES in df_2 ####

## check for duplicates
df_2_cli_account_clean %>%
  summarise(TOT_ID_CLIs = n_distinct(ID_CLI)
            , TOT_ROWs = n())

#!!! NOTE:  no duplicates !!!#

#### CLEANING DATA TYPES in df_2 ####

## format ID_CLI as character ##

df_2_cli_account_clean <- df_2_cli_account_clean %>%
  mutate(ID_CLI = as.character(ID_CLI)) %>%
  mutate(ID_ADDRESS = as.character(ID_ADDRESS))

## format columns as factor ##
df_2_cli_account_clean <- df_2_cli_account_clean %>%
  mutate(W_PHONE = as.factor(W_PHONE)) %>%
  mutate(TYP_CLI_ACCOUNT = as.factor(TYP_CLI_ACCOUNT)) %>%
  mutate(TYP_JOB = as.factor(TYP_JOB))

#### CLEANING MISSING VALUES in df_2 ####

## MISSING VALUES mapped as natural values ##
df_2_cli_account_clean <- df_2_cli_account_clean %>%
  mutate(W_PHONE = fct_explicit_na(W_PHONE, "0"))

#### CONSISTENCY CHECK ID_CLI in df_1/df_2 ####

cons_idcli_df1_df2 <- df_1_cli_fid_clean %>%
  select(ID_CLI) %>%
  mutate(is_in_df_1 = 1) %>%
  distinct() %>%
  full_join(df_2_cli_account_clean %>%
              select(ID_CLI) %>%
              mutate(ID_CLI = as.character(ID_CLI)) %>%
              mutate(is_in_df_2 = 1) %>%
              distinct()
            , by = "ID_CLI"
  ) %>%
  group_by(is_in_df_1, is_in_df_2) %>%
  summarise(NUM_ID_CLIs = n_distinct(ID_CLI)) %>%
  as.data.frame()

cons_idcli_df1_df2

#!!! NOTE: all ID_CLI in df_1 are also in df_2 and vice-versa !!!#

#### EXPLORE COLUMNS of df_2 ####

### Variable EMAIL_PROVIDER ###

## compute distribution
df_2_dist_emailprovider <- df_2_cli_account_clean %>%
  group_by(EMAIL_PROVIDER) %>%
  summarise(TOT_CLIs = n_distinct(ID_CLI)) %>%
  mutate(PERCENT = TOT_CLIs/sum(TOT_CLIs)) %>%
  arrange(desc(PERCENT)) %>%
  as.data.frame()

df_2_dist_emailprovider

tot_emailproviders <- n_distinct(df_2_dist_emailprovider$EMAIL_PROVIDER)

tot_emailproviders

#!!! NOTE: too many different values for EMAIL_PROVIDER to be an useful category !!!#

#### ???? TO DO df_2 ???? ####
# COMPUTE THE DISTRIBUTION for the remaining df_2_cli_fid_clean variables

#### RESHAPING df_2 ####

## keep the most frequent EMAIL_PROVIDER values and add a common factor level "OTHER" for the remaining ##
df_2_dist_emailprovider %>%
  arrange(desc(PERCENT)) %>%
  mutate(PERCENT_COVERED = cumsum(TOT_CLIs)/sum(TOT_CLIs)) %>%
  as.data.frame() %>%
  head(20)

## always keep the (missing) level for technical reasons
## select levels that cover the 85% of the cases, the remaining 15% 
clean_email_providers <- df_2_dist_emailprovider %>%
  arrange(desc(PERCENT)) %>%
  mutate(PERCENT_COVERED = cumsum(TOT_CLIs)/sum(TOT_CLIs)) %>%
  mutate(EMAIL_PROVIDER = as.character(EMAIL_PROVIDER)) %>%
  mutate(AUX = if_else(PERCENT_COVERED < 0.85 | (PERCENT_COVERED > 0.85 & lag(PERCENT_COVERED) < 0.85), 1,0)) %>%
  mutate(EMAIL_PROVIDER_CLEAN = if_else(AUX | EMAIL_PROVIDER == "(missing)", EMAIL_PROVIDER, "others"))

head(clean_email_providers, 20)

## add clean EMAIL_PROVIDER ##
df_2_cli_account_clean <- df_2_cli_account_clean %>%
  mutate(EMAIL_PROVIDER = as.character(EMAIL_PROVIDER)) %>%
  left_join(clean_email_providers %>%
              select(EMAIL_PROVIDER, EMAIL_PROVIDER_CLEAN)
            , by = "EMAIL_PROVIDER") %>%
  select(-EMAIL_PROVIDER) %>%
  mutate(EMAIL_PROVIDER_CLEAN = as.factor(EMAIL_PROVIDER_CLEAN))


#### EXPLORE NEW COLUMNS EMAIL_PROVIDER_CLEAN in df_2 ####

## compute distribution
df_2_dist_emailproviderclean <- df_2_cli_account_clean %>%
  group_by(EMAIL_PROVIDER_CLEAN) %>%
  summarise(TOT_CLIs = n_distinct(ID_CLI)) %>%
  mutate(PERCENT = TOT_CLIs/sum(TOT_CLIs)) %>%
  arrange(desc(PERCENT))

df_2_dist_emailproviderclean

## plot distribution
plot_df_2_dist_emailproviderclean <- (
  ggplot(data=df_2_dist_emailproviderclean
         , aes(x=EMAIL_PROVIDER_CLEAN, y=TOT_CLIs)) +
    geom_bar(stat="identity"
             , fill="steelblue") +
    theme_minimal()
)

plot_df_2_dist_emailproviderclean

#### ???? TO DO df_2 ???? ####
# EXPLORE the remaining df_2_cli_account_clean relevant variables

#### variable W_PHONE ####

table(df_2_cli_account_clean$W_PHONE)/sum(as.integer(df_2_cli_account_clean$W_PHONE))

#!!! NOTE: 86.2% of clients specified their phone number !!!#

#### variable ID_ADDRESS ####

df_2_address_distrib = df_2_cli_account_clean %>%
      group_by(ID_ADDRESS = as.integer(ID_ADDRESS)) %>%
      summarise(N_DIST_CLI = n_distinct(ID_CLI)) %>%
      filter(N_DIST_CLI > 1) %>%
      arrange(desc(N_DIST_CLI)) %>%
  left_join(df_3_cli_address %>%
              distinct()
            , by = "ID_ADDRESS")
      
#!!! NOTE: the most common ID_ADDRESS (900091) probably is an artificial placeholder for NA !!!#

df_2_cli_account_clean %>% summarise(n_distinct(ID_ADDRESS)/n())

#!!! NOTE: 97.8% of clients live in a distinct ID_ADDRESS !!!#

#### variable TYP_CLI_ACCOUNT ####

    df_2_cli_account_clean %>%
        select(ID_ADDRESS, TYP_CLI_ACCOUNT) %>%
         group_by(ID_ADDRESS) %>%
         summarise(N_DIST = n_distinct(TYP_CLI_ACCOUNT)) %>%
         filter(N_DIST > 1)

#!!! NOTE: the placeholder ID_ADDRESS (900091) is the only one to have multiple TYP_CLI_ACCOUNT related to it !!!#

df_1_cli_fid_clean %>% left_join(df_2_cli_account_clean %>% select(ID_CLI, TYP_CLI_ACCOUNT), by= 'ID_CLI') %>% select(LAST_COD_FID,TYP_CLI_ACCOUNT) %>% distinct()

#!!! NOTE: TYP_CLI_ACCOUNT is not related to the client's business type !!!#

## in the next script we will investigate whether TYP_CLI_ACCOUNT is determined by the customer's location ##

#### FINAL REVIEW df_2_clean ####

str(df_2_cli_account_clean)
summary(df_2_cli_account_clean)

#### REMOVAL OF INTERMEDIATE DATASETS
rm(df_2_address_distrib)
rm(df_2_cli_account)
rm(df_2_dist_emailprovider)
rm(df_2_dist_emailproviderclean)
rm(clean_email_providers)
rm(plot_df_2_dist_emailproviderclean)
rm(cons_idcli_df1_df2)
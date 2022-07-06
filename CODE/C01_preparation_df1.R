#### FIRST LOOK of df_1 ####

str(df_1_cli_fid)
summary(df_1_cli_fid)

#### START CLEANING df_1 ####

df_1_cli_fid_clean <- df_1_cli_fid

#### CLEANING DUPLICATE VALUES in df_1 ####

## check for duplicates
df_1_cli_fid_clean %>%
  summarise(TOT_ID_CLIs = n_distinct(ID_CLI)
            , TOT_ID_FIDs = n_distinct(ID_FID)
            , TOT_ID_CLIFIDs = n_distinct(paste0(as.character(ID_CLI),"-",as.character(ID_FID)))
            , TOT_ROWs = n())

#!!! NOTE:  no duplicates for combination CLI-FID !!!#

#### CLEANING DATA TYPES in df_1 ####

## formatting dates ##
df_1_cli_fid_clean <- df_1_cli_fid_clean %>%
  mutate(DT_ACTIVE = as.Date(DT_ACTIVE))

## formatting boolean as factor ##
df_1_cli_fid_clean <- df_1_cli_fid_clean %>%
  mutate(TYP_CLI_FID = as.factor(TYP_CLI_FID)) %>%
  mutate(STATUS_FID = as.factor(STATUS_FID))

#### CONSISTENCY CHECK on df1: number of fidelity subscriptions per client ####

## count the subscriptions for each client
num_fid_x_cli <- df_1_cli_fid_clean %>%
  group_by(ID_CLI) %>%
  summarise(NUM_FIDs =  n_distinct(ID_FID)
            , NUM_DATEs = n_distinct(DT_ACTIVE)
            )

tot_id_cli <- n_distinct(num_fid_x_cli$ID_CLI)

## compute the distribution of number of subscriptions
dist_num_fid_x_cli <- num_fid_x_cli %>%
  group_by(NUM_FIDs, NUM_DATEs) %>%
  summarise(TOT_CLIs = n_distinct(ID_CLI)) %>%
  mutate(PERCENT_CLIs = TOT_CLIs/tot_id_cli)

dist_num_fid_x_cli

#!!! NOTE: there are clients with multiple fidelity subscriptions !!!#

## let examine in details clients with multiple subscriptions

num_fid_x_cli %>% filter(NUM_FIDs == 3)

# each subscription can have different dates
df_1_cli_fid %>% filter(ID_CLI == 621814)
# there could be subscriptions at the same dates [possibly for technical reasons]
df_1_cli_fid %>% filter(ID_CLI == 320880)

#### RESHAPING df_1 ####

## combining information

# from first subscription  --> registration date, store for registration
# from last subscription   --> type of fidelity, status
# from subscriptions count --> number of subscriptions made

df_1_cli_fid_first <- df_1_cli_fid_clean %>%
  group_by(ID_CLI) %>%
  filter(DT_ACTIVE == min(DT_ACTIVE)) %>%
  arrange(ID_FID) %>%
  filter(row_number() == 1) %>%
  ungroup() %>%
  as.data.frame()

df_1_cli_fid_last <- df_1_cli_fid_clean %>%
  group_by(ID_CLI) %>%
  filter(DT_ACTIVE == max(DT_ACTIVE)) %>%
  arrange(desc(ID_FID)) %>%
  filter(row_number() == 1) %>%
  ungroup() %>%
  as.data.frame()

df_1_cli_fid_clean <- df_1_cli_fid_last %>%
  select(ID_CLI
         , ID_FID
         , LAST_COD_FID = COD_FID
         , LAST_TYP_CLI_FID = TYP_CLI_FID
         , LAST_STATUS_FID = STATUS_FID
         , LAST_DT_ACTIVE = DT_ACTIVE) %>%
  left_join(df_1_cli_fid_first %>%
              select(ID_CLI
                     , FIRST_ID_NEG = ID_NEG
                     , FIRST_DT_ACTIVE = DT_ACTIVE)
            , by = 'ID_CLI') %>%
  left_join(num_fid_x_cli %>%
              select(ID_CLI
                     , NUM_FIDs) %>%
              mutate(NUM_FIDs = as.factor(NUM_FIDs))
            , by = 'ID_CLI')

#### EXPLORE COLUMNS of df_1 ####

### variable LAST_COD_FID ###

## compute distribution
df1_dist_codfid <- df_1_cli_fid_clean %>%
  group_by(LAST_COD_FID) %>%
  summarise(TOT_CLIs = n_distinct(ID_CLI)) %>%
  mutate(PERCENT = TOT_CLIs/sum(TOT_CLIs)) %>%
  arrange(desc(PERCENT))

df1_dist_codfid

## plot distribution
plot_df1_dist_codfid <- (
  ggplot(data=df1_dist_codfid
         , aes(x=LAST_COD_FID, y=TOT_CLIs)
         ) +
    geom_bar(stat="identity"
             , fill="steelblue") +
    theme_minimal() +
  geom_text(aes(label = paste0(signif(TOT_CLIs/sum(TOT_CLIs)*100, digits = 2), " %")), nudge_y = 10000)
)

plot_df1_dist_codfid

#### ???? TO DO df_1 ???? ####
# EXPLORE the remaining df_1_cli_fid_clean relevant variables

### variable LAST_TYP_CLI_FID

df_1_percentage_subscription_main_account <- union(
  df_1_cli_fid_clean %>%
    group_by(LAST_COD_FID, LAST_TYP_CLI_FID) %>%
    summarise(n=n()) %>%
    mutate(perc_over_COD_FID = n/sum(n)) %>%
    ungroup() %>%
    filter(LAST_TYP_CLI_FID == 1) %>%
    mutate(perc_over_TYP_CLI_FID = n/sum(n))
  ,
  df_1_cli_fid_clean %>%
    group_by(LAST_COD_FID, LAST_TYP_CLI_FID) %>%
    summarise(n=n()) %>%
    mutate(perc_over_COD_FID = n/sum(n)) %>%
    ungroup() %>%
    filter(LAST_TYP_CLI_FID == 0) %>%
    mutate(perc_over_TYP_CLI_FID = n/sum(n))
)

#!!! NOTE: the percentage of premium subscriptions is higher in non-main accounts !!!#

### variable FIRST_ID_NEG ###

### compute distribution

df1_dist_idneg <- df_1_cli_fid_clean %>%
  group_by(FIRST_ID_NEG) %>%
  summarise(TOT_CLIs = n_distinct(ID_CLI)) %>%
  mutate(PERCENT = TOT_CLIs/sum(TOT_CLIs)) %>%
  arrange(desc(PERCENT))

plot_df1_stores_barchart <- (
  ggplot(data=df1_dist_idneg 
         , aes(x=FIRST_ID_NEG, y=TOT_CLIs)
  ) +
    geom_bar(stat="identity"
             , fill="steelblue") +
    theme_minimal()
)

plot_df1_stores_barchart

## Maybe the reason why most cards are provided by the first store is because it was the only open one in the past? ##
## Let's check: ##

df_1_proxy_store_opening = df_1_cli_fid %>%
  group_by(ID_NEG) %>%
  summarise(OPENING_PROXY_DATE = min(DT_ACTIVE)) %>%
  select(ID_NEG
       , OPENING_PROXY_DATE) %>%
  distinct()

  table(df_1_proxy_store_opening$OPENING_PROXY_DATE)
  
#!!! NOTE: it looks like every store was active from the beginning of the data collection !!!#

### variable LAST_STATUS_FID ###
  
  df_1_percentage_business_type_active_status = df_1_cli_fid_clean %>%
      group_by(IS_BUSINESS =
                 ifelse(
                   grepl("BIZ",LAST_COD_FID), "BIZ", "non-BIZ")
                , LAST_STATUS_FID) %>%
      summarise(n=n()) %>%
      mutate(perc_over_business_type = n/sum(n))
      
#!!! NOTE: business (BIZ) accounts have double the percentage of inactive accounts than the non-BIZ ones !!!#

## also business accounts are roughly an eighth the amount of non-BIZ ones ##

### further data manipulation ###

  df_1_cli_fid_clean = df_1_cli_fid_clean %>%
    mutate(ID_CLI = as.character(ID_CLI)) %>%
    mutate(ID_FID = as.character(ID_FID)) %>%
    mutate(LAST_COD_FID = as.factor(LAST_COD_FID)) %>%
    mutate(FIRST_ID_NEG = as.character(FIRST_ID_NEG)) %>%
    mutate(NUM_FIDs = as.integer(NUM_FIDs))
    
#### FINAL REVIEW df_1_clean ####

str(df_1_cli_fid_clean)
summary(df_1_cli_fid_clean)

#### REMOVAL OF INTERMEDIATE DATASETS
rm(df_1_cli_fid)
rm(df_1_cli_fid_first)
rm(df_1_cli_fid_last)
rm(df_1_percentage_business_type_active_status)
rm(df_1_percentage_subscription_main_account)
rm(df_1_proxy_store_opening)
rm(df1_dist_codfid)
rm(df1_dist_idneg)
rm(dist_num_fid_x_cli)
rm(num_fid_x_cli)
rm(plot_df1_dist_codfid)
rm(plot_df1_stores_barchart)
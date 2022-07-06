#### FIRST LOOK of df_3 ####

str(df_3_cli_address)
summary(df_3_cli_address)

#### START CLEANING df_3 ####

df_3_cli_address_clean <- df_3_cli_address

#### CLEANING DUPLICATE VALUES in df_3 ####

## check for duplicates
df_3_cli_address_clean %>%
  summarise(TOT_ID_ADDRESSes = n_distinct(ID_ADDRESS)
            , TOT_ROWs = n())

#!!! NOTE:  there are duplicates !!!#

df_3_cli_address_clean <- df_3_cli_address_clean %>%
  distinct()

#### CLEANING MISSING VALUES in df_3 ####

df_3_cli_address_clean %>%
  group_by(w_CAP = !is.na(CAP)
           , w_PRV = !is.na(PRV)
           , w_REGION = !is.na(REGION)) %>%
  summarise(TOT_ADDs = n_distinct(ID_ADDRESS))

## let examine in details some of these missing cases
df_3_cli_address_clean %>% filter(!is.na(PRV) & is.na(REGION))

## MISSING VALUES rows are removed ##
df_3_cli_address_clean <- df_3_cli_address_clean %>%  
  filter(!is.na(CAP) & !is.na(PRV) & !is.na(REGION))

#### CONSISTENCY CHECK ID_ADDRESS in df_2/df_3 ####

cons_idaddress_df2_df3 <- df_2_cli_account_clean %>%
  select(ID_ADDRESS) %>%
  mutate(is_in_df_2 = 1) %>%
  distinct() %>%
  full_join(df_3_cli_address_clean %>%
              select(ID_ADDRESS) %>%
              mutate(ID_ADDRESS = as.character(ID_ADDRESS)) %>%
              mutate(is_in_df_3 = 1) %>%
              distinct()
            , by = "ID_ADDRESS"
  ) %>%
  group_by(is_in_df_2, is_in_df_3) %>%
  summarise(NUM_ID_ADDRESSes = n_distinct(ID_ADDRESS)) %>%
  as.data.frame()

cons_idaddress_df2_df3

#!!! NOTE:  there are ID_ADDRESSes actually not mapped in df_3 !!!#
#!!!        this issue should be taken into account in joining these two tables !!!#

#### EXPLORE COLUMNS of df_3 ####

#### ???? TO DO df_3 ???? ####
# EXPLORE the df_3_cli_address_clean relevant variables

#### variable CAP ####

## summary ##

summary(as.integer(df_3_cli_address_clean$CAP))

#!!! NOTE: the CAP in Italy is supposed to be in range [00010 - 98168] !!!#
#!!! SOURCE: https://it.wikipedia.org/wiki/Codice_di_avviamento_postale !!!#

## fix CAP range ##
df_3_cli_address_clean = df_3_cli_address_clean %>%
                          filter(as.integer(CAP)>=00010 & as.integer(CAP)<=98168) 

df_3_cli_address_clean %>%
  group_by(PRV) %>%
  summarise(MAX_CAP = max(as.integer(CAP))
            ,MIN_CAP = min(as.integer(CAP))) %>%
  filter(MAX_CAP - MIN_CAP > 1000)

#!!! NOTE: there are certain PVC having over 1000 units CAP discrepancy !!!#

## it might be a data quality issue ##

#### variable PRV ####

## check PRV uniqueness per region ##
df_3_cli_address_clean %>%
    group_by(PRV) %>%
    summarise(DIST_COUNT = n_distinct(REGION)) %>%
    filter(DIST_COUNT > 1)

#!!! NOTE: there is no province shared among multiple regions !!!#

## column conversion to factor ##

df_3_cli_address_clean = df_3_cli_address_clean %>%
                            mutate(ID_ADDRESS = as.character(ID_ADDRESS)) %>%
                            mutate(CAP = as.factor(CAP)) %>%
                            mutate(PRV = as.factor(PRV)) %>%
                            mutate(REGION = as.factor(REGION))

## trying to understand better the variable TYP_CLI_ACCOUNT from df_2 ##

df_3_TYP_CLI_ACCOUNT_check = df_2_cli_account_clean %>%
                                select(ID_ADDRESS, TYP_CLI_ACCOUNT) %>%
                                distinct() %>%
                                left_join(df_3_cli_address_clean %>%
                                select(ID_ADDRESS, REGION)
                                 , by = "ID_ADDRESS")

## checking whether TYP_CLI_ACCOUNT is determined by the region ##

df_3_TYP_CLI_ACCOUNT_check %>%
  group_by(REGION) %>%
  summarise(DIST_COUNT = n_distinct(TYP_CLI_ACCOUNT))

#!!! NOTE: hypothesis discarded !!!#
                      
### further data exploration ###
          
## check for duplicates again

df_3_cli_address_clean %>%
  summarise(TOT_ID_ADDRESSes = n_distinct(ID_ADDRESS)
            , TOT_ROWs = n())

#!!! NOTE: there are still duplicate ID_ADDRESSES even after the distinct !!!#

df_3_cli_address_clean %>% group_by(ID_ADDRESS) %>% summarise(N_DIST = n()) %>% arrange(desc(N_DIST))

#!!! NOTE: ID_ADDRESS 201232 and 637914 are the culprits !!!#

## further investigation on these two ID_ADDRESSES ##

df_3_cli_address_clean %>% filter(ID_ADDRESS %in% c(201232,637914))

#!!! NOTE: the CAP for CN (Cuneo) is wrong while for IM (Imperia) is correct !!!#
#!!! SOURCE 1: https://www.tuttitalia.it/liguria/14-imperia/ !!!#
#!!! SOURCE 2: https://www.tuttitalia.it/piemonte/22-cuneo/ !!!#

## removal of the misleading records ##

df_3_cli_address_clean = df_3_cli_address_clean %>%
                          filter(!(ID_ADDRESS %in% c(201232,637914) & PRV == "CN"))

df_3_cli_address_clean %>% filter(ID_ADDRESS %in% c(201232,637914))

#!!! NOTE: now the records have been fixed !!!#

#### FINAL REVIEW df_3_clean ####

str(df_3_cli_address_clean)
summary(df_3_cli_address_clean)

#### REMOVAL OF INTERMEDIATE DATASETS
rm(df_3_cli_address)
rm(df_3_TYP_CLI_ACCOUNT_check)
rm(cons_idaddress_df2_df3)
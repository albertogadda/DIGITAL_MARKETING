## identify the most recent date ##

latest_date = max(df_7_tic_clean_final$TIC_DATE)

#!!! NOTE: the most recent date is "30/04/2019" !!!# 

## also since on average 90% of the customers buy again within 4 weeks (see line 444 of C07_preparation_df7) ##

# let's add up to a month as a small grace period to include some more active users that are a bit late on repurchasing #

active_threshold = months(1) # (average repurchase timespan + a couple of days of grace period)

## identify recently active users ##

rfm_study_period <- df_7_tic_clean_final %>%
    filter(DIREZIONE == 1) %>%
    filter(TIC_DATE > as.Date(latest_date %m-% active_threshold, format = "%d/%m/%Y"))

#################
#### RECENCY ####
#################

rfm_recency <- rfm_study_period %>%
  filter(DIREZIONE == 1) %>%  
  group_by(ID_CLI)%>% 
  summarise(LAST_PURCHASE_DATE = max(TIC_DATE)) %>%
  mutate(RECENCY = difftime(as.Date(latest_date %m-% active_threshold,
                                    format = "%d/%m/%Y"),        
                            LAST_PURCHASE_DATE,
                            units = "days")) %>%
  mutate(RECENCY = as.integer(RECENCY))

# division by tertiles: #
# LOW: below the 33rd percentile #
# MEDIUM: between the 33rd and 66th percentile #
# HIGH: above the 66th percentile #

rfm_recency <- within(rfm_recency,
                      REC_CLASS <- cut(as.numeric(rfm_recency$RECENCY),
                                       breaks = quantile(rfm_recency$RECENCY,
                                                         probs = c(0, .33, .66, 1)), 
                                       include.lowest = T,
                                       labels = c("low", "medium", "high")))         

# class absolute frequency #

(rec_label <- as.data.frame(table(rfm_recency$REC_CLASS)))

###################
#### FREQUENCY ####
###################

rfm_frequency <- rfm_study_period %>%
  filter(DIREZIONE == 1) %>% 
  group_by(ID_CLI) %>% 
  summarise(COUNT_PURCHASE = n_distinct(ID_SCONTRINO))

summary(rfm_frequency$COUNT_PURCHASE)

# since most customers made a single purchase, let's check if the tertiles breaks end up being non-unique #

quantile(rfm_frequency$COUNT_PURCHASE, probs = c(0, .33, .66, 1)) # non-unique as expected

# so let's create hardcoded breaks #

rfm_frequency <- within(rfm_frequency,
                      FREQ_CLASS <- cut(as.integer(rfm_frequency$COUNT_PURCHASE),
                                       breaks = c(0, 1, 2, 999999999),
                                       include.lowest = T,
                                       labels = c("low", "medium", "high")))         

# class absolute frequency #

(freq_label <- as.data.frame(table(rfm_frequency$FREQ_CLASS)))

#!!! NOTE: classes are heavily imbalanced due to the enormous amount of single purchases !!!#
# but the class [1,2) cannot be split further since it already consists only of a single integer #

##################
#### MONETARY ####
##################

rfm_monetary <- rfm_study_period

rfm_monetary <- rfm_monetary %>%
  filter(DIREZIONE == 1) %>%
  group_by(ID_CLI) %>% 
  summarise(TOT_AMOUNT = round(sum(IMPORTO_LORDO-SCONTO),2))

# division by tertiles

rfm_monetary <- within(rfm_monetary,
                        MONETARY_CLASS <- cut(rfm_monetary$TOT_AMOUNT,
                                          breaks = quantile(rfm_monetary$TOT_AMOUNT,
                                                            probs = c(0, .33, .66, 1)),
                                          include.lowest = T,
                                          labels = c("low", "medium", "high")))  

# class absolute frequency #

(monetary_label <- as.data.frame(table(rfm_monetary$MONETARY_CLASS)))

#### MERGE DATA COMPONENTS ####

rfm <- merge(rfm_frequency, 
             rfm_recency,  
             by = "ID_CLI") 
rfm <- merge(rfm,           
             rfm_monetary,   
             by = "ID_CLI") 

## encoding and interpretation of the various combinations of recency and frequency classes ##

rfm$RF <- NA

for(i in c(1:nrow(rfm)) )
  
  {
    if(rfm$REC_CLASS[i] == "low" && rfm$FREQ_CLASS[i] == "low") rfm$RF[i] = "One-Timer"
    if(rfm$REC_CLASS[i] == "medium" && rfm$FREQ_CLASS[i] == "low") rfm$RF[i] = "One-Timer"
    if(rfm$REC_CLASS[i] == "high" && rfm$FREQ_CLASS[i] == "low") rfm$RF[i] = "Leaving"
    if(rfm$REC_CLASS[i] == "low" && rfm$FREQ_CLASS[i] == "medium") rfm$RF[i] = "Engaged"
    if(rfm$REC_CLASS[i] == "medium" && rfm$FREQ_CLASS[i] == "medium") rfm$RF[i] = "Engaged"
    if(rfm$REC_CLASS[i] == "high" && rfm$FREQ_CLASS[i] == "medium") rfm$RF[i] = "Leaving"
    if(rfm$REC_CLASS[i] == "low" && rfm$FREQ_CLASS[i] == "high") rfm$RF[i] = "Top"
    if(rfm$REC_CLASS[i] == "medium" && rfm$FREQ_CLASS[i] == "high") rfm$RF[i] = "Top"
    if(rfm$REC_CLASS[i] == "high" && rfm$FREQ_CLASS[i] == "high") rfm$RF[i] = "Leaving Top"
  }

table(rfm$RF)



## add combinations for monetary classes ##

rfm$RFM <- NA

for(i in c(1:nrow(rfm)) )
  
  {
    if(rfm$RF[i] == "One-Timer" && rfm$MONETARY_CLASS[i] == "low") rfm$RFM[i] = "Cheap"
    if(rfm$RF[i] == "Leaving" && rfm$MONETARY_CLASS[i] == "low") rfm$RFM[i] = "Tin"
    if(rfm$RF[i] == "Engaged" && rfm$MONETARY_CLASS[i] == "low") rfm$RFM[i] = "Copper"
    if(rfm$RF[i] == "Leaving Top" && rfm$MONETARY_CLASS[i] == "low") rfm$RFM[i] = "Bronze"
    if(rfm$RF[i] == "Top" && rfm$MONETARY_CLASS[i] == "low") rfm$RFM[i] = "Silver"
  
    if(rfm$RF[i] == "One-Timer" && rfm$MONETARY_CLASS[i] == "medium") rfm$RFM[i] = "Tin"
    if(rfm$RF[i] == "Leaving" && rfm$MONETARY_CLASS[i] == "medium") rfm$RFM[i] = "Copper"
    if(rfm$RF[i] == "Engaged" && rfm$MONETARY_CLASS[i] == "medium") rfm$RFM[i] = "Bronze"
    if(rfm$RF[i] == "Leaving Top" && rfm$MONETARY_CLASS[i] == "medium") rfm$RFM[i] = "Silver"
    if(rfm$RF[i] == "Top" && rfm$MONETARY_CLASS[i] == "medium") rfm$RFM[i] = "Gold"
  
    if(rfm$RF[i] == "One-Timer" && rfm$MONETARY_CLASS[i] == "high") rfm$RFM[i] = "Copper"
    if(rfm$RF[i] == "Leaving" && rfm$MONETARY_CLASS[i] == "high") rfm$RFM[i] = "Bronze"
    if(rfm$RF[i] == "Engaged" && rfm$MONETARY_CLASS[i] == "high") rfm$RFM[i] = "Silver"
    if(rfm$RF[i] == "Leaving Top" && rfm$MONETARY_CLASS[i] == "high") rfm$RFM[i] = "Gold"
    if(rfm$RF[i] == "Top" && rfm$MONETARY_CLASS[i] == "high") rfm$RFM[i] = "Diamond"
  }

table(rfm$RFM)

#### COMPARISON WITH THEORICAL CLASS NUMEROSITY

rfm$RFM = as.factor(rfm$RFM)

table(rfm$RFM)/sum(table(rfm$RFM)) - 1/n_distinct(rfm$RFM)

#!!! NOTE: top category customers are less than expected and we have more of the less valuable ones !!!#

#######################################################################
#######################################################################
#######################################################################

#### COMPARISON WITH THE CUSTOMER BASE MIX FROM THE PREVIOUS MONTH ####

active_threshold_PM = months(2) # one month before the first analysis

## identify recently active users in previous timespan ##

rfm_study_period_PM <- df_7_tic_clean_final %>%
  filter(DIREZIONE == 1) %>%
  filter(TIC_DATE > as.Date(latest_date %m-% active_threshold_PM, format = "%d/%m/%Y"),
         TIC_DATE <= as.Date(latest_date %m-% active_threshold, format = "%d/%m/%Y"))

#################
#### RECENCY ####
#################

rfm_recency_PM <- rfm_study_period_PM %>%
  filter(DIREZIONE == 1) %>%  
  group_by(ID_CLI)%>% 
  summarise(LAST_PURCHASE_DATE = max(TIC_DATE)) %>%
  mutate(RECENCY = difftime(as.Date(latest_date %m-% active_threshold_PM,
                                    format = "%d/%m/%Y"),        
                            LAST_PURCHASE_DATE,
                            units = "days")) %>%
  mutate(RECENCY = as.integer(RECENCY))

# division by tertiles, following the thresholds set from the first analysis #

rfm_recency_PM <- within(rfm_recency_PM,
                      REC_CLASS <- cut(as.numeric(rfm_recency_PM$RECENCY),
                                       breaks = quantile(rfm_recency$RECENCY, # use same quantiles as first analysis
                                                         probs = c(0, .33, .66, 1)), 
                                       include.lowest = T,
                                       labels = c("low", "medium", "high")))         

# class absolute frequency #

(rec_label_PM <- as.data.frame(table(rfm_recency_PM$REC_CLASS)))

###################
#### FREQUENCY ####
###################

rfm_frequency_PM <- rfm_study_period_PM %>%
  filter(DIREZIONE == 1) %>% 
  group_by(ID_CLI)%>% 
  summarise(COUNT_PURCHASE = n_distinct(ID_SCONTRINO))

rfm_frequency_PM <- within(rfm_frequency_PM,
                        FREQ_CLASS <- cut(as.integer(rfm_frequency_PM$COUNT_PURCHASE),
                                          breaks = c(0, 1, 2, 999999999), # hard-coded as in the previous iteration
                                          include.lowest = T,
                                          labels = c("low", "medium", "high")))         

# class absolute frequency #

(freq_label_PM <- as.data.frame(table(rfm_frequency_PM$FREQ_CLASS)))

##################
#### MONETARY ####
##################

rfm_monetary_PM <- rfm_study_period_PM

rfm_monetary_PM <- rfm_monetary_PM %>%
  filter(DIREZIONE == 1) %>%
  group_by(ID_CLI) %>% 
  summarise(TOT_AMOUNT = round(sum(IMPORTO_LORDO-SCONTO),2))

# division by tertiles, following the thresholds set from the first analysis #

rfm_monetary_PM <- within(rfm_monetary_PM,
                       MONETARY_CLASS <- cut(rfm_monetary_PM$TOT_AMOUNT,
                                             breaks = quantile(rfm_monetary$TOT_AMOUNT,
                                                               probs = c(0, .33, .66, 1)),
                                             include.lowest = T,
                                             labels = c("low", "medium", "high")))  

# class absolute frequency #

(monetary_label_PM <- as.data.frame(table(rfm_monetary_PM$MONETARY_CLASS)))

#### MERGE DATA COMPONENTS ####

rfm_PM <- merge(rfm_frequency_PM, 
             rfm_recency_PM,  
             by = "ID_CLI") 
rfm_PM <- merge(rfm_PM,           
             rfm_monetary_PM,   
             by = "ID_CLI") 

## encoding and interpretation of the various combinations of recency and frequency classes ##

rfm_PM$RF <- NA

for(i in c(1:nrow(rfm_PM)) )
  
{
  if(rfm_PM$REC_CLASS[i] == "low" && rfm_PM$FREQ_CLASS[i] == "low") rfm_PM$RF[i] = "One-Timer"
  if(rfm_PM$REC_CLASS[i] == "medium" && rfm_PM$FREQ_CLASS[i] == "low") rfm_PM$RF[i] = "One-Timer"
  if(rfm_PM$REC_CLASS[i] == "high" && rfm_PM$FREQ_CLASS[i] == "low") rfm_PM$RF[i] = "Leaving"
  if(rfm_PM$REC_CLASS[i] == "low" && rfm_PM$FREQ_CLASS[i] == "medium") rfm_PM$RF[i] = "Engaged"
  if(rfm_PM$REC_CLASS[i] == "medium" && rfm_PM$FREQ_CLASS[i] == "medium") rfm_PM$RF[i] = "Engaged"
  if(rfm_PM$REC_CLASS[i] == "high" && rfm_PM$FREQ_CLASS[i] == "medium") rfm_PM$RF[i] = "Leaving"
  if(rfm_PM$REC_CLASS[i] == "low" && rfm_PM$FREQ_CLASS[i] == "high") rfm_PM$RF[i] = "Top"
  if(rfm_PM$REC_CLASS[i] == "medium" && rfm_PM$FREQ_CLASS[i] == "high") rfm_PM$RF[i] = "Top"
  if(rfm_PM$REC_CLASS[i] == "high" && rfm_PM$FREQ_CLASS[i] == "high") rfm_PM$RF[i] = "Leaving Top"
}

table(rfm_PM$RF)

## add combinations for monetary classes ##

rfm_PM$RFM <- NA

for( i in c(1:nrow(rfm_PM)) )
  
{
  if(rfm_PM$RF[i] == "One-Timer" && rfm_PM$MONETARY_CLASS[i] == "low") rfm_PM$RFM[i] = "Cheap"
  if(rfm_PM$RF[i] == "Leaving" && rfm_PM$MONETARY_CLASS[i] == "low") rfm_PM$RFM[i] = "Tin"
  if(rfm_PM$RF[i] == "Engaged" && rfm_PM$MONETARY_CLASS[i] == "low") rfm_PM$RFM[i] = "Copper"
  if(rfm_PM$RF[i] == "Leaving Top" && rfm_PM$MONETARY_CLASS[i] == "low") rfm_PM$RFM[i] = "Bronze"
  if(rfm_PM$RF[i] == "Top" && rfm_PM$MONETARY_CLASS[i] == "low") rfm_PM$RFM[i] = "Silver"
  
  if(rfm_PM$RF[i] == "One-Timer" && rfm_PM$MONETARY_CLASS[i] == "medium") rfm_PM$RFM[i] = "Tin"
  if(rfm_PM$RF[i] == "Leaving" && rfm_PM$MONETARY_CLASS[i] == "medium") rfm_PM$RFM[i] = "Copper"
  if(rfm_PM$RF[i] == "Engaged" && rfm_PM$MONETARY_CLASS[i] == "medium") rfm_PM$RFM[i] = "Bronze"
  if(rfm_PM$RF[i] == "Leaving Top" && rfm_PM$MONETARY_CLASS[i] == "medium") rfm_PM$RFM[i] = "Silver"
  if(rfm_PM$RF[i] == "Top" && rfm_PM$MONETARY_CLASS[i] == "medium") rfm_PM$RFM[i] = "Gold"
  
  if(rfm_PM$RF[i] == "One-Timer" && rfm_PM$MONETARY_CLASS[i] == "high") rfm_PM$RFM[i] = "Copper"
  if(rfm_PM$RF[i] == "Leaving" && rfm_PM$MONETARY_CLASS[i] == "high") rfm_PM$RFM[i] = "Bronze"
  if(rfm_PM$RF[i] == "Engaged" && rfm_PM$MONETARY_CLASS[i] == "high") rfm_PM$RFM[i] = "Silver"
  if(rfm_PM$RF[i] == "Leaving Top" && rfm_PM$MONETARY_CLASS[i] == "high") rfm_PM$RFM[i] = "Gold"
  if(rfm_PM$RF[i] == "Top" && rfm_PM$MONETARY_CLASS[i] == "high") rfm_PM$RFM[i] = "Diamond"
}

table(rfm_PM$RFM)

####################################
#### RESULTS COMPARISON WITH PM ####
####################################

rfm_PM$RFM = as.factor(rfm_PM$RFM)

table(rfm$RFM)/sum(table(rfm$RFM)) - table(rfm_PM$RFM)/sum(table(rfm_PM$RFM))

#!!! NOTE: we have also being losing top customers mix over time in favor of bad customer mix !!!#


# PLOT RF
rf_df <- as.data.frame(rbind(c("Top",         "High",   "Low",   4287 ),
                             c("Top",         "High",   "Medium", 2163),
                             c("Leaving Top", "High",   "High",   583),
                             c("Engaged",     "Medium", "Low",    3883),
                             c("Engaged",     "Medium", "Medium", 3269),
                             c("Leaving",     "Medium", "High",   2207),
                             c("One Timer",   "Low",    "Low",    8252),
                             c("One Timer",   "Low",    "Medium", 8957),
                             c("Leaving",     "Low",    "High",   12741)))

colnames(rf_df) <-  c("Level", "Frequency", "Recency", "Value")
rf_df$Frequency <- factor(rf_df$Frequency,
                          levels = c("High", "Medium", "Low"))
rf_df$Recency <- factor(rf_df$Recency,
                        levels = c("High", "Medium", "Low"))
rf_df$Value <- as.numeric(rf_df$Value)

ggplot(rf_df, aes(x = Frequency, y = Recency, fill = Value)) + 
  geom_tile() +
  labs(title='RF Model')+
  geom_text(aes(label = Level),color = "white") +
  theme_minimal()+
  theme(plot.title = element_text(size=15,hjust = 0.5),
        axis.text.x = element_text(size=10),
        axis.text.y = element_text(size=10, hjust=1),
        legend.text = element_text(size=10),
        legend.title = element_text(size=10))


#PLOT RFM
rfm_df <- as.data.frame(rbind(c("Top", "High", "Diamond", 4719 ),
                              c("Top", "Medium", "Gold", 1526),
                              c("Top", "Low", "Silver", 205),
                              c("Leaving Top", "High", "Gold", 397),
                              c("Leaving Top", "Medium", "Silver", 153),
                              c("Leaving Top", "Low", "Bronze", 33),
                              c("Engaged", "High", "Silver", 3220),
                              c("Engaged", "Medium", "Bronze", 2811),
                              c("Engaged", "Low", "Copper", 1121),
                              c("Leaving", "High", "Bronze", 3716),
                              c("Leaving", "Medium", "Copper", 5100),
                              c("Leaving", "Low", "Tin", 6132),
                              c("One Timer", "High", "Copper", 3704),
                              c("One Timer", "Medium", "Tin", 5694),
                              c("One Timer", "Low", "Cheap", 7811)))

colnames(rfm_df) <- c("RF", "Monetary", "Level", "Value")
rfm_df$RF <- factor(rfm_df$RF,
                    levels = c("Top", "Leaving Top",
                               "Engaged", "Leaving", "One Timer"))
rfm_df$Monetary <- factor(rfm_df$Monetary,
                          levels = c("Low", "Medium", "High"))
rfm_df$Value <- as.numeric(rfm_df$Value)

ggplot(rfm_df, aes(x = RF, y = Monetary, fill = Value)) + 
  geom_tile() +
  labs(title='RFM Model')+
  geom_text(aes(label = Level),color = "white") +
  theme_minimal()+
  theme(plot.title = element_text(size=15,hjust = 0.5),
        axis.text.x = element_text(size=10),
        axis.text.y = element_text(size=10, hjust=1),
        legend.text = element_text(size=10),
        legend.title = element_text(size=10))


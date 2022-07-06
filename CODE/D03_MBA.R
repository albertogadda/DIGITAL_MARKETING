
tickets_clean=df_7_tic_clean_final

#guardo per ogni prodotto quante volte compare
count_tickets <- tickets_clean %>%
  group_by(ID_ARTICOLO) %>%
  summarise(count = n()) %>% 
  arrange(desc(count)) %>%
  slice_max(count, n = 100)

#plottiamo i 20 prodotti pi? acquistati
count_tickets %>%
  slice_max(count, n = 10) %>%
  ggplot(aes(x = reorder(ID_ARTICOLO, order(count,decreasing=TRUE) ), y = count)) +
  
  geom_bar(stat= "identity", fill = "orange") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)) + # Centering Title
  theme(axis.text.x=element_text(angle = 70, vjust = 0.6)) +
  labs(x = "Item",
       y = "Count Purchase",
       title = "Top 10 Best Sellers")


#Guardo per ogni cliente quali prodotti ha acquistato
itemList <- plyr::ddply(tickets_clean, c("ID_CLI", "TIC_DATE"),
                        function(df1)paste(df1$ID_ARTICOLO, 
                                           collapse = ","))

itemList$ID_ARTICOLO <- NULL
itemList$TIC_DATE <- NULL
colnames(itemList) <- c("items")
# composto da queste colonne:
#la prima ? un numero crescente senza significato
#la seconda ? id cliente
#le altre sono i prodotti che ha acquistato

#scrivo su un csv perch? la funzione read transaction si aspetta quello
write.csv(itemList,"market_basket_2.csv", quote = FALSE, row.names = TRUE)

#leggo il csv
tr <- read.transactions('market_basket_2.csv', format = 'basket', sep=',')
tr
summary(tr)


#in quante transazioni compare ogni articolo? plotto i 20 pi? presenti
itemFrequencyPlot(tr, topN = 10, type = 'absolute',col='orange')
#sono poche in valore assoluto perch? ho fatto il subset

#cerco le migliori regole di associazione tra prodotti (imposto a mano il supporto e la confidenza)
rules <- apriori(tr, parameter = list(supp = 0.001, conf = 0.8))
rules <- sort(rules, by = 'confidence', decreasing = TRUE)
summary(rules)


inspect(rules)
# se acquisto il prodotto [x1,x2,..]  ====>  predico di acquistare y  |confidenza|supporto|lift ....


#mostro le regole migliori
topRules <- rules[1:10]
plot(topRules, col=c('blue','lightskyblue1'), main="TOP 10 RULES")
type=c("scatterplot", "two-key plot", "matrix", "matrix3D", "mosaic", "doubledecker", "graph", "paracoord" ,"grouped", "iplots")

par(mfrow=c(1,1))

plot(topRules, method = type[9], col=c('blue','lightskyblue1'))



Progetto sviluppato a cura di Cornacchia Giovanni, Gadda Alberto e Guerini Rocco Paolo 
######################################################################################


Per lanciare l'intera pipeline è sufficiente eseguie il file A00_main_compiled.R 
Esso avvierà la data ingestion, le analisi relative ad ogni dataset, e le implemantazioni aggiuntive (RFM, CHURN, MBA).



R version 4.2.0 (2022-04-22 ucrt)
Platform: x86_64-w64-mingw32/x64 (64-bit)
Running under: Windows 10 x64 (build 19044)

Matrix products: default

locale:
[1] LC_COLLATE=English_United Kingdom.utf8  LC_CTYPE=English_United Kingdom.utf8   
[3] LC_MONETARY=English_United Kingdom.utf8 LC_NUMERIC=C                           
[5] LC_TIME=English_United Kingdom.utf8    

attached base packages:
[1] stats     graphics  grDevices utils     datasets  methods   base     

other attached packages:
 [1] funModeling_1.9.4    Hmisc_4.7-0          Formula_1.2-4        survival_3.3-1       glmnet_4.1-4        
 [6] randomForest_4.7-1.1 ranger_0.13.1        rpart.plot_3.1.1     rpart_4.1.16         caret_6.0-92        
[11] lattice_0.20-45      corrplot_0.92        e1071_1.7-9          arulesViz_1.5-1      arules_1.7-3        
[16] Matrix_1.4-1         RQuantLib_0.4.16     lubridate_1.8.0      forcats_0.5.1        ggplot2_3.3.6       
[21] magrittr_2.0.3       dplyr_1.0.9         

loaded via a namespace (and not attached):
 [1] colorspace_2.0-3     ellipsis_0.3.2       class_7.3-20         htmlTable_2.4.0      base64enc_0.1-3     
 [6] rstudioapi_0.13      proxy_0.4-26         listenv_0.8.0        farver_2.1.0         graphlayouts_0.8.0  
[11] ggrepel_0.9.1        prodlim_2019.11.13   fansi_1.0.3          codetools_0.2-18     splines_4.2.0       
[16] knitr_1.39           polyclip_1.10-0      entropy_1.3.1        pROC_1.18.0          cluster_2.1.3       
[21] png_0.1-7            ggforce_0.3.3        compiler_4.2.0       backports_1.4.1      assertthat_0.2.1    
[26] fastmap_1.1.0        lazyeval_0.2.2       cli_3.3.0            tweenr_1.0.2         htmltools_0.5.2     
[31] tools_4.2.0          igraph_1.3.1         gtable_0.3.0         glue_1.6.2           reshape2_1.4.4      
[36] Rcpp_1.0.8.3         vctrs_0.4.1          nlme_3.1-157         iterators_1.0.14     ggraph_2.0.5        
[41] timeDate_3043.102    gower_1.0.0          xfun_0.31            stringr_1.4.0        globals_0.15.0      
[46] lifecycle_1.0.1      future_1.26.1        MASS_7.3-56          zoo_1.8-10           scales_1.2.0        
[51] TSP_1.2-0            ipred_0.9-12         tidygraph_1.2.1      parallel_4.2.0       RColorBrewer_1.1-3  
[56] gridExtra_2.3        pander_0.6.5         latticeExtra_0.6-29  stringi_1.7.6        foreach_1.5.2       
[61] checkmate_2.1.0      seriation_1.3.5      hardhat_1.0.0        lava_1.6.10          shape_1.4.6         
[66] rlang_1.0.2          pkgconfig_2.0.3      moments_0.14.1       ROCR_1.0-11          purrr_0.3.4         
[71] labeling_0.4.2       recipes_0.2.0        htmlwidgets_1.5.4    tidyselect_1.1.2     parallelly_1.31.1   
[76] plyr_1.8.7           R6_2.5.1             generics_0.1.2       DBI_1.1.2            pillar_1.7.0        
[81] foreign_0.8-82       withr_2.5.0          nnet_7.3-17          tibble_3.1.7         future.apply_1.9.0  
[86] crayon_1.5.1         utf8_1.2.2           viridis_0.6.2        jpeg_0.1-9           grid_4.2.0          
[91] data.table_1.14.2    ModelMetrics_1.2.2.2 digest_0.6.29        tidyr_1.2.0          stats4_4.2.0        
[96] munsell_0.5.0        registry_0.5-1       viridisLite_0.4.0   

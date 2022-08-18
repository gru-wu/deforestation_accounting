scale_colour_manual(values = c("#fde725", "#CDAA7D","#FFD39B", "#528B8B", "#97FFFF", "#00CDCD", "#458B00", "#A2CD5A", "#CAFF70") )
 ggplot(plot_data, aes(x= year, y= value, colour = amort))+
  +   geom_line(aes(linetype=intervall, color = amort))+
  +   ylab("deviation [%]")+
  +   #scale_colour_viridis_d("amortization", direction = -1)+
  +   scale_linetype_manual(values=c("twodash", "dotted", "solid"))+
  +   #labs(colour = "system definitions")
  +   scale_colour_manual(values = c("#fde725", "#CDAA7D","#FFD39B") )
> ggplot(plot_data, aes(x= year, y= value, colour = amort))+
  +   geom_line(aes(linetype=intervall, color = amort))+
  +   ylab("deviation [%]")+
  +   #scale_colour_viridis_d("amortization", direction = -1)+
  +   scale_linetype_manual(values=c("twodash", "dotted", "solid"))+
  +   #labs(colour = "system definitions")
  +   scale_colour_manual(values = c("#5ec962", "#CDAA7D","#FFD39B") )
> ggplot(plot_data, aes(x= year, y= value, colour = amort))+
  +   geom_line(aes(linetype=intervall, color = amort))+
  +   ylab("deviation [%]")+
  +   #scale_colour_viridis_d("amortization", direction = -1)+
  +   scale_linetype_manual(values=c("twodash", "dotted", "solid"))+
  +   #labs(colour = "system definitions")
  +   scale_colour_manual(values = c("#5ec962", "#3b528b","#FFD39B") )
> ggplot(plot_data, aes(x= year, y= value, colour = amort))+
  +   geom_line(aes(linetype=intervall, color = amort))+
  +   ylab("deviation [%]")+
  +   #scale_colour_viridis_d("amortization", direction = -1)+
  +   scale_linetype_manual(values=c("twodash", "dotted", "solid"))+
  +   #labs(colour = "system definitions")
  +   scale_colour_manual(values = c("#5ec962", "#3b528b","#440154") )
> load data 
Error: unexpected symbol in "load data"
> 
  > footprint_CHN <- readRDS("~/Deforestation-calculation/Paper/footprint_CHN.rds")
> footprint_EU <- readRDS("~/Deforestation-calculation/Paper/footprint_EU.rds")
> 
  > # aggregate EU countries to EU total
  > plot_EU <- footprint_EU %>%
  +   group_by(year, amort, var) %>%
  +   summarise(complete_footprint = sum(complete_footprint)) %>% 
  +   ungroup() %>% 
  +   mutate(year = as.character(year))
`summarise()` has grouped output by 'year', 'amort'. You can override using the `.groups` argument.
> 
  > avg <- plot_EU %>% 
  +   group_by(amort, var) %>% 
  +   summarise(complete_footprint = sum(complete_footprint)/length(unique(plot_EU$year))) %>% 
  +   ungroup() %>% 
  +   mutate(year = "average")
`summarise()` has grouped output by 'amort'. You can override using the `.groups` argument.
> 
  > plot_EU <- bind_rows(plot_EU, avg)%>%
  +   mutate(type=ifelse(year=="average","Highlighted","Normal"))%>%
  +   mutate(complete_footprint = complete_footprint / 100)
> 
  > rm(avg)
> 
  > 
  > #plot EU
  > ggplot(plot_EU, aes(x=as.factor(year), y=complete_footprint, fill= type)) + 
  +   geom_boxplot()+ 
  +   theme(legend.position="none")+
  +   xlab("years")+
  +   ylab("Footprint [km²]")+
  +   ggtitle("EU") +
  +   scale_fill_manual(values=c("#440154", "#5ec962"))
> #plot EU
  > ggplot(plot_EU, aes(x=as.factor(year), y=complete_footprint, fill= type)) + 
  +   geom_boxplot()+ 
  +   theme(legend.position="none")+
  +   xlab("years")+
  +   ylab("Footprint [km²]")+
  +   ggtitle("EU") +
  +   scale_fill_manual(values=c("#440154", "#31688e"))
> #plot EU
  > ggplot(plot_EU, aes(x=as.factor(year), y=complete_footprint, fill= type)) + 
  +   geom_boxplot()+ 
  +   theme(legend.position="none")+
  +   xlab("years")+
  +   ylab("Footprint [km²]")+
  +   ggtitle("EU") +
  +   scale_fill_manual(values=c("#31688e", "#35b779"))
>  load data 
Error: unexpected symbol in " load data"
> 
  > footprint_CHN <- readRDS("~/Deforestation-calculation/Paper/footprint_CHN.rds")
> footprint_EU <- readRDS("~/Deforestation-calculation/Paper/footprint_EU.rds")
> 
  > # aggregate EU countries to EU total
  > plot_EU <- footprint_EU %>%
  +   group_by(year, amort, var) %>%
  +   summarise(complete_footprint = sum(complete_footprint)) %>% 
  +   ungroup() %>% 
  +   mutate(year = as.character(year))
`summarise()` has grouped output by 'year', 'amort'. You can override using the `.groups` argument.
> 
  > avg <- plot_EU %>% 
  +   group_by(amort, var) %>% 
  +   summarise(complete_footprint = sum(complete_footprint)/length(unique(plot_EU$year))) %>% 
  +   ungroup() %>% 
  +   mutate(year = "average")
`summarise()` has grouped output by 'amort'. You can override using the `.groups` argument.
> 
  > plot_EU <- bind_rows(plot_EU, avg)%>%
  +   mutate(type=ifelse(year=="average","Highlighted","Normal"))%>%
  +   mutate(complete_footprint = complete_footprint / 100)
> 
  > rm(avg)
> 
  > 
  > #plot EU
  > ggplot(plot_EU, aes(x=as.factor(year), y=complete_footprint, fill= type)) + 
  +   geom_boxplot()+ 
  +   theme(legend.position="none")+
  +   xlab("years")+
  +   ylab("Footprint [km²]")+
  +   ggtitle("EU") +
  +   scale_fill_manual(values=c("#31688e", "#35b779"))
>   
  > 
  > #prepare plot data for CHINA
  > plot_CHN <-  footprint_CHN %>%
  +   group_by(year, amort, var) %>%
  +   summarise(complete_footprint = sum(complete_footprint)) %>% 
  +   ungroup() %>% 
  +   mutate(year = as.character(year))
`summarise()` has grouped output by 'year', 'amort'. You can override using the `.groups` argument.
> 
  > avg <- plot_CHN %>% 
  +   group_by(amort, var) %>% 
  +   summarise(complete_footprint = sum(complete_footprint)/length(unique(plot_CHN$year))) %>% 
  +   ungroup() %>% 
  +   mutate(year = "average")
`summarise()` has grouped output by 'amort'. You can override using the `.groups` argument.
> 
  > plot_CHN <- bind_rows(plot_CHN, avg)%>%
  +   mutate(type=ifelse(year=="average","Highlighted","Normal"))%>%
  +   mutate(complete_footprint = complete_footprint / 100)
> 
  > # plot China
  > ggplot(plot_CHN, aes(x=as.factor(year), y=complete_footprint, fill= type)) + 
  +   geom_boxplot()+ 
  +   theme(legend.position="none")+
  +   xlab("years")+
  +   ylab("Footprint [km²]")+
  +   ggtitle("CHN") +
  +   scale_fill_manual(values=c("#31688e", "#35b779"))
> 
  > 
  > rm(list = ls())
> #load data
  > defor <- readRDS("~/Deforestation-calculation/input/defor.rds")
> 
  > facet_data <- defor %>%
  +   gather(key= y, value = "value",1:3)%>%
  +   mutate(value = value / 100)
> facet_data$y <- substring(facet_data$y, 2)
> 
  > # combine amort & intervall into one variable
  > a <- "a"
> b <- "_y"
> facet_data$amort_var <- paste0(a, sprintf("%02d",as.numeric(facet_data$amort)), 
                                 +                               b, sprintf("%02d",as.numeric(facet_data$y)))
> rm(a,b)
> 
  > ggplot(facet_data, aes(x = year, y = value, fill = factor(luc, levels=c("indir", "dir")))) +
  +   geom_area()+
  +   facet_wrap(vars(amort_var))+
  +   ylab("deforestation [km²]")+
  +   labs(fill = "LUC")+
  +   scale_fill_manual(values = c("#31688e", "#35b779"))
> ggplot(facet_data, aes(x = year, y = value, fill = factor(luc, levels=c("indir", "dir")))) +
  +   geom_area()+
  +   facet_wrap(vars(amort_var))+
  +   ylab("deforestation [km²]")+
  +   labs(fill = "LUC")+
  +   scale_fill_manual(values = c("#90d743", "#443983"))
>   scale_fill_manual(values = c( "#443983", "#90d743")
                      + ggplot(facet_data, aes(x = year, y = value, fill = factor(luc, levels=c("indir", "dir")))) +
                        Error: unexpected symbol in:
                        "  scale_fill_manual(values = c( "#443983", "#90d743")
                      ggplot"
>   geom_area()+
+   facet_wrap(vars(amort_var))+
+   ylab("deforestation [km²]")+
+   labs(fill = "LUC")+
+   scale_fill_manual(values = c( "#443983", "#90d743"))
                      Error: Cannot add ggproto objects together. Did you forget to add this object to a ggplot object?
                        Run `rlang::last_error()` to see where the error occurred.
                      > ggplot(facet_data, aes(x = year, y = value, fill = factor(luc, levels=c("indir", "dir")))) +
                        +   geom_area()+
                        +   facet_wrap(vars(amort_var))+
                        +   ylab("deforestation [km²]")+
                        +   labs(fill = "LUC")+
                        +   scale_fill_manual(values = c( "#443983", "#90d743"))
                      > ggplot(facet_data, aes(x = year, y = value, fill = factor(luc, levels=c("indir", "dir")))) +
                        +   geom_area()+
                        +   facet_wrap(vars(amort_var))+
                        +   ylab("deforestation [km²]")+
                        +   labs(fill = "LUC")+
                        +   scale_fill_manual(values = c( "#2c728e", "#addc30"))
                      > ggplot(facet_data, aes(x = year, y = value, fill = factor(luc, levels=c("indir", "dir")))) +
                        +   geom_area()+
                        +   facet_wrap(vars(amort_var))+
                        +   ylab("deforestation [km²]")+
                        +   labs(fill = "LUC")+
                        +   scale_fill_manual(values = c( "#31688e", "#addc30"))
                      > ggplot(facet_data, aes(x = year, y = value, fill = factor(luc, levels=c("indir", "dir")))) +
                        +   geom_area()+
                        +   facet_wrap(vars(amort_var))+
                        +   ylab("deforestation [km²]")+
                        +   labs(fill = "LUC")+
                        +   scale_fill_manual(values = c( "#31688e", "#35b779"))
                      > ggplot(facet_data, aes(x = year, y = value, fill = factor(luc, levels=c("indir", "dir")))) +
                        +   geom_area()+
                        +   facet_wrap(vars(amort_var))+
                        +   ylab("deforestation [km²]")+
                        +   labs(fill = "LUC")+
                        +   scale_fill_manual(values = c( "#31688e", "#35b779"))
                      > ggplot(facet_data, aes(x = year, y = value, fill = factor(luc, levels=c("indir", "dir")))) +
                        +   geom_area()+
                        +   facet_wrap(vars(amort_var))+
                        +   ylab("deforestation [km²]")+
                        +   labs(fill = "LUC")+
                        +   scale_fill_manual(values = c( "#31688e", "#addc30"))
                      > g
                      Error: object 'g' not found
                      > ggplot(facet_data, aes(x = year, y = value, fill = factor(luc, levels=c("indir", "dir")))) +
                        +   geom_area()+
                        +   facet_wrap(vars(amort_var))+
                        +   ylab("deforestation [km²]")+
                        +   labs(fill = "LUC")+
                        +   scale_fill_manual(values = c( "#31688e", "#addc30"))
                      > #load data
                        > defor <- readRDS("~/Deforestation-calculation/input/defor.rds")
                      > 
                        > # summarized direct+indirect
                        > sens_table <- readRDS("~/Deforestation-calculation/Analysis_deforestation/sens_table.rds")
                      > 
                        > 
                        > fun <- function(x){
                          +   round(((x/sens_table[,6])-1) * 100, 0)
                          + }
                      > 
                        > plot_data <- data.frame(apply(sens_table,2, fun))
                      > 
                        > colnames(plot_data) <- colnames(sens_table)
                      > plot_data$year <- sens_table$year
                      > 
                        > plot_data <- plot_data %>% 
                        +   gather("key","value",-year) %>% 
                        +   mutate(amort = paste0("a", sprintf("%02d", as.numeric(substr(key, regexpr("a", key)+1,10))))) %>% 
                        +   mutate(intervall = paste0("y", sprintf("%02d", as.numeric(substr(key, regexpr("y", key)+1,regexpr("a", key)-1))))) %>% 
                        +   select(-key)
                      > 
                        > ggplot(plot_data, aes(x= year, y= value, colour = amort))+
                        +   geom_line(aes(linetype=intervall, color = amort))+
                        +   ylab("deviation [%]")+
                        +   #scale_colour_viridis_d("amortization", direction = -1)+
                        +   scale_linetype_manual(values=c("twodash", "dotted", "solid"))+
                        +   #labs(colour = "system definitions")
                        +   scale_colour_manual(values = c("#5ec962", "#3b528b","#440154") )
                      >   
                        > rm(list = ls())  
                      > #load data
                        > defor <- readRDS("~/Deforestation-calculation/input/defor.rds")
                      > # summarized direct+indirect
                        > sens_table <- readRDS("~/Deforestation-calculation/Analysis_deforestation/sens_table.rds")
                      > plot_data <- data.frame(apply(sens_table,2, fun))
                      Error in match.fun(FUN) : object 'fun' not found
                      > colnames(plot_data) <- colnames(sens_table)
                      Error in colnames(plot_data) <- colnames(sens_table) : 
                        object 'plot_data' not found
                      > plot_data <- plot_data %>% 
                        +   gather("key","value",-year) %>% 
                        +   mutate(amort = paste0("a", sprintf("%02d", as.numeric(substr(key, regexpr("a", key)+1,10))))) %>% 
                        +   mutate(intervall = paste0("y", sprintf("%02d", as.numeric(substr(key, regexpr("y", key)+1,regexpr("a", key)-1))))) %>% 
                        +   select(-key)
                      Error in gather(., "key", "value", -year) : object 'plot_data' not found
                      > plot_data$year <- sens_table$year
                      Error in plot_data$year <- sens_table$year : object 'plot_data' not found
                      > fun <- function(x){
                        +   round(((x/sens_table[,6])-1) * 100, 0)
                        + }
                      > ggplot(plot_data, aes(x= year, y= value, colour = amort))+
                        +   geom_line(aes(linetype=intervall, color = amort))+
                        +   ylab("deviation [%]")+
                        +   #scale_colour_viridis_d("amortization", direction = -1)+
                        +   scale_linetype_manual(values=c("twodash", "dotted", "solid"))+
                        +   #labs(colour = "system definitions")
                        +   scale_colour_manual(values = c("#5ec962", "#3b528b","#440154") )
                      Error in ggplot(plot_data, aes(x = year, y = value, colour = amort)) : 
                        object 'plot_data' not found
                      > # summarized direct+indirect
                        > sens_table <- readRDS("~/Deforestation-calculation/Analysis_deforestation/sens_table.rds")
                      > fun <- function(x){
                        +   round(((x/sens_table[,6])-1) * 100, 0)
                        + }
                      > plot_data <- data.frame(apply(sens_table,2, fun))
                      > colnames(plot_data) <- colnames(sens_table)
                      > plot_data <- plot_data %>% 
                        +   gather("key","value",-year) %>% 
                        +   mutate(amort = paste0("a", sprintf("%02d", as.numeric(substr(key, regexpr("a", key)+1,10))))) %>% 
                        +   mutate(intervall = paste0("y", sprintf("%02d", as.numeric(substr(key, regexpr("y", key)+1,regexpr("a", key)-1))))) %>% 
                        +   select(-key)
                      > plot_data$year <- sens_table$year
                      > ggplot(plot_data, aes(x= year, y= value, colour = amort))+
                        +   geom_line(aes(linetype=intervall, color = amort))+
                        +   ylab("deviation [%]")+
                        +   #scale_colour_viridis_d("amortization", direction = -1)+
                        +   scale_linetype_manual(values=c("twodash", "dotted", "solid"))+
                        +   #labs(colour = "system definitions")
                        +   scale_colour_manual(values = c("#5ec962", "#3b528b","#440154") )
                      System has not been booted with systemd as init system (PID 1). Can't operate.
Failed to connect to bus: Host is down

Attaching package: ‘maps’

The following object is masked from ‘package:purrr’:

    map

> footprint_CHN <- readRDS("~/Deforestation-calculation/Paper/footprint_CHN.rds")
Warning message:
In system("timedatectl", intern = TRUE) :
  running command 'timedatectl' had status 1
> footprint_EU <- readRDS("~/Deforestation-calculation/Paper/footprint_EU.rds")
> 
> # aggregate EU countries to EU total
> plot_EU <- footprint_EU %>%
+   group_by(year, amort, var) %>%
+   summarise(complete_footprint = sum(complete_footprint)) %>% 
+   ungroup() %>% 
+   mutate(year = as.character(year))
`summarise()` has grouped output by 'year', 'amort'. You can override using the `.groups` argument.
> 
> avg <- plot_EU %>% 
+   group_by(amort, var) %>% 
+   summarise(complete_footprint = sum(complete_footprint)/length(unique(plot_EU$year))) %>% 
+   ungroup() %>% 
+   mutate(year = "average")
`summarise()` has grouped output by 'amort'. You can override using the `.groups` argument.
> 
> plot_EU <- bind_rows(plot_EU, avg)%>%
+   mutate(type=ifelse(year=="average","Highlighted","Normal"))%>%
+   mutate(complete_footprint = complete_footprint / 100)
> 
> rm(avg)
> 
> 
> #plot EU
> ggplot(plot_EU, aes(x=as.factor(year), y=complete_footprint, fill= type)) + 
+   geom_boxplot()+ 
+   theme(legend.position="none")+
+   xlab("years")+
+   ylab("Footprint [km²]")+
+   ggtitle("EU") +
+   scale_fill_manual(values=c("#31688e", "#35b779"))
>   
> 
> #prepare plot data for CHINA
> plot_CHN <-  footprint_CHN %>%
+   group_by(year, amort, var) %>%
+   summarise(complete_footprint = sum(complete_footprint)) %>% 
+   ungroup() %>% 
+   mutate(year = as.character(year))
`summarise()` has grouped output by 'year', 'amort'. You can override using the `.groups` argument.
> 
> avg <- plot_CHN %>% 
+   group_by(amort, var) %>% 
+   summarise(complete_footprint = sum(complete_footprint)/length(unique(plot_CHN$year))) %>% 
+   ungroup() %>% 
+   mutate(year = "average")
`summarise()` has grouped output by 'amort'. You can override using the `.groups` argument.
> 
> plot_CHN <- bind_rows(plot_CHN, avg)%>%
+   mutate(type=ifelse(year=="average","Highlighted","Normal"))%>%
+   mutate(complete_footprint = complete_footprint / 100)
> 
> # plot China
> ggplot(plot_CHN, aes(x=as.factor(year), y=complete_footprint, fill= type)) + 
+   geom_boxplot()+ 
+   theme(legend.position="none")+
+   xlab("years")+
+   ylab("Footprint [km²]")+
+   ggtitle("CHN") +
+   scale_fill_manual(values=c("#31688e", "#35b779"))
> View(plot_EU)
> filter(plot_EU, year == 2000)
# A tibble: 9 × 5
  year  amort   var complete_footprint type  
  <chr> <dbl> <dbl>              <dbl> <chr> 
1 2000      1     1               746. Normal
2 2000      1     5              2296. Normal
3 2000      1    10              1775. Normal
4 2000      5     1               276. Normal
5 2000      5     5              1076. Normal
6 2000      5    10               864. Normal
7 2000     10     1               138. Normal
8 2000     10     5               615. Normal
9 2000     10    10               751. Normal
> test <- filter(plot_EU, year == 2000)
> View(test)
> rm(test)
> filter(plot_EU, year == 2001)
# A tibble: 9 × 5
  year  amort   var complete_footprint type  
  <chr> <dbl> <dbl>              <dbl> <chr> 
1 2001      1     1               814. Normal
2 2001      1     5              2703. Normal
3 2001      1    10              2089. Normal
4 2001      5     1               488. Normal
5 2001      5     5              1626. Normal
6 2001      5    10              1285. Normal
7 2001     10     1               244. Normal
8 2001     10     5               994. Normal
9 2001     10    10              1017. Normal
> filter(plot_CHN, year == 2001)
# A tibble: 9 × 5
  year  amort   var complete_footprint type  
  <chr> <dbl> <dbl>              <dbl> <chr> 
1 2001      1     1              173.  Normal
2 2001      1     5              574.  Normal
3 2001      1    10              443.  Normal
4 2001      5     1              104.  Normal
5 2001      5     5              345.  Normal
6 2001      5    10              273.  Normal
7 2001     10     1               51.8 Normal
8 2001     10     5              211.  Normal
9 2001     10    10              216.  Normal
> #plot EU
> ggplot(plot_EU, aes(x=as.factor(year), y=complete_footprint, fill= type)) + 
+   geom_boxplot()+ 
+   scale_y_continuous("Footprint [km²]", limits = c(0, 3500)) +
+   theme(legend.position="none")+
+   xlab("years")+
+   ylab("Footprint [km²]")+
+   ggtitle("EU") +
+   scale_fill_manual(values=c("#31688e", "#35b779"))
> #plot EU
> ggplot(plot_EU, aes(x=as.factor(year), y=complete_footprint, fill= type)) + 
+   geom_boxplot()+ 
+   scale_y_continuous("Footprint [km²]", limits = c(0, 3200)) +
+   theme(legend.position="none")+
+   xlab("years")+
+   ylab("Footprint [km²]")+
+   ggtitle("EU") +
+   scale_fill_manual(values=c("#31688e", "#35b779"))
> # plot China
> ggplot(plot_CHN, aes(x=as.factor(year), y=complete_footprint, fill= type)) + 
+   geom_boxplot()+ 
+   theme(legend.position="none")+
+   xlab("years")+
+   scale_y_continuous("Footprint [km²]", limits = c(0, 3200))+
+   ggtitle("CHN") +
+   scale_fill_manual(values=c("#31688e", "#35b779"))
> # plot China
> ggplot(plot_CHN, aes(x=as.factor(year), y=complete_footprint, fill= type)) + 
+   geom_boxplot()+ 
+   theme(legend.position="none")+
+   xlab("years")+
+   scale_y_continuous("Footprint [km²]", limits = c(0, 3500))+
+   ggtitle("CHN") +
+   scale_fill_manual(values=c("#31688e", "#35b779"))
> #plot EU
> ggplot(plot_EU, aes(x=as.factor(year), y=complete_footprint, fill= type)) + 
+   geom_boxplot()+ 
+   scale_y_continuous("Footprint [km²]", limits = c(0, 3500)) +
+   theme(legend.position="none")+
+   xlab("years")+
+   ylab("Footprint [km²]")+
+   ggtitle("EU") +
+   scale_fill_manual(values=c("#31688e", "#35b779"))
> l
Error: object 'l' not found
> #prepare plot data for CHINA
> plot_CHN <-  footprint_CHN %>%
+   group_by(year, amort, var) %>%
+   summarise(complete_footprint = sum(complete_footprint)) %>% 
+   ungroup() %>% 
+   mutate(year = as.character(year))
`summarise()` has grouped output by 'year', 'amort'. You can override using the `.groups` argument.
> avg <- plot_CHN %>% 
+   group_by(amort, var) %>% 
+   summarise(complete_footprint = sum(complete_footprint)/length(unique(plot_CHN$year))) %>% 
+   ungroup() %>% 
+   mutate(year = "average")
`summarise()` has grouped output by 'amort'. You can override using the `.groups` argument.
> plot_CHN <- bind_rows(plot_CHN, avg)%>%
+   mutate(type=ifelse(year=="average","Highlighted","Normal"))%>%
+   mutate(complete_footprint = complete_footprint / 100)
> # plot China
> ggplot(plot_CHN, aes(x=as.factor(year), y=complete_footprint, fill= type)) + 
+   geom_boxplot()+ 
+   theme(legend.position="none")+
+   xlab("years")+
+   scale_y_continuous("Footprint [km²]", limits = c(0, 3500))+
+   ggtitle("CHN") +
+   scale_fill_manual(values=c("#31688e", "#35b779"))
> footprint_CHN <- readRDS("~/Deforestation-calculation/Paper/footprint_CHN.rds")
> footprint_EU <- readRDS("~/Deforestation-calculation/Paper/footprint_EU.rds")
> View(footprint_CHN)
System has not been booted with systemd as init system (PID 1). Can't operate.
                      Failed to connect to bus: Host is down
                      
                      Attaching package: ‘maps’
                      
                      The following object is masked from ‘package:purrr’:
                        
                        map
                      
                      System has not been booted with systemd as init system (PID 1). Can't operate.
Failed to connect to bus: Host is down

Attaching package: ‘maps’

The following object is masked from ‘package:purrr’:

    map

> footprint_CHN <- readRDS("~/Deforestation-calculation/Paper/footprint_CHN.rds")
Warning message:
In system("timedatectl", intern = TRUE) :
  running command 'timedatectl' had status 1
> footprint_EU <- readRDS("~/Deforestation-calculation/Paper/footprint_EU.rds")
> footprint_CHN <- readRDS("~/Deforestation-calculation/Paper/footprint_CHN.rds")
> View(footprint_CHN)
> table_CHN <- footprint_CHN %>%
+   filter(iso == "dir")
> View(table_CHN)
> table_CHN <- footprint_CHN %>%
+   filter(luc == "dir")
> View(table_CHN)
> table_CHN <- footprint_CHN %>%
+   filter(luc == "dir")%>%
+   gather(key = "year", value = "complete_footprint")
Warning message:
attributes are not identical across measure variables;
they will be dropped 
> table_CHN <- footprint_CHN %>%
+   filter(luc == "dir")%>%
+   gather()
Warning message:
attributes are not identical across measure variables;
they will be dropped 
> table_CHN <- footprint_CHN %>%
+   filter(luc == "dir")#%>%
> table_CHN <- footprint_CHN %>%
+   filter(luc == "dir")%>%
+   select(!iso & !nonfood & !food)
Adding missing grouping variables: `iso`
> table_CHN <- footprint_CHN %>%
+   filter(luc == "dir")%>%
+   select(!iso & !nonfood & !food)
Adding missing grouping variables: `iso`
> table_CHN <- footprint_CHN %>%
+   filter(luc == "dir")%>%
+   select(!iso & !nonfood)
Adding missing grouping variables: `iso`
> table_CHN <- footprint_CHN %>%
+   filter(luc == "dir")%>%
+   select(!iso & !nonfood &!food)
Adding missing grouping variables: `iso`
> table_CHN <- footprint_CHN %>%
+   filter(luc == "dir")%>%
+   select(!iso & !nonfood &!food)%>%
+   select(!iso)
Adding missing grouping variables: `iso`
Adding missing grouping variables: `iso`
> table_CHN <- footprint_CHN %>%
+   filter(luc == "dir")%>%
+   select(!nonfood &!food)%>%
+   select(!iso)
Adding missing grouping variables: `iso`
> table_CHN <- footprint_CHN %>%
+   filter(luc == "dir")%>%
+   select(!nonfood &!food)
> table_CHN <- footprint_CHN %>%
+   filter(luc == "dir")%>%
+   select(!iso)
Adding missing grouping variables: `iso`
> table_CHN <- footprint_CHN %>%
+   filter(luc == "dir")%>%
+   select(!iso)
Adding missing grouping variables: `iso`
> table_CHN <- footprint_CHN %>%
+   select(!iso)
Adding missing grouping variables: `iso`
> table_CHN <- footprint_CHN %>%
+   filter(luc == "dir")%>%
+   ungroup()%>%
+   select(!iso & !nonfood &!food)
> table_CHN <- footprint_CHN %>%
+   filter(luc == "dir")%>%
+   ungroup()%>%
+   select(!iso & !nonfood &!food)%>%
+   gather()
> table_CHN <- footprint_CHN %>%
+   filter(luc == "dir")%>%
+   ungroup()%>%
+   select(!iso & !nonfood &!food)#%>%
>   gather(key = "complete_footprint")
Error in UseMethod("gather") : 
  no applicable method for 'gather' applied to an object of class "character"
> table_CHN <- footprint_CHN %>%
+   filter(luc == "dir")%>%
+   ungroup()%>%
+   select(!iso & !nonfood &!food)%>%
+   gather(key = "complete_footprint")
> table_CHN <- footprint_CHN %>%
+   filter(luc == "dir")%>%
+   ungroup()%>%
+   select(!iso & !nonfood &!food)#%>%
> table_CHN <- footprint_CHN %>%
+   filter(luc == "dir")%>%
+   ungroup()%>%
+   select(!iso & !nonfood &!food)%>%
+   mutate(amort_var = paste0("a",sprintf("%02d", amort),"_y",sprintf("%02d",var))) %>% 
+   select(-amort,-var)
> table_CHN <- footprint_CHN %>%
+   filter(luc == "dir")%>%
+   ungroup()%>%
+   select(!iso & !nonfood &!food)%>%
+   mutate(amort_var = paste0("a",sprintf("%02d", amort),"_y",sprintf("%02d",var))) %>% 
+   select(-amort,-var, -luc)
> table_CHN <- footprint_CHN %>%
+   filter(luc == "dir")%>%
+   ungroup()%>%
+   select(-iso, -nonfood, -food, -luc)%>%
+   mutate(amort_var = paste0("a",sprintf("%02d", amort),"_y",sprintf("%02d",var))) %>% 
+   select(-amort,-var)
> table_CHN <- footprint_CHN %>%
+   filter(luc == "dir")%>%
+   ungroup()%>%
+   select(-iso, -nonfood, -food, -luc)%>%
+   mutate(amort_var = paste0("a",sprintf("%02d", amort),"_y",sprintf("%02d",var))) %>% 
+   select(-amort,-var)%>%
+   spread(amort_var, complete_footprint)
> print.data.frame(table_CHN)
   year   a01_y01  a01_y05  a01_y10   a05_y01   a05_y05   a05_y10   a10_y01   a10_y05   a10_y10
1  2000  606.9784 14549.81 14962.74  219.2599  7009.764  8329.881  109.6300  4017.358  7500.774
2  2001  679.4944 16475.38 16942.96  384.1765 10071.942 11309.956  192.0882  6196.567  9432.287
3  2002  405.4681 20311.53 20887.98  554.7223 15048.583 16258.250  277.3612  9670.539 12785.949
4  2003 2761.7920 26809.58 27570.45 1284.5469 23336.244 24515.009  642.2735 15445.285 18404.135
5  2004 1332.6933 27525.98 28307.18 1400.2670 27525.978 28307.178  792.7055 18610.609 20464.468
6  2005  808.9509 10320.27 34347.41 1582.1812 28783.670 34347.414 1042.7495 22437.398 26734.453
7  2006  745.3480 13838.65 46057.13 1901.2251 32407.107 46057.127 1472.7782 29893.154 38400.850
8  2007  573.8823 11210.07 37308.83 1510.0297 21237.727 37308.826 1250.4202 24058.288 33174.153
9  2008 1909.1654 13025.60 43351.16 1267.9029 18851.436 43351.163 1643.8481 27772.416 40949.012
10 2009 3327.0429 17269.39 57475.15 1805.2170 17269.391 57475.151 2324.1659 36579.194 57475.151
11 2010 2096.8472 29904.06 40635.96 1986.3361 20088.762 55080.586 2344.9579 34636.679 56886.164
12 2011 3556.4767 31518.62 42829.95 2604.6569 23759.691 54248.333 2579.1221 33643.281 58054.460
13 2012 3986.2823 37596.78 51089.43 3677.1980 31426.658 60169.661 3331.8797 36715.490 66979.832
14 2013 9092.5895 41495.70 56387.58 5159.7175 38090.702 61398.516 3770.8406 36753.109 71420.394
> stargazer(table_CHN, type = 'text', out = 'out.txt')
Error in stargazer(table_CHN, type = "text", out = "out.txt") : 
  could not find function "stargazer"
> xtable(table_CHN)
Error in xtable(table_CHN) : could not find function "xtable"
> table_CHN_direct <- footprint_CHN %>%
+   filter(luc == "dir")%>%
+   ungroup()%>%
+   select(-iso, -nonfood, -food, -luc)%>%
+   mutate(amort_var = paste0("a",sprintf("%02d", amort),"_y",sprintf("%02d",var))) %>% 
+   select(-amort,-var)%>%
+   spread(amort_var, complete_footprint)
> table_CHN_indirect <- footprint_CHN %>%
+   filter(luc == "indir")%>%
+   ungroup()%>%
+   select(-iso, -nonfood, -food, -luc)%>%
+   mutate(amort_var = paste0("a",sprintf("%02d", amort),"_y",sprintf("%02d",var))) %>% 
+   select(-amort,-var)%>%
+   spread(amort_var, complete_footprint)
> table_EU_direct <- footprint_EU %>%
+   filter(luc == "dir")%>%
+   ungroup()%>%
+   select(-iso, -nonfood, -food, -luc)%>%
+   mutate(amort_var = paste0("a",sprintf("%02d", amort),"_y",sprintf("%02d",var))) %>% 
+   select(-amort,-var)%>%
+   spread(amort_var, complete_footprint)
Error: Each row of output must be identified by a unique combination of keys.
Keys are shared for 3402 rows:
* 1, 127, 253, 379, 505, 631, 757, 883, 1009, 1135, 1261, 1387, 1513, 1639, 1765, 1891, 2017, 2143, 2269, 2395, 2521, 2647, 2773, 2899, 3025, 3151, 3277
* 10, 136, 262, 388, 514, 640, 766, 892, 1018, 1144, 1270, 1396, 1522, 1648, 1774, 1900, 2026, 2152, 2278, 2404, 2530, 2656, 2782, 2908, 3034, 3160, 3286
* 19, 145, 271, 397, 523, 649, 775, 901, 1027, 1153, 1279, 1405, 1531, 1657, 1783, 1909, 2035, 2161, 2287, 2413, 2539, 2665, 2791, 2917, 3043, 3169, 3295
* 28, 154, 280, 406, 532, 658, 784, 910, 1036, 1162, 1288, 1414, 1540, 1666, 1792, 1918, 2044, 2170, 2296, 2422, 2548, 2674, 2800, 2926, 3052, 3178, 3304
* 37, 163, 289, 415, 541, 667, 793, 919, 1045, 1171, 1297, 1423, 1549, 1675, 1801, 1927, 2053, 2179, 2305, 2431, 2557, 2683, 2809, 2935, 3061, 3187, 3313
* 46, 172, 298, 424, 550, 676, 802, 928, 1054, 1180, 1306, 1432, 1558, 1684, 1810, 1936, 2062, 2188, 2314, 2440, 2566, 26
> table_EU_direct <- footprint_EU %>%
+   filter(luc == "dir")%>%
+   ungroup()%>%
+   select(-iso, -nonfood, -food, -luc)%>%
+   mutate(amort_var = paste0("a",sprintf("%02d", amort),"_y",sprintf("%02d",var))) %>% 
+   select(-amort,-var)%>%
+   spread(amort_var, complete_footprint)
Error: Each row of output must be identified by a unique combination of keys.
Keys are shared for 3402 rows:
* 1, 127, 253, 379, 505, 631, 757, 883, 1009, 1135, 1261, 1387, 1513, 1639, 1765, 1891, 2017, 2143, 2269, 2395, 2521, 2647, 2773, 2899, 3025, 3151, 3277
* 10, 136, 262, 388, 514, 640, 766, 892, 1018, 1144, 1270, 1396, 1522, 1648, 1774, 1900, 2026, 2152, 2278, 2404, 2530, 2656, 2782, 2908, 3034, 3160, 3286
* 19, 145, 271, 397, 523, 649, 775, 901, 1027, 1153, 1279, 1405, 1531, 1657, 1783, 1909, 2035, 2161, 2287, 2413, 2539, 2665, 2791, 2917, 3043, 3169, 3295
* 28, 154, 280, 406, 532, 658, 784, 910, 1036, 1162, 1288, 1414, 1540, 1666, 1792, 1918, 2044, 2170, 2296, 2422, 2548, 2674, 2800, 2926, 3052, 3178, 3304
* 37, 163, 289, 415, 541, 667, 793, 919, 1045, 1171, 1297, 1423, 1549, 1675, 1801, 1927, 2053, 2179, 2305, 2431, 2557, 2683, 2809, 2935, 3061, 3187, 3313
* 46, 172, 298, 424, 550, 676, 802, 928, 1054, 1180, 1306, 1432, 1558, 1684, 1810, 1936, 2062, 2188, 2314, 2440, 2566, 26
> table_EU_direct <- footprint_EU %>%
+   filter(luc == "dir")#%>%
> table_EU_direct <- footprint_EU %>%
+   filter(luc == "dir")%>%
+   ungroup()#%>%
> table_EU_direct <- footprint_EU %>%
+   filter(luc == "dir")%>%
+   ungroup()%>%
+   select(-iso, -nonfood, -food, -luc)#%>%
> table_EU_direct <- footprint_EU %>%
+   filter(luc == "dir")%>%
+   ungroup()%>%
+   select(-iso, -nonfood, -food, -luc)%>%
+   mutate(amort_var = paste0("a",sprintf("%02d", amort),"_y",sprintf("%02d",var)))# %>% 
> table_EU_direct <- footprint_EU %>%
+   filter(luc == "dir")%>%
+   ungroup()%>%
+   select(-iso, -nonfood, -food, -luc)%>%
+   mutate(amort_var = paste0("a",sprintf("%02d", amort),"_y",sprintf("%02d",var))) %>% 
+   select(-amort,-var)#%>%
> table_EU_direct <- footprint_EU %>%
+   filter(luc == "dir")%>%
+   ungroup()%>%
+   select(-iso, -nonfood, -food, -luc)%>%
+   mutate(amort_var = paste0("a",sprintf("%02d", amort),"_y",sprintf("%02d",var))) %>% 
+   select(-amort,-var)%>%
+   spread(amort_var, complete_footprint)
Error: Each row of output must be identified by a unique combination of keys.
Keys are shared for 3402 rows:
* 1, 127, 253, 379, 505, 631, 757, 883, 1009, 1135, 1261, 1387, 1513, 1639, 1765, 1891, 2017, 2143, 2269, 2395, 2521, 2647, 2773, 2899, 3025, 3151, 3277
* 10, 136, 262, 388, 514, 640, 766, 892, 1018, 1144, 1270, 1396, 1522, 1648, 1774, 1900, 2026, 2152, 2278, 2404, 2530, 2656, 2782, 2908, 3034, 3160, 3286
* 19, 145, 271, 397, 523, 649, 775, 901, 1027, 1153, 1279, 1405, 1531, 1657, 1783, 1909, 2035, 2161, 2287, 2413, 2539, 2665, 2791, 2917, 3043, 3169, 3295
* 28, 154, 280, 406, 532, 658, 784, 910, 1036, 1162, 1288, 1414, 1540, 1666, 1792, 1918, 2044, 2170, 2296, 2422, 2548, 2674, 2800, 2926, 3052, 3178, 3304
* 37, 163, 289, 415, 541, 667, 793, 919, 1045, 1171, 1297, 1423, 1549, 1675, 1801, 1927, 2053, 2179, 2305, 2431, 2557, 2683, 2809, 2935, 3061, 3187, 3313
* 46, 172, 298, 424, 550, 676, 802, 928, 1054, 1180, 1306, 1432, 1558, 1684, 1810, 1936, 2062, 2188, 2314, 2440, 2566, 26
> table_EU_direct <- footprint_EU %>%
+   filter(luc == "dir")%>%
+   ungroup()%>%
+   select(-iso, -nonfood, -food, -luc)%>%
+   mutate(amort_var = paste0("a",sprintf("%02d", amort),"_y",sprintf("%02d",var))) %>% 
+   select(-amort,-var)#%>%
> View(table_EU_direct)
> table_EU_direct <- footprint_EU %>%
+   filter(luc == "dir")%>%
+   ungroup()%>%
+   select(-iso, -nonfood, -food, -luc)%>%
+   mutate(amort_var = paste0("a",sprintf("%02d", amort),"_y",sprintf("%02d",var))) %>% 
+   select(-amort,-var)%>%
+   spread(amort_var, complete_footprint)
Error: Each row of output must be identified by a unique combination of keys.
Keys are shared for 3402 rows:
* 1, 127, 253, 379, 505, 631, 757, 883, 1009, 1135, 1261, 1387, 1513, 1639, 1765, 1891, 2017, 2143, 2269, 2395, 2521, 2647, 2773, 2899, 3025, 3151, 3277
* 10, 136, 262, 388, 514, 640, 766, 892, 1018, 1144, 1270, 1396, 1522, 1648, 1774, 1900, 2026, 2152, 2278, 2404, 2530, 2656, 2782, 2908, 3034, 3160, 3286
* 19, 145, 271, 397, 523, 649, 775, 901, 1027, 1153, 1279, 1405, 1531, 1657, 1783, 1909, 2035, 2161, 2287, 2413, 2539, 2665, 2791, 2917, 3043, 3169, 3295
* 28, 154, 280, 406, 532, 658, 784, 910, 1036, 1162, 1288, 1414, 1540, 1666, 1792, 1918, 2044, 2170, 2296, 2422, 2548, 2674, 2800, 2926, 3052, 3178, 3304
* 37, 163, 289, 415, 541, 667, 793, 919, 1045, 1171, 1297, 1423, 1549, 1675, 1801, 1927, 2053, 2179, 2305, 2431, 2557, 2683, 2809, 2935, 3061, 3187, 3313
* 46, 172, 298, 424, 550, 676, 802, 928, 1054, 1180, 1306, 1432, 1558, 1684, 1810, 1936, 2062, 2188, 2314, 2440, 2566, 26
> table_EU_direct <- footprint_EU %>%
+   filter(luc == "dir")%>%
+   ungroup()%>%
+   select(-iso, -nonfood, -food, -luc)%>%
+   mutate(amort_var = paste0("a",sprintf("%02d", amort),"_y",sprintf("%02d",var))) %>% 
+   select(-amort,-var)%>%
+   spread(amort_var, complete_footprint)
Error: Each row of output must be identified by a unique combination of keys.
Keys are shared for 3402 rows:
* 1, 127, 253, 379, 505, 631, 757, 883, 1009, 1135, 1261, 1387, 1513, 1639, 1765, 1891, 2017, 2143, 2269, 2395, 2521, 2647, 2773, 2899, 3025, 3151, 3277
* 10, 136, 262, 388, 514, 640, 766, 892, 1018, 1144, 1270, 1396, 1522, 1648, 1774, 1900, 2026, 2152, 2278, 2404, 2530, 2656, 2782, 2908, 3034, 3160, 3286
* 19, 145, 271, 397, 523, 649, 775, 901, 1027, 1153, 1279, 1405, 1531, 1657, 1783, 1909, 2035, 2161, 2287, 2413, 2539, 2665, 2791, 2917, 3043, 3169, 3295
* 28, 154, 280, 406, 532, 658, 784, 910, 1036, 1162, 1288, 1414, 1540, 1666, 1792, 1918, 2044, 2170, 2296, 2422, 2548, 2674, 2800, 2926, 3052, 3178, 3304
* 37, 163, 289, 415, 541, 667, 793, 919, 1045, 1171, 1297, 1423, 1549, 1675, 1801, 1927, 2053, 2179, 2305, 2431, 2557, 2683, 2809, 2935, 3061, 3187, 3313
* 46, 172, 298, 424, 550, 676, 802, 928, 1054, 1180, 1306, 1432, 1558, 1684, 1810, 1936, 2062, 2188, 2314, 2440, 2566, 26
> table_CHN_indirect <- footprint_CHN %>%
+   filter(luc == "indir")%>%
+   ungroup()%>%
+   select(-iso, -nonfood, -food, -luc)%>%
+   mutate(amort_var = paste0("a",sprintf("%02d", amort),"_y",sprintf("%02d",var))) %>% 
+   select(-amort,-var)%>%
+   spread(amort_var, complete_footprint)
> table_CHN_direct <- footprint_CHN %>%
+   filter(luc == "dir")%>%
+   ungroup()%>%
+   select(-iso, -nonfood, -food, -luc)%>%
+   mutate(amort_var = paste0("a",sprintf("%02d", amort),"_y",sprintf("%02d",var))) %>% 
+   select(-amort,-var)%>%
+   spread(amort_var, complete_footprint)
> table_EU_direct <- footprint_EU %>%
+   filter(luc == "dir")%>%
+   ungroup()%>%
+   select(-iso, -nonfood, -food, -luc)%>%
+   mutate(amort_var = paste0("a",sprintf("%02d", amort),"_y",sprintf("%02d",var))) %>% 
+   select(-amort,-var)%>%
+   spread(amort_var, complete_footprint)
Error: Each row of output must be identified by a unique combination of keys.
Keys are shared for 3402 rows:
* 1, 127, 253, 379, 505, 631, 757, 883, 1009, 1135, 1261, 1387, 1513, 1639, 1765, 1891, 2017, 2143, 2269, 2395, 2521, 2647, 2773, 2899, 3025, 3151, 3277
* 10, 136, 262, 388, 514, 640, 766, 892, 1018, 1144, 1270, 1396, 1522, 1648, 1774, 1900, 2026, 2152, 2278, 2404, 2530, 2656, 2782, 2908, 3034, 3160, 3286
* 19, 145, 271, 397, 523, 649, 775, 901, 1027, 1153, 1279, 1405, 1531, 1657, 1783, 1909, 2035, 2161, 2287, 2413, 2539, 2665, 2791, 2917, 3043, 3169, 3295
* 28, 154, 280, 406, 532, 658, 784, 910, 1036, 1162, 1288, 1414, 1540, 1666, 1792, 1918, 2044, 2170, 2296, 2422, 2548, 2674, 2800, 2926, 3052, 3178, 3304
* 37, 163, 289, 415, 541, 667, 793, 919, 1045, 1171, 1297, 1423, 1549, 1675, 1801, 1927, 2053, 2179, 2305, 2431, 2557, 2683, 2809, 2935, 3061, 3187, 3313
* 46, 172, 298, 424, 550, 676, 802, 928, 1054, 1180, 1306, 1432, 1558, 1684, 1810, 1936, 2062, 2188, 2314, 2440, 2566, 26
> table_EU_indirect <- footprint_EU %>%
+   filter(luc == "indir")%>%
+   ungroup()%>%
+   select(-iso, -nonfood, -food, -luc)%>%
+   mutate(amort_var = paste0("a",sprintf("%02d", amort),"_y",sprintf("%02d",var))) %>% 
+   select(-amort,-var)%>%
+   spread(amort_var, complete_footprint)
Error: Each row of output must be identified by a unique combination of keys.
Keys are shared for 3402 rows:
* 1, 127, 253, 379, 505, 631, 757, 883, 1009, 1135, 1261, 1387, 1513, 1639, 1765, 1891, 2017, 2143, 2269, 2395, 2521, 2647, 2773, 2899, 3025, 3151, 3277
* 10, 136, 262, 388, 514, 640, 766, 892, 1018, 1144, 1270, 1396, 1522, 1648, 1774, 1900, 2026, 2152, 2278, 2404, 2530, 2656, 2782, 2908, 3034, 3160, 3286
* 19, 145, 271, 397, 523, 649, 775, 901, 1027, 1153, 1279, 1405, 1531, 1657, 1783, 1909, 2035, 2161, 2287, 2413, 2539, 2665, 2791, 2917, 3043, 3169, 3295
* 28, 154, 280, 406, 532, 658, 784, 910, 1036, 1162, 1288, 1414, 1540, 1666, 1792, 1918, 2044, 2170, 2296, 2422, 2548, 2674, 2800, 2926, 3052, 3178, 3304
* 37, 163, 289, 415, 541, 667, 793, 919, 1045, 1171, 1297, 1423, 1549, 1675, 1801, 1927, 2053, 2179, 2305, 2431, 2557, 2683, 2809, 2935, 3061, 3187, 3313
* 46, 172, 298, 424, 550, 676, 802, 928, 1054, 1180, 1306, 1432, 1558, 1684, 1810, 1936, 2062, 2188, 2314, 2440, 2566, 26
> table_EU_indirect <- footprint_EU %>%
+   filter(luc == "indir")%>%
+   ungroup()%>%
+   select(-iso, -nonfood, -food, -luc)%>%
+   mutate(amort_var = paste0("a",sprintf("%02d", amort),"_y",sprintf("%02d",var))) %>% 
+   select(-amort,-var)%>%
+   ungroup()%>%
+   spread(amort_var, complete_footprint)
Error: Each row of output must be identified by a unique combination of keys.
Keys are shared for 3402 rows:
* 1, 127, 253, 379, 505, 631, 757, 883, 1009, 1135, 1261, 1387, 1513, 1639, 1765, 1891, 2017, 2143, 2269, 2395, 2521, 2647, 2773, 2899, 3025, 3151, 3277
* 10, 136, 262, 388, 514, 640, 766, 892, 1018, 1144, 1270, 1396, 1522, 1648, 1774, 1900, 2026, 2152, 2278, 2404, 2530, 2656, 2782, 2908, 3034, 3160, 3286
* 19, 145, 271, 397, 523, 649, 775, 901, 1027, 1153, 1279, 1405, 1531, 1657, 1783, 1909, 2035, 2161, 2287, 2413, 2539, 2665, 2791, 2917, 3043, 3169, 3295
* 28, 154, 280, 406, 532, 658, 784, 910, 1036, 1162, 1288, 1414, 1540, 1666, 1792, 1918, 2044, 2170, 2296, 2422, 2548, 2674, 2800, 2926, 3052, 3178, 3304
* 37, 163, 289, 415, 541, 667, 793, 919, 1045, 1171, 1297, 1423, 1549, 1675, 1801, 1927, 2053, 2179, 2305, 2431, 2557, 2683, 2809, 2935, 3061, 3187, 3313
* 46, 172, 298, 424, 550, 676, 802, 928, 1054, 1180, 1306, 1432, 1558, 1684, 1810, 1936, 2062, 2188, 2314, 2440, 2566, 26
> table_EU_indirect <- footprint_EU %>%
+   filter(luc == "indir")%>%
+   ungroup()%>%
+   select(-iso, -nonfood, -food, -luc)%>%
+   mutate(amort_var = paste0("a",sprintf("%02d", amort),"_y",sprintf("%02d",var))) %>% 
+   select(-amort,-var)%>%
+   spread(amort_var, complete_footprint)
Error: Each row of output must be identified by a unique combination of keys.
Keys are shared for 3402 rows:
* 1, 127, 253, 379, 505, 631, 757, 883, 1009, 1135, 1261, 1387, 1513, 1639, 1765, 1891, 2017, 2143, 2269, 2395, 2521, 2647, 2773, 2899, 3025, 3151, 3277
* 10, 136, 262, 388, 514, 640, 766, 892, 1018, 1144, 1270, 1396, 1522, 1648, 1774, 1900, 2026, 2152, 2278, 2404, 2530, 2656, 2782, 2908, 3034, 3160, 3286
* 19, 145, 271, 397, 523, 649, 775, 901, 1027, 1153, 1279, 1405, 1531, 1657, 1783, 1909, 2035, 2161, 2287, 2413, 2539, 2665, 2791, 2917, 3043, 3169, 3295
* 28, 154, 280, 406, 532, 658, 784, 910, 1036, 1162, 1288, 1414, 1540, 1666, 1792, 1918, 2044, 2170, 2296, 2422, 2548, 2674, 2800, 2926, 3052, 3178, 3304
* 37, 163, 289, 415, 541, 667, 793, 919, 1045, 1171, 1297, 1423, 1549, 1675, 1801, 1927, 2053, 2179, 2305, 2431, 2557, 2683, 2809, 2935, 3061, 3187, 3313
* 46, 172, 298, 424, 550, 676, 802, 928, 1054, 1180, 1306, 1432, 1558, 1684, 1810, 1936, 2062, 2188, 2314, 2440, 2566, 26
> table_EU_indirect <- footprint_EU %>%
+   filter(luc == "indir")%>%
+   ungroup()%>%
+   select(-iso, -nonfood, -food, -luc)%>%
+   mutate(amort_var = paste0("a",sprintf("%02d", amort),"_y",sprintf("%02d",var))) %>% 
+   select(-amort,-var)%>%
+   spread(amort_var, complete_footprint)
Error: Each row of output must be identified by a unique combination of keys.
Keys are shared for 3402 rows:
* 1, 127, 253, 379, 505, 631, 757, 883, 1009, 1135, 1261, 1387, 1513, 1639, 1765, 1891, 2017, 2143, 2269, 2395, 2521, 2647, 2773, 2899, 3025, 3151, 3277
* 10, 136, 262, 388, 514, 640, 766, 892, 1018, 1144, 1270, 1396, 1522, 1648, 1774, 1900, 2026, 2152, 2278, 2404, 2530, 2656, 2782, 2908, 3034, 3160, 3286
* 19, 145, 271, 397, 523, 649, 775, 901, 1027, 1153, 1279, 1405, 1531, 1657, 1783, 1909, 2035, 2161, 2287, 2413, 2539, 2665, 2791, 2917, 3043, 3169, 3295
* 28, 154, 280, 406, 532, 658, 784, 910, 1036, 1162, 1288, 1414, 1540, 1666, 1792, 1918, 2044, 2170, 2296, 2422, 2548, 2674, 2800, 2926, 3052, 3178, 3304
* 37, 163, 289, 415, 541, 667, 793, 919, 1045, 1171, 1297, 1423, 1549, 1675, 1801, 1927, 2053, 2179, 2305, 2431, 2557, 2683, 2809, 2935, 3061, 3187, 3313
* 46, 172, 298, 424, 550, 676, 802, 928, 1054, 1180, 1306, 1432, 1558, 1684, 1810, 1936, 2062, 2188, 2314, 2440, 2566, 26
> table_EU_direct <- footprint_EU %>%
+   filter(luc == "dir")%>%
+   ungroup()%>%
+   select(-iso, -nonfood, -food, -luc)%>%
+   mutate(amort_var = paste0("a",sprintf("%02d", amort),"_y",sprintf("%02d",var))) %>% 
+   select(-amort,-var)%>%
+   spread(amort_var, complete_footprint)
Error: Each row of output must be identified by a unique combination of keys.
Keys are shared for 3402 rows:
* 1, 127, 253, 379, 505, 631, 757, 883, 1009, 1135, 1261, 1387, 1513, 1639, 1765, 1891, 2017, 2143, 2269, 2395, 2521, 2647, 2773, 2899, 3025, 3151, 3277
* 10, 136, 262, 388, 514, 640, 766, 892, 1018, 1144, 1270, 1396, 1522, 1648, 1774, 1900, 2026, 2152, 2278, 2404, 2530, 2656, 2782, 2908, 3034, 3160, 3286
* 19, 145, 271, 397, 523, 649, 775, 901, 1027, 1153, 1279, 1405, 1531, 1657, 1783, 1909, 2035, 2161, 2287, 2413, 2539, 2665, 2791, 2917, 3043, 3169, 3295
* 28, 154, 280, 406, 532, 658, 784, 910, 1036, 1162, 1288, 1414, 1540, 1666, 1792, 1918, 2044, 2170, 2296, 2422, 2548, 2674, 2800, 2926, 3052, 3178, 3304
* 37, 163, 289, 415, 541, 667, 793, 919, 1045, 1171, 1297, 1423, 1549, 1675, 1801, 1927, 2053, 2179, 2305, 2431, 2557, 2683, 2809, 2935, 3061, 3187, 3313
* 46, 172, 298, 424, 550, 676, 802, 928, 1054, 1180, 1306, 1432, 1558, 1684, 1810, 1936, 2062, 2188, 2314, 2440, 2566, 26
> table_EU_direct <- footprint_EU %>%
+   filter(luc == "dir")%>%
+   ungroup()%>%
+   select(-iso, -nonfood, -food, -luc)%>%
+   mutate(amort_var = paste0("a",sprintf("%02d", amort),"_y",sprintf("%02d",var))) %>% 
+   select(-amort,-var)%>%
+   spread(complete_footprint, amort_var)
Error: Each row of output must be identified by a unique combination of keys.
Keys are shared for 459 rows:
* 2225, 2228
* 3233, 3236
* 2477, 2480
* 965, 968
* 461, 464
* 2351, 2354
* 2217, 2220
* 2208, 2211
* 2469, 2472
* 2226, 2229, 2232
* 2451, 2454
* 2099, 2102
* 3234, 3237, 3240
* 957, 960
* 2478, 2481, 2484
* 966, 969, 972
* 2199, 2202
* 2190, 2193
* 2432, 2435
* 2460, 2463
* 2433, 2436
* 948, 951
* 2442, 2445
* 2180, 2183
* 2181, 2184
* 939, 942
* 426, 429
* 444, 447
* 462, 465, 468
* 416, 419
* 417, 420
* 435, 438
* 839, 842
* 2352, 2355, 2358
* 1847, 1850
* 3359, 3362
* 1217, 1220
* 335, 338
* 2603, 2606
* 920, 923
* 2343, 2346
* 921, 924
* 2334, 2337
* 3207, 3210
* 2306, 2309
* 2307, 2310
* 2082, 2085
* 453, 456
* 2100, 2103, 2106
* 1595, 1598
* 2325, 2328
* 3216, 3219
* 3225, 3228
* 83, 86
* 1721, 1724
* 2054, 2057
* 2055, 2058
* 2316, 2319
* 2091, 2094
* 2073, 2076
* 930, 933
* 1200, 1203
* 2064, 2067
* 3107, 3110
* 1839, 1842
* 1191, 1194
* 327, 330
* 3198, 3201
* 840, 84
> table_EU_direct <- footprint_EU %>%
+   filter(luc == "dir")%>%
+   ungroup()%>%
+   select(-iso, -nonfood, -food, -luc)%>%
+   mutate(amort_var = paste0("a",sprintf("%02d", amort),"_y",sprintf("%02d",var))) %>% 
+   select(-amort,-var)%>%
+   spread(amort_var, complete_footprint)
Error: Each row of output must be identified by a unique combination of keys.
Keys are shared for 3402 rows:
* 1, 127, 253, 379, 505, 631, 757, 883, 1009, 1135, 1261, 1387, 1513, 1639, 1765, 1891, 2017, 2143, 2269, 2395, 2521, 2647, 2773, 2899, 3025, 3151, 3277
* 10, 136, 262, 388, 514, 640, 766, 892, 1018, 1144, 1270, 1396, 1522, 1648, 1774, 1900, 2026, 2152, 2278, 2404, 2530, 2656, 2782, 2908, 3034, 3160, 3286
* 19, 145, 271, 397, 523, 649, 775, 901, 1027, 1153, 1279, 1405, 1531, 1657, 1783, 1909, 2035, 2161, 2287, 2413, 2539, 2665, 2791, 2917, 3043, 3169, 3295
* 28, 154, 280, 406, 532, 658, 784, 910, 1036, 1162, 1288, 1414, 1540, 1666, 1792, 1918, 2044, 2170, 2296, 2422, 2548, 2674, 2800, 2926, 3052, 3178, 3304
* 37, 163, 289, 415, 541, 667, 793, 919, 1045, 1171, 1297, 1423, 1549, 1675, 1801, 1927, 2053, 2179, 2305, 2431, 2557, 2683, 2809, 2935, 3061, 3187, 3313
* 46, 172, 298, 424, 550, 676, 802, 928, 1054, 1180, 1306, 1432, 1558, 1684, 1810, 1936, 2062, 2188, 2314, 2440, 2566, 26
> footprint_EU <- readRDS("~/Deforestation-calculation/Paper/footprint_EU.rds")
> table_EU_direct <- footprint_EU %>%
+   filter(luc == "dir")%>%
+   ungroup()%>%
+   select(-iso, -nonfood, -food, -luc)%>%
+   mutate(amort_var = paste0("a",sprintf("%02d", amort),"_y",sprintf("%02d",var))) %>% 
+   select(-amort,-var)#%>%
> table_EU_indirect <- footprint_EU %>%
+   filter(luc == "indir")%>%
+   ungroup()%>%
+   select(-iso, -nonfood, -food, -luc)%>%
+   mutate(amort_var = paste0("a",sprintf("%02d", amort),"_y",sprintf("%02d",var))) %>% 
+   select(-amort,-var)#%>%
> table_EU_indirect <- footprint_EU %>%
+   filter(luc == "indir")%>%
+   ungroup()%>%
+   select(-iso, -nonfood, -food, -luc)%>%
+   mutate(amort_var = paste0("a",sprintf("%02d", amort),"_y",sprintf("%02d",var))) %>% 
+   select(-amort,-var)%>%
+   spread(amort_var, complete_footprint) # warum geht das nicht ?????
Error: Each row of output must be identified by a unique combination of keys.
Keys are shared for 3402 rows:
* 1, 127, 253, 379, 505, 631, 757, 883, 1009, 1135, 1261, 1387, 1513, 1639, 1765, 1891, 2017, 2143, 2269, 2395, 2521, 2647, 2773, 2899, 3025, 3151, 3277
* 10, 136, 262, 388, 514, 640, 766, 892, 1018, 1144, 1270, 1396, 1522, 1648, 1774, 1900, 2026, 2152, 2278, 2404, 2530, 2656, 2782, 2908, 3034, 3160, 3286
* 19, 145, 271, 397, 523, 649, 775, 901, 1027, 1153, 1279, 1405, 1531, 1657, 1783, 1909, 2035, 2161, 2287, 2413, 2539, 2665, 2791, 2917, 3043, 3169, 3295
* 28, 154, 280, 406, 532, 658, 784, 910, 1036, 1162, 1288, 1414, 1540, 1666, 1792, 1918, 2044, 2170, 2296, 2422, 2548, 2674, 2800, 2926, 3052, 3178, 3304
* 37, 163, 289, 415, 541, 667, 793, 919, 1045, 1171, 1297, 1423, 1549, 1675, 1801, 1927, 2053, 2179, 2305, 2431, 2557, 2683, 2809, 2935, 3061, 3187, 3313
* 46, 172, 298, 424, 550, 676, 802, 928, 1054, 1180, 1306, 1432, 1558, 1684, 1810, 1936, 2062, 2188, 2314, 2440, 2566, 26
ggplot(facet_data, aes(x = year, y = value, fill = factor(luc, levels=c("indir", "dir")))) +
  geom_area()+
  facet_wrap(vars(amort_var))+
  ylab("deforestation [km²]")+
  labs(fill = "LUC")+
  scale_fill_manual(values = c("#90d743", "#443983"))
scale_fill_manual(values = c( "#443983", "#90d743")
                  ggplot(facet_data, aes(x = year, y = value, fill = factor(luc, levels=c("indir", "dir")))) +
                    geom_area()+
                    facet_wrap(vars(amort_var))+
                    ylab("deforestation [km²]")+
                    labs(fill = "LUC")+
                    scale_fill_manual(values = c( "#443983", "#90d743"))
                  ggplot(facet_data, aes(x = year, y = value, fill = factor(luc, levels=c("indir", "dir")))) +
                    geom_area()+
                    facet_wrap(vars(amort_var))+
                    ylab("deforestation [km²]")+
                    labs(fill = "LUC")+
                    scale_fill_manual(values = c( "#443983", "#90d743"))
                  ggplot(facet_data, aes(x = year, y = value, fill = factor(luc, levels=c("indir", "dir")))) +
                    geom_area()+
                    facet_wrap(vars(amort_var))+
                    ylab("deforestation [km²]")+
                    labs(fill = "LUC")+
                    scale_fill_manual(values = c( "#2c728e", "#addc30"))
                  ggplot(facet_data, aes(x = year, y = value, fill = factor(luc, levels=c("indir", "dir")))) +
                    geom_area()+
                    facet_wrap(vars(amort_var))+
                    ylab("deforestation [km²]")+
                    labs(fill = "LUC")+
                    scale_fill_manual(values = c( "#31688e", "#addc30"))
                  ggplot(facet_data, aes(x = year, y = value, fill = factor(luc, levels=c("indir", "dir")))) +
                    geom_area()+
                    facet_wrap(vars(amort_var))+
                    ylab("deforestation [km²]")+
                    labs(fill = "LUC")+
                    scale_fill_manual(values = c( "#31688e", "#35b779"))
                  ggplot(facet_data, aes(x = year, y = value, fill = factor(luc, levels=c("indir", "dir")))) +
                    geom_area()+
                    facet_wrap(vars(amort_var))+
                    ylab("deforestation [km²]")+
                    labs(fill = "LUC")+
                    scale_fill_manual(values = c( "#31688e", "#35b779"))
                  ggplot(facet_data, aes(x = year, y = value, fill = factor(luc, levels=c("indir", "dir")))) +
                    geom_area()+
                    facet_wrap(vars(amort_var))+
                    ylab("deforestation [km²]")+
                    labs(fill = "LUC")+
                    scale_fill_manual(values = c( "#31688e", "#addc30"))
                  g
                  ggplot(facet_data, aes(x = year, y = value, fill = factor(luc, levels=c("indir", "dir")))) +
                    geom_area()+
                    facet_wrap(vars(amort_var))+
                    ylab("deforestation [km²]")+
                    labs(fill = "LUC")+
                    scale_fill_manual(values = c( "#31688e", "#addc30"))
                  #load data
                  defor <- readRDS("~/Deforestation-calculation/input/defor.rds")
                  # summarized direct+indirect
                  sens_table <- readRDS("~/Deforestation-calculation/Analysis_deforestation/sens_table.rds")
                  fun <- function(x){
                    round(((x/sens_table[,6])-1) * 100, 0)
                  }
                  plot_data <- data.frame(apply(sens_table,2, fun))
                  colnames(plot_data) <- colnames(sens_table)
                  plot_data$year <- sens_table$year
                  plot_data <- plot_data %>%
                    gather("key","value",-year) %>%
                    mutate(amort = paste0("a", sprintf("%02d", as.numeric(substr(key, regexpr("a", key)+1,10))))) %>%
                    mutate(intervall = paste0("y", sprintf("%02d", as.numeric(substr(key, regexpr("y", key)+1,regexpr("a", key)-1))))) %>%
                    select(-key)
                  ggplot(plot_data, aes(x= year, y= value, colour = amort))+
                    geom_line(aes(linetype=intervall, color = amort))+
                    ylab("deviation [%]")+
                    #scale_colour_viridis_d("amortization", direction = -1)+
                    scale_linetype_manual(values=c("twodash", "dotted", "solid"))+
                    #labs(colour = "system definitions")
                    scale_colour_manual(values = c("#5ec962", "#3b528b","#440154") )
                  rm(list = ls())
                  #load data
                  defor <- readRDS("~/Deforestation-calculation/input/defor.rds")
                  # summarized direct+indirect
                  sens_table <- readRDS("~/Deforestation-calculation/Analysis_deforestation/sens_table.rds")
                  plot_data <- data.frame(apply(sens_table,2, fun))
                  colnames(plot_data) <- colnames(sens_table)
                  plot_data <- plot_data %>%
                    gather("key","value",-year) %>%
                    mutate(amort = paste0("a", sprintf("%02d", as.numeric(substr(key, regexpr("a", key)+1,10))))) %>%
                    mutate(intervall = paste0("y", sprintf("%02d", as.numeric(substr(key, regexpr("y", key)+1,regexpr("a", key)-1))))) %>%
                    select(-key)
                  plot_data$year <- sens_table$year
                  fun <- function(x){
                    round(((x/sens_table[,6])-1) * 100, 0)
                  }
                  ggplot(plot_data, aes(x= year, y= value, colour = amort))+
                    geom_line(aes(linetype=intervall, color = amort))+
                    ylab("deviation [%]")+
                    #scale_colour_viridis_d("amortization", direction = -1)+
                    scale_linetype_manual(values=c("twodash", "dotted", "solid"))+
                    #labs(colour = "system definitions")
                    scale_colour_manual(values = c("#5ec962", "#3b528b","#440154") )
                  # summarized direct+indirect
                  sens_table <- readRDS("~/Deforestation-calculation/Analysis_deforestation/sens_table.rds")
                  fun <- function(x){
                    round(((x/sens_table[,6])-1) * 100, 0)
                  }
                  plot_data <- data.frame(apply(sens_table,2, fun))
                  colnames(plot_data) <- colnames(sens_table)
                  plot_data <- plot_data %>%
                    gather("key","value",-year) %>%
                    mutate(amort = paste0("a", sprintf("%02d", as.numeric(substr(key, regexpr("a", key)+1,10))))) %>%
                    mutate(intervall = paste0("y", sprintf("%02d", as.numeric(substr(key, regexpr("y", key)+1,regexpr("a", key)-1))))) %>%
                    select(-key)
                  plot_data$year <- sens_table$year
                  ggplot(plot_data, aes(x= year, y= value, colour = amort))+
                    geom_line(aes(linetype=intervall, color = amort))+
                    ylab("deviation [%]")+
                    #scale_colour_viridis_d("amortization", direction = -1)+
                    scale_linetype_manual(values=c("twodash", "dotted", "solid"))+
                    #labs(colour = "system definitions")
                    scale_colour_manual(values = c("#5ec962", "#3b528b","#440154") )
                  footprint_CHN <- readRDS("~/Deforestation-calculation/Paper/footprint_CHN.rds")
                  footprint_EU <- readRDS("~/Deforestation-calculation/Paper/footprint_EU.rds")
                  # aggregate EU countries to EU total
                  plot_EU <- footprint_EU %>%
                    group_by(year, amort, var) %>%
                    summarise(complete_footprint = sum(complete_footprint)) %>%
                    ungroup() %>%
                    mutate(year = as.character(year))
                  avg <- plot_EU %>%
                    group_by(amort, var) %>%
                    summarise(complete_footprint = sum(complete_footprint)/length(unique(plot_EU$year))) %>%
                    ungroup() %>%
                    mutate(year = "average")
                  plot_EU <- bind_rows(plot_EU, avg)%>%
                    mutate(type=ifelse(year=="average","Highlighted","Normal"))%>%
                    mutate(complete_footprint = complete_footprint / 100)
                  rm(avg)
                  #plot EU
                  ggplot(plot_EU, aes(x=as.factor(year), y=complete_footprint, fill= type)) +
                    geom_boxplot()+
                    theme(legend.position="none")+
                    xlab("years")+
                    ylab("Footprint [km²]")+
                    ggtitle("EU") +
                    scale_fill_manual(values=c("#31688e", "#35b779"))
                  #prepare plot data for CHINA
                  plot_CHN <-  footprint_CHN %>%
                    group_by(year, amort, var) %>%
                    summarise(complete_footprint = sum(complete_footprint)) %>%
                    ungroup() %>%
                    mutate(year = as.character(year))
                  avg <- plot_CHN %>%
                    group_by(amort, var) %>%
                    summarise(complete_footprint = sum(complete_footprint)/length(unique(plot_CHN$year))) %>%
                    ungroup() %>%
                    mutate(year = "average")
                  plot_CHN <- bind_rows(plot_CHN, avg)%>%
                    mutate(type=ifelse(year=="average","Highlighted","Normal"))%>%
                    mutate(complete_footprint = complete_footprint / 100)
                  # plot China
                  ggplot(plot_CHN, aes(x=as.factor(year), y=complete_footprint, fill= type)) +
                    geom_boxplot()+
                    theme(legend.position="none")+
                    xlab("years")+
                    ylab("Footprint [km²]")+
                    ggtitle("CHN") +
                    scale_fill_manual(values=c("#31688e", "#35b779"))
                  View(plot_EU)
                  filter(plot_EU, year == 2000)
                  test <- filter(plot_EU, year == 2000)
                  View(test)
                  rm(test)
                  filter(plot_EU, year == 2001)
                  filter(plot_CHN, year == 2001)
                  #plot EU
                  ggplot(plot_EU, aes(x=as.factor(year), y=complete_footprint, fill= type)) +
                    geom_boxplot()+
                    scale_y_continuous("Footprint [km²]", limits = c(0, 3500)) +
                    theme(legend.position="none")+
                    xlab("years")+
                    ylab("Footprint [km²]")+
                    ggtitle("EU") +
                    scale_fill_manual(values=c("#31688e", "#35b779"))
                  #plot EU
                  ggplot(plot_EU, aes(x=as.factor(year), y=complete_footprint, fill= type)) +
                    geom_boxplot()+
                    scale_y_continuous("Footprint [km²]", limits = c(0, 3200)) +
                    theme(legend.position="none")+
                    xlab("years")+
                    ylab("Footprint [km²]")+
                    ggtitle("EU") +
                    scale_fill_manual(values=c("#31688e", "#35b779"))
                  # plot China
                  ggplot(plot_CHN, aes(x=as.factor(year), y=complete_footprint, fill= type)) +
                    geom_boxplot()+
                    theme(legend.position="none")+
                    xlab("years")+
                    scale_y_continuous("Footprint [km²]", limits = c(0, 3200))+
                    ggtitle("CHN") +
                    scale_fill_manual(values=c("#31688e", "#35b779"))
                  # plot China
                  ggplot(plot_CHN, aes(x=as.factor(year), y=complete_footprint, fill= type)) +
                    geom_boxplot()+
                    theme(legend.position="none")+
                    xlab("years")+
                    scale_y_continuous("Footprint [km²]", limits = c(0, 3500))+
                    ggtitle("CHN") +
                    scale_fill_manual(values=c("#31688e", "#35b779"))
                  #plot EU
                  ggplot(plot_EU, aes(x=as.factor(year), y=complete_footprint, fill= type)) +
                    geom_boxplot()+
                    scale_y_continuous("Footprint [km²]", limits = c(0, 3500)) +
                    theme(legend.position="none")+
                    xlab("years")+
                    ylab("Footprint [km²]")+
                    ggtitle("EU") +
                    scale_fill_manual(values=c("#31688e", "#35b779"))
                  l
                  #prepare plot data for CHINA
                  plot_CHN <-  footprint_CHN %>%
                    group_by(year, amort, var) %>%
                    summarise(complete_footprint = sum(complete_footprint)) %>%
                    ungroup() %>%
                    mutate(year = as.character(year))
                  avg <- plot_CHN %>%
                    group_by(amort, var) %>%
                    summarise(complete_footprint = sum(complete_footprint)/length(unique(plot_CHN$year))) %>%
                    ungroup() %>%
                    mutate(year = "average")
                  plot_CHN <- bind_rows(plot_CHN, avg)%>%
                    mutate(type=ifelse(year=="average","Highlighted","Normal"))%>%
                    mutate(complete_footprint = complete_footprint / 100)
                  # plot China
                  ggplot(plot_CHN, aes(x=as.factor(year), y=complete_footprint, fill= type)) +
                    geom_boxplot()+
                    theme(legend.position="none")+
                    xlab("years")+
                    scale_y_continuous("Footprint [km²]", limits = c(0, 3500))+
                    ggtitle("CHN") +
                    scale_fill_manual(values=c("#31688e", "#35b779"))
                  footprint_CHN <- readRDS("~/Deforestation-calculation/Paper/footprint_CHN.rds")
                  footprint_EU <- readRDS("~/Deforestation-calculation/Paper/footprint_EU.rds")
                  View(footprint_CHN)
                  footprint_CHN <- readRDS("~/Deforestation-calculation/Paper/footprint_CHN.rds")
                  footprint_EU <- readRDS("~/Deforestation-calculation/Paper/footprint_EU.rds")
                  footprint_CHN <- readRDS("~/Deforestation-calculation/Paper/footprint_CHN.rds")
                  View(footprint_CHN)
                  table_CHN <- footprint_CHN %>%
                    filter(iso == "dir")
                  View(table_CHN)
                  table_CHN <- footprint_CHN %>%
                    filter(luc == "dir")
                  View(table_CHN)
                  table_CHN <- footprint_CHN %>%
                    filter(luc == "dir")%>%
                    gather(key = "year", value = "complete_footprint")
                  table_CHN <- footprint_CHN %>%
                    filter(luc == "dir")%>%
                    gather()
                  table_CHN <- footprint_CHN %>%
                    filter(luc == "dir")#%>%
                  table_CHN <- footprint_CHN %>%
                    filter(luc == "dir")%>%
                    select(!iso & !nonfood & !food)
                  table_CHN <- footprint_CHN %>%
                    filter(luc == "dir")%>%
                    select(!iso & !nonfood & !food)
                  table_CHN <- footprint_CHN %>%
                    filter(luc == "dir")%>%
                    select(!iso & !nonfood)
                  table_CHN <- footprint_CHN %>%
                    filter(luc == "dir")%>%
                    select(!iso & !nonfood &!food)
                  table_CHN <- footprint_CHN %>%
                    filter(luc == "dir")%>%
                    select(!iso & !nonfood &!food)%>%
                    select(!iso)
                  table_CHN <- footprint_CHN %>%
                    filter(luc == "dir")%>%
                    select(!nonfood &!food)%>%
                    select(!iso)
                  table_CHN <- footprint_CHN %>%
                    filter(luc == "dir")%>%
                    select(!nonfood &!food)
                  table_CHN <- footprint_CHN %>%
                    filter(luc == "dir")%>%
                    select(!iso)
                  table_CHN <- footprint_CHN %>%
                    filter(luc == "dir")%>%
                    select(!iso)
                  table_CHN <- footprint_CHN %>%
                    select(!iso)
                  table_CHN <- footprint_CHN %>%
                    filter(luc == "dir")%>%
                    ungroup()%>%
                    select(!iso & !nonfood &!food)
                  table_CHN <- footprint_CHN %>%
                    filter(luc == "dir")%>%
                    ungroup()%>%
                    select(!iso & !nonfood &!food)%>%
                    gather()
                  table_CHN <- footprint_CHN %>%
                    filter(luc == "dir")%>%
                    ungroup()%>%
                    select(!iso & !nonfood &!food)#%>%
                  gather(key = "complete_footprint")
                  table_CHN <- footprint_CHN %>%
                    filter(luc == "dir")%>%
                    ungroup()%>%
                    select(!iso & !nonfood &!food)%>%
                    gather(key = "complete_footprint")
                  table_CHN <- footprint_CHN %>%
                    filter(luc == "dir")%>%
                    ungroup()%>%
                    select(!iso & !nonfood &!food)#%>%
                  table_CHN <- footprint_CHN %>%
                    filter(luc == "dir")%>%
                    ungroup()%>%
                    select(!iso & !nonfood &!food)%>%
                    mutate(amort_var = paste0("a",sprintf("%02d", amort),"_y",sprintf("%02d",var))) %>%
                    select(-amort,-var)
                  table_CHN <- footprint_CHN %>%
                    filter(luc == "dir")%>%
                    ungroup()%>%
                    select(!iso & !nonfood &!food)%>%
                    mutate(amort_var = paste0("a",sprintf("%02d", amort),"_y",sprintf("%02d",var))) %>%
                    select(-amort,-var, -luc)
                  table_CHN <- footprint_CHN %>%
                    filter(luc == "dir")%>%
                    ungroup()%>%
                    select(-iso, -nonfood, -food, -luc)%>%
                    mutate(amort_var = paste0("a",sprintf("%02d", amort),"_y",sprintf("%02d",var))) %>%
                    select(-amort,-var)
                  table_CHN <- footprint_CHN %>%
                    filter(luc == "dir")%>%
                    ungroup()%>%
                    select(-iso, -nonfood, -food, -luc)%>%
                    mutate(amort_var = paste0("a",sprintf("%02d", amort),"_y",sprintf("%02d",var))) %>%
                    select(-amort,-var)%>%
                    spread(amort_var, complete_footprint)
                  print.data.frame(table_CHN)
                  stargazer(table_CHN, type = 'text', out = 'out.txt')
                  xtable(table_CHN)
                  table_CHN_direct <- footprint_CHN %>%
                    filter(luc == "dir")%>%
                    ungroup()%>%
                    select(-iso, -nonfood, -food, -luc)%>%
                    mutate(amort_var = paste0("a",sprintf("%02d", amort),"_y",sprintf("%02d",var))) %>%
                    select(-amort,-var)%>%
                    spread(amort_var, complete_footprint)
                  table_CHN_indirect <- footprint_CHN %>%
                    filter(luc == "indir")%>%
                    ungroup()%>%
                    select(-iso, -nonfood, -food, -luc)%>%
                    mutate(amort_var = paste0("a",sprintf("%02d", amort),"_y",sprintf("%02d",var))) %>%
                    select(-amort,-var)%>%
                    spread(amort_var, complete_footprint)
                  table_EU_direct <- footprint_EU %>%
                    filter(luc == "dir")%>%
                    ungroup()%>%
                    select(-iso, -nonfood, -food, -luc)%>%
                    mutate(amort_var = paste0("a",sprintf("%02d", amort),"_y",sprintf("%02d",var))) %>%
                    select(-amort,-var)%>%
                    spread(amort_var, complete_footprint)
                  table_EU_direct <- footprint_EU %>%
                    filter(luc == "dir")%>%
                    ungroup()%>%
                    select(-iso, -nonfood, -food, -luc)%>%
                    mutate(amort_var = paste0("a",sprintf("%02d", amort),"_y",sprintf("%02d",var))) %>%
                    select(-amort,-var)%>%
                    spread(amort_var, complete_footprint)
                  table_EU_direct <- footprint_EU %>%
                    filter(luc == "dir")#%>%
                  table_EU_direct <- footprint_EU %>%
                    filter(luc == "dir")%>%
                    ungroup()#%>%
                  table_EU_direct <- footprint_EU %>%
                    filter(luc == "dir")%>%
                    ungroup()%>%
                    select(-iso, -nonfood, -food, -luc)#%>%
                  table_EU_direct <- footprint_EU %>%
                    filter(luc == "dir")%>%
                    ungroup()%>%
                    select(-iso, -nonfood, -food, -luc)%>%
                    mutate(amort_var = paste0("a",sprintf("%02d", amort),"_y",sprintf("%02d",var)))# %>%
                  table_EU_direct <- footprint_EU %>%
                    filter(luc == "dir")%>%
                    ungroup()%>%
                    select(-iso, -nonfood, -food, -luc)%>%
                    mutate(amort_var = paste0("a",sprintf("%02d", amort),"_y",sprintf("%02d",var))) %>%
                    select(-amort,-var)#%>%
                  table_EU_direct <- footprint_EU %>%
                    filter(luc == "dir")%>%
                    ungroup()%>%
                    select(-iso, -nonfood, -food, -luc)%>%
                    mutate(amort_var = paste0("a",sprintf("%02d", amort),"_y",sprintf("%02d",var))) %>%
                    select(-amort,-var)%>%
                    spread(amort_var, complete_footprint)
                  table_EU_direct <- footprint_EU %>%
                    filter(luc == "dir")%>%
                    ungroup()%>%
                    select(-iso, -nonfood, -food, -luc)%>%
                    mutate(amort_var = paste0("a",sprintf("%02d", amort),"_y",sprintf("%02d",var))) %>%
                    select(-amort,-var)#%>%
                  View(table_EU_direct)
                  table_EU_direct <- footprint_EU %>%
                    filter(luc == "dir")%>%
                    ungroup()%>%
                    select(-iso, -nonfood, -food, -luc)%>%
                    mutate(amort_var = paste0("a",sprintf("%02d", amort),"_y",sprintf("%02d",var))) %>%
                    select(-amort,-var)%>%
                    spread(amort_var, complete_footprint)
                  table_EU_direct <- footprint_EU %>%
                    filter(luc == "dir")%>%
                    ungroup()%>%
                    select(-iso, -nonfood, -food, -luc)%>%
                    mutate(amort_var = paste0("a",sprintf("%02d", amort),"_y",sprintf("%02d",var))) %>%
                    select(-amort,-var)%>%
                    spread(amort_var, complete_footprint)
                  table_CHN_indirect <- footprint_CHN %>%
                    filter(luc == "indir")%>%
                    ungroup()%>%
                    select(-iso, -nonfood, -food, -luc)%>%
                    mutate(amort_var = paste0("a",sprintf("%02d", amort),"_y",sprintf("%02d",var))) %>%
                    select(-amort,-var)%>%
                    spread(amort_var, complete_footprint)
                  table_CHN_direct <- footprint_CHN %>%
                    filter(luc == "dir")%>%
                    ungroup()%>%
                    select(-iso, -nonfood, -food, -luc)%>%
                    mutate(amort_var = paste0("a",sprintf("%02d", amort),"_y",sprintf("%02d",var))) %>%
                    select(-amort,-var)%>%
                    spread(amort_var, complete_footprint)
                  table_EU_direct <- footprint_EU %>%
                    filter(luc == "dir")%>%
                    ungroup()%>%
                    select(-iso, -nonfood, -food, -luc)%>%
                    mutate(amort_var = paste0("a",sprintf("%02d", amort),"_y",sprintf("%02d",var))) %>%
                    select(-amort,-var)%>%
                    spread(amort_var, complete_footprint)
                  table_EU_indirect <- footprint_EU %>%
                    filter(luc == "indir")%>%
                    ungroup()%>%
                    select(-iso, -nonfood, -food, -luc)%>%
                    mutate(amort_var = paste0("a",sprintf("%02d", amort),"_y",sprintf("%02d",var))) %>%
                    select(-amort,-var)%>%
                    spread(amort_var, complete_footprint)
                  table_EU_indirect <- footprint_EU %>%
                    filter(luc == "indir")%>%
                    ungroup()%>%
                    select(-iso, -nonfood, -food, -luc)%>%
                    mutate(amort_var = paste0("a",sprintf("%02d", amort),"_y",sprintf("%02d",var))) %>%
                    select(-amort,-var)%>%
                    ungroup()%>%
                    spread(amort_var, complete_footprint)
                  table_EU_indirect <- footprint_EU %>%
                    filter(luc == "indir")%>%
                    ungroup()%>%
                    select(-iso, -nonfood, -food, -luc)%>%
                    mutate(amort_var = paste0("a",sprintf("%02d", amort),"_y",sprintf("%02d",var))) %>%
                    select(-amort,-var)%>%
                    spread(amort_var, complete_footprint)
                  table_EU_indirect <- footprint_EU %>%
                    filter(luc == "indir")%>%
                    ungroup()%>%
                    select(-iso, -nonfood, -food, -luc)%>%
                    mutate(amort_var = paste0("a",sprintf("%02d", amort),"_y",sprintf("%02d",var))) %>%
                    select(-amort,-var)%>%
                    spread(amort_var, complete_footprint)
                  table_EU_direct <- footprint_EU %>%
                    filter(luc == "dir")%>%
                    ungroup()%>%
                    select(-iso, -nonfood, -food, -luc)%>%
                    mutate(amort_var = paste0("a",sprintf("%02d", amort),"_y",sprintf("%02d",var))) %>%
                    select(-amort,-var)%>%
                    spread(amort_var, complete_footprint)
                  table_EU_direct <- footprint_EU %>%
                    filter(luc == "dir")%>%
                    ungroup()%>%
                    select(-iso, -nonfood, -food, -luc)%>%
                    mutate(amort_var = paste0("a",sprintf("%02d", amort),"_y",sprintf("%02d",var))) %>%
                    select(-amort,-var)%>%
                    spread(complete_footprint, amort_var)
                  table_EU_direct <- footprint_EU %>%
                    filter(luc == "dir")%>%
                    ungroup()%>%
                    select(-iso, -nonfood, -food, -luc)%>%
                    mutate(amort_var = paste0("a",sprintf("%02d", amort),"_y",sprintf("%02d",var))) %>%
                    select(-amort,-var)%>%
                    spread(amort_var, complete_footprint)
                  footprint_EU <- readRDS("~/Deforestation-calculation/Paper/footprint_EU.rds")
                  table_EU_direct <- footprint_EU %>%
                    filter(luc == "dir")%>%
                    ungroup()%>%
                    select(-iso, -nonfood, -food, -luc)%>%
                    mutate(amort_var = paste0("a",sprintf("%02d", amort),"_y",sprintf("%02d",var))) %>%
                    select(-amort,-var)#%>%
                  table_EU_indirect <- footprint_EU %>%
                    filter(luc == "indir")%>%
                    ungroup()%>%
                    select(-iso, -nonfood, -food, -luc)%>%
                    mutate(amort_var = paste0("a",sprintf("%02d", amort),"_y",sprintf("%02d",var))) %>%
                    select(-amort,-var)#%>%
                  table_EU_indirect <- footprint_EU %>%
                    filter(luc == "indir")%>%
                    ungroup()%>%
                    select(-iso, -nonfood, -food, -luc)%>%
                    mutate(amort_var = paste0("a",sprintf("%02d", amort),"_y",sprintf("%02d",var))) %>%
                    select(-amort,-var)%>%
                    spread(amort_var, complete_footprint) # warum geht das nicht ?????
                  load("~/.RData")
                  
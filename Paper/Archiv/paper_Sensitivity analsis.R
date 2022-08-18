#load data
defor <- readRDS("~/Deforestation-calculation/input/defor.rds")
sens_table <- readRDS("~/Deforestation-calculation/Analysis_deforestation/sens_table.rds")

sens <- defor

##### yearly data with different amort. periods

y1a1 <- sens %>%
  select(y1,year,amort, luc)%>%
  filter(amort==1)%>%
  group_by(year)%>%
  summarise(value = sum(y1))
y1a1$label <- c("y1a1")    

y1a5 <- sens %>%
  select(y1,year,amort, luc)%>%
  filter(amort==5)%>%
  group_by(year)%>%
  summarise(value = sum(y1)) 
y1a5$label <- c("y1a5") 

y1a10 <- sens %>%
  select(y1,year,amort, luc)%>%
  filter(amort==10)%>%
  group_by(year)%>%
  summarise(value = sum(y1))
y1a10$label <- c("y1a10") 

y1 <- rbind(y1a1,y1a5)
y1 <- rbind(y1,y1a10)



rm(y1a1, y1a10, y1a5)

##### 5-year period data with different amort. periods

y5a1 <- sens %>%
  select(y5,year,amort, luc)%>%
  filter(amort==1)%>%
  group_by(year)%>%
  summarise(value = sum(y5))
y5a1$label <- c("y5a1") 

y5a5 <- sens %>%
  select(y5,year,amort, luc)%>%
  filter(amort==5)%>%
  group_by(year)%>%
  summarise(value = sum(y5)) 
y5a5$label <- c("y5a5") 

y5a10 <- sens %>%
  select(y5,year,amort, luc)%>%
  filter(amort==10)%>%
  group_by(year)%>%
  summarise(value = sum(y5))
y5a10$label <- c("y5a10") 

y5 <- rbind(y5a1, y5a5)
y5 <- rbind(y5, y5a10)

rm(y5a1, y5a10, y5a5)

##### 10- year periods data with different amort. periods

y10a1 <- sens %>%
  select(y10,year,amort, luc)%>%
  filter(amort==1)%>%
  group_by(year)%>%
  summarise(value = sum(y10))
y10a1$label <- c("y10a1") 

y10a5 <- sens %>%
  select(y10,year,amort, luc)%>%
  filter(amort==5)%>%
  group_by(year)%>%
  summarise(value = sum(y10)) 
y10a5$label <- c("y10a5") 

y10a10 <- sens %>%
  select(y10,year,amort, luc)%>%
  filter(amort==10)%>%
  group_by(year)%>%
  summarise(value = sum(y10))
y10a10$label <- c("y10a10") 

y10 <- rbind(y10a1, y10a5)
y10 <- rbind(y10, y10a10)

rm(y10a1, y10a10, y10a5)

# merge to one dataframe



defor_sens <- rbind(y1,y5)
defor_sens <- rbind(defor_sens,y10)



##### sensitivity graphs yy10a5 as baseline

green_col <- c("#d9f0a3","#c7e9c0", "#fc4e2a", "#99d8c9", "#66c2a4", "#41ae76", "#02818a", "#238b45", "#00441b")

green_col1 <- c("#fc4e2a", "#c7e9c0", "#41ae76", "#02818a")

##
defor_sens <- defor_sens %>%
  mutate(value = value*0.01)

# thesis figure 24
ggplot()+
  geom_line(data= filter(defor_sens),aes(x= year, y= value, colour= label),size= 1)+
  scale_colour_manual(values = green_col)+
  theme_light()+
  labs(x= "year", y = "deforestation [km²] ")+
  theme(legend.title=element_blank())


# thesis figure 25
ggplot()+
  geom_line(data= filter(defor_sens, label %in% c("y1a1", "y1a5", "y1a10", "y10a5")),aes(x= year, y= value, colour= label),size= 1)+
  scale_colour_manual(values = green_col1)+
  theme_light()+
  labs(x= "year", y = "deforestation [km²] ")+
  theme(legend.title=element_blank())

# thesis figure 26
ggplot()+
  geom_line(data= filter(defor_sens, label %in% c("y5a1", "y5a5", "y5a10", "y10a5")),aes(x= year, y= value, colour= label),size= 1)+
  scale_colour_manual(values = green_col1)+
  theme_light()+
  labs(x= "year", y = "deforestation [km²] ")+
  theme(legend.title=element_blank())

# thesis figure 27
green_col2 <- c("#c7e9c0", "#41ae76", "#fc4e2a")
ggplot()+
  geom_line(data= filter(defor_sens, label %in% c("y10a5", "y10a1", "y10a10")),aes(x= year, y= value, colour= label),size= 1)+
  scale_colour_manual(values = green_col2)+
  theme_light()+
  labs(x= "year", y = "deforestation [km²] ")+
  theme(legend.title=element_blank())


##### table

fun_test <- function(x, na.rm = FALSE) (x*0.01)      
test <- fun_test(sens_table[ , c(2: 10)]  )
test$year <- sens_table$year
sens_table <- test

            
sens_table_per <-  sens_table %>% 
  add_column(y1a1_percent = sens_table$y1a1/sens_table$y10a5, .after = "y1a1")

sens_table_per <-  sens_table_per %>% 
  add_column(y1a5_percent = sens_table$y1a5/sens_table$y10a5, .after = "y1a5")

sens_table_per <-  sens_table_per %>% 
  add_column(y1a10_percent = sens_table$y1a10/sens_table$y10a5, .after = "y1a10")


sens_table_per <-  sens_table_per %>% 
  add_column(y5a1_percent = sens_table$y5a1/sens_table$y10a5, .after = "y5a1")

sens_table_per <-  sens_table_per %>% 
  add_column(y5a5_percent = sens_table$y5a5/sens_table$y10a5, .after = "y5a5")

sens_table_per <-  sens_table_per %>% 
  add_column(y5a10_percent = sens_table$y5a10/sens_table$y10a5, .after = "y5a10")



sens_table_per <-  sens_table_per %>% 
  add_column(y10a1_percent = sens_table$y10a1/sens_table$y10a5, .after = "y10a1")

sens_table_per <-  sens_table_per %>% 
  add_column(y10a5_percent = sens_table$y10a5/sens_table$y10a5, .after = "y10a5")

sens_table_per <-  sens_table_per %>% 
  add_column(y10a10_percent = sens_table$y10a10/sens_table$y10a5, .after = "y10a10")



sens_table_mean <- sens_table_per %>%
  select(-y1a1, -y1a5, -y1a10, -y5a1, -y5a5, -y5a10, -y10a1, -y10a5, -y10a10)
x <- colMeans(select(sens_table_mean, -year))
sens_table_colmean <- as.data.frame(x)



sens_table_dev <- data.frame(matrix(nrow = 9, ncol = 1))
for (i in 1:9) {
  sens_table_dev[i,1] <- round(((-1+sens_table_colmean[i,1])*100),2)
}
sens_table_deviation <- cbind(sens_table_colmean, sens_table_dev)


labels <- c("y1a1", "y1a5", "y1a10", "y5a1", "y5a5", "y5a10", "y10a1", "y10a5", "y10a10")
sens_table_deviation <- add_column(sens_table_deviation, labels, .before = "x")

colnames(sens_table_deviation) <- c("labels","x", "mean deviation [%] to base mean (y10a5)")

### print tables
sens_table_deviation %>%
  mutate_if(is.numeric,
            round,
            digits = 2)%>%
  select(-"x")%>%
  gt(rowname_col = "labels")%>%
  tab_options(column_labels.font.size = "small")

sens_table %>%
  mutate_if(is.numeric,
            round,
            digits = 2)%>%
  gt(rowname_col = "year")%>%
  tab_header("Deforestation [km²]")
#row.names(sens_table_deviation) <- test



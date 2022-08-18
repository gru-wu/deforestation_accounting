1659613134126:#plot EU
  1659613134161:ggplot(plot_EU, aes(x=as.factor(year), y=complete_footprint, fill= type)) +
  1659613134186:geom_boxplot()+
  1659613134215:theme(legend.position="none")+
  1659613134240:xlab("years")+
  1659613134269:ylab("Footprint [km²]")+
  1659613134292:scale_fill_manual(values=c("#EE6A50", "#79CDCD"))
1659613151416:plot_EU <- bind_rows(plot_EU, avg)%>%
  1659613151445:mutate(type=ifelse(year=="average","Highlighted","Normal"))%>%
  1659613151470:mutate(complete_footprint = complete_footprint / 100)
1659613152187:rm(avg)
1659613152497:#plot EU
  1659613152518:ggplot(plot_EU, aes(x=as.factor(year), y=complete_footprint, fill= type)) +
  1659613152543:geom_boxplot()+
  1659613152567:theme(legend.position="none")+
  1659613152590:xlab("years")+
  1659613152620:ylab("Footprint [km²]")+
  1659613152647:scale_fill_manual(values=c("#EE6A50", "#79CDCD"))
1659613352570:#plot EU
  1659613352592:ggplot(plot_EU, aes(x=as.factor(year), y=complete_footprint, fill= type)) +
  1659613352613:geom_boxplot()+
  1659613352633:theme(legend.position="none")+
  1659613352652:xlab("years")+
  1659613352673:ylab("Footprint [km²]")+
  1659613352692:ggtitle(EU) +
  1659613352712:scale_fill_manual(values=c("#EE6A50", "#79CDCD"))
1659613356014:#plot EU
  1659613356038:ggplot(plot_EU, aes(x=as.factor(year), y=complete_footprint, fill= type)) +
  1659613356060:geom_boxplot()+
  1659613356084:theme(legend.position="none")+
  1659613356107:xlab("years")+
  1659613356130:ylab("Footprint [km²]")+
  1659613356151:ggtitle(EU) +
  1659613356170:scale_fill_manual(values=c("#EE6A50", "#79CDCD"))
1659613360550:#plot EU
  1659613360574:ggplot(plot_EU, aes(x=as.factor(year), y=complete_footprint, fill= type)) +
  1659613360600:geom_boxplot()+
  1659613360620:theme(legend.position="none")+
  1659613360643:xlab("years")+
  1659613360666:ylab("Footprint [km²]")+
  1659613360689:ggtitle(EU) +
  1659613360712:scale_fill_manual(values=c("#EE6A50", "#79CDCD"))
1659613367176:#plot EU
  1659613367199:ggplot(plot_EU, aes(x=as.factor(year), y=complete_footprint, fill= type)) +
  1659613367229:geom_boxplot()+
  1659613367254:theme(legend.position="none")+
  1659613367280:xlab("years")+
  1659613367306:ylab("Footprint [km²]")+
  1659613367332:ggtitle("EU") +
  1659613367357:scale_fill_manual(values=c("#EE6A50", "#79CDCD"))
1659613380607:# plot China
  1659613380628:ggplot(plot_CHN, aes(x=as.factor(year), y=complete_footprint, fill= type)) +
  1659613380644:geom_boxplot()+
  1659613380661:theme(legend.position="none")+
  1659613380678:xlab("years")+
  1659613380698:ylab("Footprint [km²]")+
  1659613380721:ggtitle("EU") +
  1659613380742:scale_fill_manual(values=c("#EE6A50", "#79CDCD"))
1659613390059:# load data
  1659613390099:footprint_CHN <- readRDS("~/Deforestation-calculation/Paper/footprint_CHN.rds")
1659613390133:footprint_EU <- readRDS("~/Deforestation-calculation/Paper/footprint_EU.rds")
1659613390192:# aggregate EU countries to EU total
  1659613390214:plot_EU <- footprint_EU %>%
  1659613390235:group_by(year, amort, var) %>%
  1659613390257:summarise(complete_footprint = sum(complete_footprint)) %>%
  1659613390279:ungroup() %>%
  1659613390298:mutate(year = as.character(year))
1659613390358:avg <- plot_EU %>%
  1659613390380:group_by(amort, var) %>%
  1659613390403:summarise(complete_footprint = sum(complete_footprint)/length(unique(plot_EU$year))) %>%
  1659613390423:ungroup() %>%
  1659613390445:mutate(year = "average")
1659613390502:plot_EU <- bind_rows(plot_EU, avg)%>%
  1659613390524:mutate(type=ifelse(year=="average","Highlighted","Normal"))%>%
  1659613390550:mutate(complete_footprint = complete_footprint / 100)
1659613390616:rm(avg)
1659613390674:#plot EU
  1659613390698:ggplot(plot_EU, aes(x=as.factor(year), y=complete_footprint, fill= type)) +
  1659613390722:geom_boxplot()+
  1659613390754:theme(legend.position="none")+
  1659613390775:xlab("years")+
  1659613390798:ylab("Footprint [km²]")+
  1659613390826:ggtitle("EU") +
  1659613390850:scale_fill_manual(values=c("#EE6A50", "#79CDCD"))
1659613391562:#prepare plot data for CHINA
  1659613391585:plot_CHN <-  footprint_CHN %>%
  1659613391608:group_by(year, amort, var) %>%
  1659613391630:summarise(complete_footprint = sum(complete_footprint)) %>%
  1659613391654:ungroup() %>%
  1659613391678:mutate(year = as.character(year))
1659613392485:avg <- plot_CHN %>%
  1659613392508:group_by(amort, var) %>%
  1659613392532:summarise(complete_footprint = sum(complete_footprint)/length(unique(plot_CHN$year))) %>%
  1659613392556:ungroup() %>%
  1659613392581:mutate(year = "average")
1659613392651:plot_CHN <- bind_rows(plot_CHN, avg)%>%
  1659613392680:mutate(type=ifelse(year=="average","Highlighted","Normal"))%>%
  1659613392705:mutate(complete_footprint = complete_footprint / 100)
1659613392761:# plot China
  1659613392791:ggplot(plot_CHN, aes(x=as.factor(year), y=complete_footprint, fill= type)) +
  1659613392816:geom_boxplot()+
  1659613392844:theme(legend.position="none")+
  1659613392868:xlab("years")+
  1659613392892:ylab("Footprint [km²]")+
  1659613392916:ggtitle("CHN") +
  1659613392939:scale_fill_manual(values=c("#EE6A50", "#79CDCD"))
1659613417784:#load data
  1659613417808:defor <- readRDS("~/Deforestation-calculation/input/defor.rds")
1659613417875:facet_data <- defor %>%
  1659613417898:gather(key= y, value = "value",1:3)%>%
  1659613417920:mutate(value = value / 100)
1659613417976:facet_data$y <- substring(facet_data$y, 2)
1659613418016:# combine amort & intervall into one variable
  1659613418041:a <- "a"
1659613418070:b <- "_y"
1659613418099:facet_data$amort_var <- paste0(a, sprintf("%02d",as.numeric(facet_data$amort)),
                                             1659613418121:b, sprintf("%02d",as.numeric(facet_data$y)))
1659613418205:rm(a,b)
1659613418243:ggplot(facet_data, aes(x = year, y = value, fill = factor(luc, levels=c("indir", "dir")))) +
  1659613418266:geom_area()+
  1659613418288:facet_wrap(vars(amort_var))+
  1659613418312:ylab("deforestation [km²]")+
  1659613418334:labs(fill = "LUC")+
  1659613418355:scale_fill_manual(values = c("#5F9EA0","#EE6A50"))
1659613420309:rm(list= ls())
1659613455598:#load data
  1659613455626:defor <- readRDS("~/Deforestation-calculation/input/defor.rds")
1659613455672:# summarized direct+indirect
  1659613455693:sens_table <- readRDS("~/Deforestation-calculation/Analysis_deforestation/sens_table.rds")
1659613455750:fun <- function(x){
  1659613455771:round(((x/sens_table[,6])-1) * 100, 0)
  1659613455792:}
1659613455826:plot_data <- data.frame(apply(sens_table,2, fun))
1659613455913:colnames(plot_data) <- colnames(sens_table)
1659613455934:plot_data$year <- sens_table$year
1659613455968:plot_data <- plot_data %>%
  1659613455989:gather("key","value",-year) %>%
  1659613456009:mutate(amort = paste0("a", sprintf("%02d", as.numeric(substr(key, regexpr("a", key)+1,10))))) %>%
  1659613456031:mutate(intervall = paste0("y", sprintf("%02d", as.numeric(substr(key, regexpr("y", key)+1,regexpr("a", key)-1))))) %>%
  1659613456053:select(-key)
1659613456173:ggplot(plot_data, aes(x= year, y= value, colour = amort))+
  1659613456192:geom_line(aes(linetype=intervall, color = amort))+
  1659613456209:ylab("deviation [%]")+
  1659613456228:scale_colour_viridis_d("amortization", direction = -1)+
  1659613456248:scale_linetype_manual(values=c("twodash", "dotted", "solid"))
1659613456973:#labs(colour = "system definitions")
  1659613456996:#scale_colour_manual(values = c("#8B7355", "#CDAA7D","#FFD39B", "#528B8B", "#97FFFF", "#00CDCD", "#458B00", "#A2CD5A", "#CAFF70") )
  1659613457036:rm(list = ls())
1659613482066:load data
1659613482113:footprint_CHN <- readRDS("~/Deforestation-calculation/Paper/footprint_CHN.rds")
1659613482146:footprint_EU <- readRDS("~/Deforestation-calculation/Paper/footprint_EU.rds")
1659613482212:# aggregate EU countries to EU total
  1659613482239:plot_EU <- footprint_EU %>%
  1659613482264:group_by(year, amort, var) %>%
  1659613482288:summarise(complete_footprint = sum(complete_footprint)) %>%
  1659613482313:ungroup() %>%
  1659613482335:mutate(year = as.character(year))
1659613482402:avg <- plot_EU %>%
  1659613482423:group_by(amort, var) %>%
  1659613482441:summarise(complete_footprint = sum(complete_footprint)/length(unique(plot_EU$year))) %>%
  1659613482461:ungroup() %>%
  1659613482482:mutate(year = "average")
1659613482549:plot_EU <- bind_rows(plot_EU, avg)%>%
  1659613482579:mutate(type=ifelse(year=="average","Highlighted","Normal"))%>%
  1659613482607:mutate(complete_footprint = complete_footprint / 100)
1659613482659:rm(avg)
1659613482713:#plot EU
  1659613482733:ggplot(plot_EU, aes(x=as.factor(year), y=complete_footprint, fill= type)) +
  1659613482757:geom_boxplot()+
  1659613482779:theme(legend.position="none")+
  1659613482802:xlab("years")+
  1659613482822:ylab("Footprint [km²]")+
  1659613482844:ggtitle("EU") +
  1659613482877:scale_fill_manual(values=c("#EE6A50", "#79CDCD"))
1659613501977:#prepare plot data for CHINA
  1659613502005:plot_CHN <-  footprint_CHN %>%
  1659613502032:group_by(year, amort, var) %>%
  1659613502059:summarise(complete_footprint = sum(complete_footprint)) %>%
  1659613502087:ungroup() %>%
  1659613502112:mutate(year = as.character(year))
1659613502175:avg <- plot_CHN %>%
  1659613502194:group_by(amort, var) %>%
  1659613502213:summarise(complete_footprint = sum(complete_footprint)/length(unique(plot_CHN$year))) %>%
  1659613502232:ungroup() %>%
  1659613502251:mutate(year = "average")
1659613502314:plot_CHN <- bind_rows(plot_CHN, avg)%>%
  1659613502343:mutate(type=ifelse(year=="average","Highlighted","Normal"))%>%
  1659613502370:mutate(complete_footprint = complete_footprint / 100)
1659613502427:# plot China
  1659613502453:ggplot(plot_CHN, aes(x=as.factor(year), y=complete_footprint, fill= type)) +
  1659613502479:geom_boxplot()+
  1659613502509:theme(legend.position="none")+
  1659613502537:xlab("years")+
  1659613502561:ylab("Footprint [km²]")+
  1659613502586:ggtitle("CHN") +
  1659613502613:scale_fill_manual(values=c("#EE6A50", "#79CDCD"))
1659613503389:rm(list = ls())
1659613620995:#plot EU
  1659613621020:ggplot(plot_EU, aes(x=as.factor(year), y=complete_footprint, fill= type)) +
  1659613621044:geom_boxplot()+
  1659613621066:theme(legend.position="none")+
  1659613621086:xlab("years")+
  1659613621110:ylab("Footprint [km²]")+
  1659613621134:ggtitle("EU") +
  1659613621158:scale_fill_manual(values=c("#EE6A50", "#79CDCD"))
1659613626177:# load data
  1659613626223:footprint_CHN <- readRDS("~/Deforestation-calculation/Paper/footprint_CHN.rds")
1659613626249:footprint_EU <- readRDS("~/Deforestation-calculation/Paper/footprint_EU.rds")
1659613626314:# aggregate EU countries to EU total
  1659613626343:plot_EU <- footprint_EU %>%
  1659613626369:group_by(year, amort, var) %>%
  1659613626398:summarise(complete_footprint = sum(complete_footprint)) %>%
  1659613626422:ungroup() %>%
  1659613626448:mutate(year = as.character(year))
1659613626521:avg <- plot_EU %>%
  1659613626548:group_by(amort, var) %>%
  1659613626579:summarise(complete_footprint = sum(complete_footprint)/length(unique(plot_EU$year))) %>%
  1659613626604:ungroup() %>%
  1659613626627:mutate(year = "average")
1659613626689:plot_EU <- bind_rows(plot_EU, avg)%>%
  1659613626712:mutate(type=ifelse(year=="average","Highlighted","Normal"))%>%
  1659613626735:mutate(complete_footprint = complete_footprint / 100)
1659613626790:rm(avg)
1659613626839:#plot EU
  1659613626857:ggplot(plot_EU, aes(x=as.factor(year), y=complete_footprint, fill= type)) +
  1659613626877:geom_boxplot()+
  1659613626902:theme(legend.position="none")+
  1659613626925:xlab("years")+
  1659613626950:ylab("Footprint [km²]")+
  1659613626973:ggtitle("EU") +
  1659613626997:scale_fill_manual(values=c("#EE6A50", "#79CDCD"))
1659613680441:#load data
  1659613680471:defor <- readRDS("~/Deforestation-calculation/input/defor.rds")
1659613680523:facet_data <- defor %>%
  1659613680548:gather(key= y, value = "value",1:3)%>%
  1659613680576:mutate(value = value / 100)
1659613680621:facet_data$y <- substring(facet_data$y, 2)
1659613680664:# combine amort & intervall into one variable
  1659613680688:a <- "a"
1659613680716:b <- "_y"
1659613680742:facet_data$amort_var <- paste0(a, sprintf("%02d",as.numeric(facet_data$amort)),
                                             1659613680765:b, sprintf("%02d",as.numeric(facet_data$y)))
1659613680790:rm(a,b)
1659613680833:ggplot(facet_data, aes(x = year, y = value, fill = factor(luc, levels=c("indir", "dir")))) +
  1659613680861:geom_area()+
  1659613680885:facet_wrap(vars(amort_var))+
  1659613680916:ylab("deforestation [km²]")+
  1659613680944:labs(fill = "LUC")+
  1659613680968:scale_fill_manual(values = c("#5F9EA0","#EE6A50"))
1659613682921:rm(list= ls())
1659613739898:#load data
  1659613739928:defor <- readRDS("~/Deforestation-calculation/input/defor.rds")
1659613739978:# summarized direct+indirect
  1659613740004:sens_table <- readRDS("~/Deforestation-calculation/Analysis_deforestation/sens_table.rds")
1659613740070:fun <- function(x){
  1659613740089:round(((x/sens_table[,6])-1) * 100, 0)
  1659613740113:}
1659613740160:plot_data <- data.frame(apply(sens_table,2, fun))
1659613740230:colnames(plot_data) <- colnames(sens_table)
1659613740257:plot_data$year <- sens_table$year
1659613740300:plot_data <- plot_data %>%
  1659613740327:gather("key","value",-year) %>%
  1659613740356:mutate(amort = paste0("a", sprintf("%02d", as.numeric(substr(key, regexpr("a", key)+1,10))))) %>%
  1659613740385:mutate(intervall = paste0("y", sprintf("%02d", as.numeric(substr(key, regexpr("y", key)+1,regexpr("a", key)-1))))) %>%
  1659613740415:select(-key)
1659613740488:ggplot(plot_data, aes(x= year, y= value, colour = amort))+
  1659613740513:geom_line(aes(linetype=intervall, color = amort))+
  1659613740534:ylab("deviation [%]")+
  1659613740558:scale_colour_viridis_d("amortization", direction = -1)+
  1659613740582:scale_linetype_manual(values=c("twodash", "dotted", "solid"))
1659613741271:#labs(colour = "system definitions")
  1659613741301:#scale_colour_manual(values = c("#8B7355", "#CDAA7D","#FFD39B", "#528B8B", "#97FFFF", "#00CDCD", "#458B00", "#A2CD5A", "#CAFF70") )
  1659613741349:rm(list = ls())
1659613918720:ggplot(plot_data, aes(x= year, y= value, colour = amort))+
  1659613918745:geom_line(aes(linetype=intervall, color = amort))+
  1659613918767:ylab("deviation [%]")+
  1659613918787:scale_color_viridis(discrete=TRUE, option="plasma")+
  1659613918809:#scale_colour_viridis_d("amortization", direction = -1)+
  1659613918834:scale_linetype_manual(values=c("twodash", "dotted", "solid"))
1659613927380:#load data
  1659613927403:defor <- readRDS("~/Deforestation-calculation/input/defor.rds")
1659613927457:# summarized direct+indirect
  1659613927481:sens_table <- readRDS("~/Deforestation-calculation/Analysis_deforestation/sens_table.rds")
1659613927545:fun <- function(x){
  1659613927568:round(((x/sens_table[,6])-1) * 100, 0)
  1659613927595:}
1659613927638:plot_data <- data.frame(apply(sens_table,2, fun))
1659613927700:colnames(plot_data) <- colnames(sens_table)
1659613927727:plot_data$year <- sens_table$year
1659613927759:plot_data <- plot_data %>%
  1659613927782:gather("key","value",-year) %>%
  1659613927806:mutate(amort = paste0("a", sprintf("%02d", as.numeric(substr(key, regexpr("a", key)+1,10))))) %>%
  1659613927831:mutate(intervall = paste0("y", sprintf("%02d", as.numeric(substr(key, regexpr("y", key)+1,regexpr("a", key)-1))))) %>%
  1659613927858:select(-key)
1659613927928:ggplot(plot_data, aes(x= year, y= value, colour = amort))+
  1659613927951:geom_line(aes(linetype=intervall, color = amort))+
  1659613927977:ylab("deviation [%]")+
  1659613927999:scale_color_viridis(discrete=TRUE, option="plasma")+
  1659613928020:#scale_colour_viridis_d("amortization", direction = -1)+
  1659613928040:scale_linetype_manual(values=c("twodash", "dotted", "solid"))
1659613928135:#l
  1659613940531:ggplot(plot_data, aes(x= year, y= value, colour = amort))+
  1659613940557:geom_line(aes(linetype=intervall, color = amort))+
  1659613940581:ylab("deviation [%]")+
  1659613940603:scale_color_viridis_d(discrete=TRUE, option="plasma")+
  1659613940626:#scale_colour_viridis_d("amortization", direction = -1)+
  1659613940649:scale_linetype_manual(values=c("twodash", "dotted", "solid"))
1659613948097:ggplot(plot_data, aes(x= year, y= value, colour = amort))+
  1659613948124:geom_line(aes(linetype=intervall, color = amort))+
  1659613948150:ylab("deviation [%]")+
  1659613948175:scale_color_viridis_d(option="plasma")+
  1659613948199:#scale_colour_viridis_d("amortization", direction = -1)+
  1659613948223:scale_linetype_manual(values=c("twodash", "dotted", "solid"))
1659614012096:ggplot(plot_data, aes(x= year, y= value, colour = amort))+
  1659614012122:geom_line(aes(linetype=intervall, color = amort))+
  1659614012153:ylab("deviation [%]")+
  1659614012173:scale_color_brewer(palette = "Paired")+
  1659614012194:#scale_colour_viridis_d("amortization", direction = -1)+
  1659614012215:scale_linetype_manual(values=c("twodash", "dotted", "solid"))
1659614048415:ggplot(plot_data, aes(x= year, y= value, colour = amort))+
  1659614048440:geom_line(aes(linetype=intervall, color = amort))+
  1659614048462:ylab("deviation [%]")+
  1659614048482:scale_color_hue(h = c(180, 300))
1659614082954:ggplot(plot_data, aes(x= year, y= value, colour = amort))+
  1659614082983:geom_line(aes(linetype=intervall, color = amort))+
  1659614083014:ylab("deviation [%]")+
  1659614083049:scale_color_brewer(palette = "Spectral")+
  1659614083071:#scale_colour_viridis_d("amortization", direction = -1)+
  1659614083094:scale_linetype_manual(values=c("twodash", "dotted", "solid"))
1659614107235:ggplot(plot_data, aes(x= year, y= value, colour = amort))+
  1659614107262:geom_line(aes(linetype=intervall, color = amort))+
  1659614107283:ylab("deviation [%]")+
  1659614107305:scale_color_viridis(discrete=TRUE, option="viridis")+
  1659614107327:#scale_colour_viridis_d("amortization", direction = -1)+
  1659614107347:scale_linetype_manual(values=c("twodash", "dotted", "solid"))
1659614121979:ggplot(plot_data, aes(x= year, y= value, colour = amort))+
  1659614122008:geom_line(aes(linetype=intervall, color = amort))+
  1659614122034:ylab("deviation [%]")+
  1659614122062:scale_color_viridis_c(discrete=TRUE, option="viridis")+
  1659614122086:#scale_colour_viridis_d("amortization", direction = -1)+
  1659614122112:scale_linetype_manual(values=c("twodash", "dotted", "solid"))
1659614129781:ggplot(plot_data, aes(x= year, y= value, colour = amort))+
  1659614129803:geom_line(aes(linetype=intervall, color = amort))+
  1659614129825:ylab("deviation [%]")+
  1659614129850:scale_color_viridis_c( option="viridis")+
  1659614129871:#scale_colour_viridis_d("amortization", direction = -1)+
  1659614129892:scale_linetype_manual(values=c("twodash", "dotted", "solid"))
1659614136096:ggplot(plot_data, aes(x= year, y= value, colour = amort))+
  1659614136119:geom_line(aes(linetype=intervall, color = amort))+
  1659614136141:ylab("deviation [%]")+
  1659614136167:scale_color_viridis_d( option="viridis")+
  1659614136196:#scale_colour_viridis_d("amortization", direction = -1)+
  1659614136222:scale_linetype_manual(values=c("twodash", "dotted", "solid"))
1659614147937:ggplot(plot_data, aes(x= year, y= value, colour = amort))+
  1659614147960:geom_line(aes(linetype=intervall, color = amort))+
  1659614147982:ylab("deviation [%]")+
  1659614148006:scale_colour_viridis_d("amortization", direction = -1)+
  1659614148029:scale_linetype_manual(values=c("twodash", "dotted", "solid"))
1659614261737:#labs(colour = "system definitions")
  1659614261762:scale_colour_manual(values = c("#8B7355", "#CDAA7D","#FFD39B", "#528B8B", "#97FFFF", "#00CDCD", "#458B00", "#A2CD5A", "#CAFF70") )
1659614268157:ggplot(plot_data, aes(x= year, y= value, colour = amort))+
  1659614268182:geom_line(aes(linetype=intervall, color = amort))+
  1659614268202:ylab("deviation [%]")+
  1659614268221:#scale_colour_viridis_d("amortization", direction = -1)+
  1659614268242:scale_linetype_manual(values=c("twodash", "dotted", "solid"))+
  1659614268263:#labs(colour = "system definitions")
  1659614268284:scale_colour_manual(values = c("#8B7355", "#CDAA7D","#FFD39B", "#528B8B", "#97FFFF", "#00CDCD", "#458B00", "#A2CD5A", "#CAFF70") )
1659614291490:ggplot(plot_data, aes(x= year, y= value, colour = amort))+
  1659614291516:geom_line(aes(linetype=intervall, color = amort))+
  1659614291542:ylab("deviation [%]")+
  1659614291567:#scale_colour_viridis_d("amortization", direction = -1)+
  1659614291592:scale_linetype_manual(values=c("twodash", "dotted", "solid"))+
  1659614291615:#labs(colour = "system definitions")
  1659614291644:scale_colour_manual(values = c("#fde725", "#CDAA7D","#FFD39B", "#528B8B", "#97FFFF", "#00CDCD", "#458B00", "#A2CD5A", "#CAFF70") )
1659614313547:ggplot(plot_data, aes(x= year, y= value, colour = amort))+
  1659614313572:geom_line(aes(linetype=intervall, color = amort))+
  1659614313599:ylab("deviation [%]")+
  1659614313628:#scale_colour_viridis_d("amortization", direction = -1)+
  1659614313654:scale_linetype_manual(values=c("twodash", "dotted", "solid"))+
  1659614313683:#labs(colour = "system definitions")
  1659614313710:scale_colour_manual(values = c("#fde725", "#CDAA7D","#FFD39B") )
1659614327052:ggplot(plot_data, aes(x= year, y= value, colour = amort))+
  1659614327079:geom_line(aes(linetype=intervall, color = amort))+
  1659614327104:ylab("deviation [%]")+
  1659614327127:#scale_colour_viridis_d("amortization", direction = -1)+
  1659614327150:scale_linetype_manual(values=c("twodash", "dotted", "solid"))+
  1659614327175:#labs(colour = "system definitions")
  1659614327197:scale_colour_manual(values = c("#5ec962", "#CDAA7D","#FFD39B") )
1659614339950:ggplot(plot_data, aes(x= year, y= value, colour = amort))+
  1659614339975:geom_line(aes(linetype=intervall, color = amort))+
  1659614340001:ylab("deviation [%]")+
  1659614340023:#scale_colour_viridis_d("amortization", direction = -1)+
  1659614340046:scale_linetype_manual(values=c("twodash", "dotted", "solid"))+
  1659614340067:#labs(colour = "system definitions")
  1659614340091:scale_colour_manual(values = c("#5ec962", "#3b528b","#FFD39B") )
1659614359338:ggplot(plot_data, aes(x= year, y= value, colour = amort))+
  1659614359362:geom_line(aes(linetype=intervall, color = amort))+
  1659614359385:ylab("deviation [%]")+
  1659614359408:#scale_colour_viridis_d("amortization", direction = -1)+
  1659614359430:scale_linetype_manual(values=c("twodash", "dotted", "solid"))+
  1659614359452:#labs(colour = "system definitions")
  1659614359477:scale_colour_manual(values = c("#5ec962", "#3b528b","#440154") )
1659614430353:load data
1659614430389:footprint_CHN <- readRDS("~/Deforestation-calculation/Paper/footprint_CHN.rds")
1659614430414:footprint_EU <- readRDS("~/Deforestation-calculation/Paper/footprint_EU.rds")
1659614430460:# aggregate EU countries to EU total
  1659614430480:plot_EU <- footprint_EU %>%
  1659614430498:group_by(year, amort, var) %>%
  1659614430517:summarise(complete_footprint = sum(complete_footprint)) %>%
  1659614430539:ungroup() %>%
  1659614430563:mutate(year = as.character(year))
1659614430649:avg <- plot_EU %>%
  1659614430671:group_by(amort, var) %>%
  1659614430694:summarise(complete_footprint = sum(complete_footprint)/length(unique(plot_EU$year))) %>%
  1659614430717:ungroup() %>%
  1659614430736:mutate(year = "average")
1659614430798:plot_EU <- bind_rows(plot_EU, avg)%>%
  1659614430821:mutate(type=ifelse(year=="average","Highlighted","Normal"))%>%
  1659614430841:mutate(complete_footprint = complete_footprint / 100)
1659614430890:rm(avg)
1659614430937:#plot EU
  1659614430955:ggplot(plot_EU, aes(x=as.factor(year), y=complete_footprint, fill= type)) +
  1659614430973:geom_boxplot()+
  1659614430992:theme(legend.position="none")+
  1659614431012:xlab("years")+
  1659614431032:ylab("Footprint [km²]")+
  1659614431053:ggtitle("EU") +
  1659614431075:scale_fill_manual(values=c("#440154", "#5ec962"))
1659614464016:#plot EU
  1659614464039:ggplot(plot_EU, aes(x=as.factor(year), y=complete_footprint, fill= type)) +
  1659614464070:geom_boxplot()+
  1659614464100:theme(legend.position="none")+
  1659614464126:xlab("years")+
  1659614464152:ylab("Footprint [km²]")+
  1659614464177:ggtitle("EU") +
  1659614464201:scale_fill_manual(values=c("#440154", "#31688e"))
1659614478290:#plot EU
  1659614478312:ggplot(plot_EU, aes(x=as.factor(year), y=complete_footprint, fill= type)) +
  1659614478335:geom_boxplot()+
  1659614478357:theme(legend.position="none")+
  1659614478381:xlab("years")+
  1659614478400:ylab("Footprint [km²]")+
  1659614478423:ggtitle("EU") +
  1659614478444:scale_fill_manual(values=c("#31688e", "#35b779"))
1659614551428:load data
1659614551475:footprint_CHN <- readRDS("~/Deforestation-calculation/Paper/footprint_CHN.rds")
1659614551503:footprint_EU <- readRDS("~/Deforestation-calculation/Paper/footprint_EU.rds")
1659614551561:# aggregate EU countries to EU total
  1659614551586:plot_EU <- footprint_EU %>%
  1659614551611:group_by(year, amort, var) %>%
  1659614551635:summarise(complete_footprint = sum(complete_footprint)) %>%
  1659614551660:ungroup() %>%
  1659614551685:mutate(year = as.character(year))
1659614551877:avg <- plot_EU %>%
  1659614551900:group_by(amort, var) %>%
  1659614551921:summarise(complete_footprint = sum(complete_footprint)/length(unique(plot_EU$year))) %>%
  1659614551941:ungroup() %>%
  1659614551961:mutate(year = "average")
1659614552027:plot_EU <- bind_rows(plot_EU, avg)%>%
  1659614552053:mutate(type=ifelse(year=="average","Highlighted","Normal"))%>%
  1659614552080:mutate(complete_footprint = complete_footprint / 100)
1659614552138:rm(avg)
1659614552203:#plot EU
  1659614552225:ggplot(plot_EU, aes(x=as.factor(year), y=complete_footprint, fill= type)) +
  1659614552249:geom_boxplot()+
  1659614552274:theme(legend.position="none")+
  1659614552299:xlab("years")+
  1659614552326:ylab("Footprint [km²]")+
  1659614552353:ggtitle("EU") +
  1659614552378:scale_fill_manual(values=c("#31688e", "#35b779"))
1659614553146:#prepare plot data for CHINA
  1659614553172:plot_CHN <-  footprint_CHN %>%
  1659614553193:group_by(year, amort, var) %>%
  1659614553214:summarise(complete_footprint = sum(complete_footprint)) %>%
  1659614553236:ungroup() %>%
  1659614553261:mutate(year = as.character(year))
1659614554023:avg <- plot_CHN %>%
  1659614554046:group_by(amort, var) %>%
  1659614554071:summarise(complete_footprint = sum(complete_footprint)/length(unique(plot_CHN$year))) %>%
  1659614554099:ungroup() %>%
  1659614554123:mutate(year = "average")
1659614554211:plot_CHN <- bind_rows(plot_CHN, avg)%>%
  1659614554231:mutate(type=ifelse(year=="average","Highlighted","Normal"))%>%
  1659614554263:mutate(complete_footprint = complete_footprint / 100)
1659614554313:# plot China
  1659614554336:ggplot(plot_CHN, aes(x=as.factor(year), y=complete_footprint, fill= type)) +
  1659614554360:geom_boxplot()+
  1659614554382:theme(legend.position="none")+
  1659614554401:xlab("years")+
  1659614554424:ylab("Footprint [km²]")+
  1659614554448:ggtitle("CHN") +
  1659614554470:scale_fill_manual(values=c("#31688e", "#35b779"))
1659614555775:rm(list = ls())
1659614628059:#load data
  1659614628085:defor <- readRDS("~/Deforestation-calculation/input/defor.rds")
1659614628141:facet_data <- defor %>%
  1659614628170:gather(key= y, value = "value",1:3)%>%
  1659614628193:mutate(value = value / 100)
1659614628247:facet_data$y <- substring(facet_data$y, 2)
1659614628294:# combine amort & intervall into one variable
  1659614628318:a <- "a"
1659614628339:b <- "_y"
1659614628364:facet_data$amort_var <- paste0(a, sprintf("%02d",as.numeric(facet_data$amort)),
                                             1659614628381:b, sprintf("%02d",as.numeric(facet_data$y)))
1659614628398:rm(a,b)
1659614628443:ggplot(facet_data, aes(x = year, y = value, fill = factor(luc, levels=c("indir", "dir")))) +
  1659614628465:geom_area()+
  1659614628485:facet_wrap(vars(amort_var))+
  1659614628505:ylab("deforestation [km²]")+
  1659614628528:labs(fill = "LUC")+
  1659614628549:scale_fill_manual(values = c("#31688e", "#35b779"))
1659614662156:ggplot(facet_data, aes(x = year, y = value, fill = factor(luc, levels=c("indir", "dir")))) +
  1659614662178:geom_area()+
  1659614662199:facet_wrap(vars(amort_var))+
  1659614662220:ylab("deforestation [km²]")+
  1659614662246:labs(fill = "LUC")+
  1659614662273:scale_fill_manual(values = c("#90d743", "#443983"))
1659614679574:scale_fill_manual(values = c( "#443983", "#90d743")
                                1659614685843:ggplot(facet_data, aes(x = year, y = value, fill = factor(luc, levels=c("indir", "dir")))) +
                                  1659614685872:geom_area()+
                                  1659614685895:facet_wrap(vars(amort_var))+
                                  1659614685918:ylab("deforestation [km²]")+
                                  1659614685942:labs(fill = "LUC")+
                                  1659614685965:scale_fill_manual(values = c( "#443983", "#90d743"))
                                1659614688040:ggplot(facet_data, aes(x = year, y = value, fill = factor(luc, levels=c("indir", "dir")))) +
                                  1659614688066:geom_area()+
                                  1659614688088:facet_wrap(vars(amort_var))+
                                  1659614688114:ylab("deforestation [km²]")+
                                  1659614688141:labs(fill = "LUC")+
                                  1659614688173:scale_fill_manual(values = c( "#443983", "#90d743"))
                                1659614725139:ggplot(facet_data, aes(x = year, y = value, fill = factor(luc, levels=c("indir", "dir")))) +
                                  1659614725173:geom_area()+
                                  1659614725198:facet_wrap(vars(amort_var))+
                                  1659614725222:ylab("deforestation [km²]")+
                                  1659614725246:labs(fill = "LUC")+
                                  1659614725270:scale_fill_manual(values = c( "#2c728e", "#addc30"))
                                1659614773297:ggplot(facet_data, aes(x = year, y = value, fill = factor(luc, levels=c("indir", "dir")))) +
                                  1659614773326:geom_area()+
                                  1659614773357:facet_wrap(vars(amort_var))+
                                  1659614773379:ylab("deforestation [km²]")+
                                  1659614773405:labs(fill = "LUC")+
                                  1659614773429:scale_fill_manual(values = c( "#31688e", "#addc30"))
                                1659614815067:ggplot(facet_data, aes(x = year, y = value, fill = factor(luc, levels=c("indir", "dir")))) +
                                  1659614815093:geom_area()+
                                  1659614815117:facet_wrap(vars(amort_var))+
                                  1659614815139:ylab("deforestation [km²]")+
                                  1659614815161:labs(fill = "LUC")+
                                  1659614815187:scale_fill_manual(values = c( "#31688e", "#35b779"))
                                1659614818988:ggplot(facet_data, aes(x = year, y = value, fill = factor(luc, levels=c("indir", "dir")))) +
                                  1659614819018:geom_area()+
                                  1659614819044:facet_wrap(vars(amort_var))+
                                  1659614819072:ylab("deforestation [km²]")+
                                  1659614819098:labs(fill = "LUC")+
                                  1659614819124:scale_fill_manual(values = c( "#31688e", "#35b779"))
                                1659614856133:ggplot(facet_data, aes(x = year, y = value, fill = factor(luc, levels=c("indir", "dir")))) +
                                  1659614856176:geom_area()+
                                  1659614856205:facet_wrap(vars(amort_var))+
                                  1659614856230:ylab("deforestation [km²]")+
                                  1659614856254:labs(fill = "LUC")+
                                  1659614856276:scale_fill_manual(values = c( "#31688e", "#addc30"))
                                1659614876681:g
                                1659614880643:ggplot(facet_data, aes(x = year, y = value, fill = factor(luc, levels=c("indir", "dir")))) +
                                  1659614880670:geom_area()+
                                  1659614880694:facet_wrap(vars(amort_var))+
                                  1659614880718:ylab("deforestation [km²]")+
                                  1659614880748:labs(fill = "LUC")+
                                  1659614880770:scale_fill_manual(values = c( "#31688e", "#addc30"))
                                1659615032566:#load data
                                  1659615032593:defor <- readRDS("~/Deforestation-calculation/input/defor.rds")
                                1659615032632:# summarized direct+indirect
                                  1659615032653:sens_table <- readRDS("~/Deforestation-calculation/Analysis_deforestation/sens_table.rds")
                                1659615032711:fun <- function(x){
                                  1659615032734:round(((x/sens_table[,6])-1) * 100, 0)
                                  1659615032757:}
                                1659615032794:plot_data <- data.frame(apply(sens_table,2, fun))
                                1659615032862:colnames(plot_data) <- colnames(sens_table)
                                1659615032885:plot_data$year <- sens_table$year
                                1659615032923:plot_data <- plot_data %>%
                                  1659615032945:gather("key","value",-year) %>%
                                  1659615032971:mutate(amort = paste0("a", sprintf("%02d", as.numeric(substr(key, regexpr("a", key)+1,10))))) %>%
                                  1659615032995:mutate(intervall = paste0("y", sprintf("%02d", as.numeric(substr(key, regexpr("y", key)+1,regexpr("a", key)-1))))) %>%
                                  1659615033013:select(-key)
                                1659615033071:ggplot(plot_data, aes(x= year, y= value, colour = amort))+
                                  1659615033095:geom_line(aes(linetype=intervall, color = amort))+
                                  1659615033118:ylab("deviation [%]")+
                                  1659615033140:#scale_colour_viridis_d("amortization", direction = -1)+
                                  1659615033161:scale_linetype_manual(values=c("twodash", "dotted", "solid"))+
                                  1659615033180:#labs(colour = "system definitions")
                                  1659615033207:scale_colour_manual(values = c("#5ec962", "#3b528b","#440154") )
                                1659615033919:rm(list = ls())
                                1659615038138:#load data
                                  1659615038167:defor <- readRDS("~/Deforestation-calculation/input/defor.rds")
                                1659615038347:# summarized direct+indirect
                                  1659615038376:sens_table <- readRDS("~/Deforestation-calculation/Analysis_deforestation/sens_table.rds")
                                1659615041506:plot_data <- data.frame(apply(sens_table,2, fun))
                                1659615042192:colnames(plot_data) <- colnames(sens_table)
                                1659615042348:plot_data <- plot_data %>%
                                  1659615042375:gather("key","value",-year) %>%
                                  1659615042400:mutate(amort = paste0("a", sprintf("%02d", as.numeric(substr(key, regexpr("a", key)+1,10))))) %>%
                                  1659615042423:mutate(intervall = paste0("y", sprintf("%02d", as.numeric(substr(key, regexpr("y", key)+1,regexpr("a", key)-1))))) %>%
                                  1659615042456:select(-key)
                                1659615042723:plot_data$year <- sens_table$year
                                1659615043106:fun <- function(x){
                                  1659615043130:round(((x/sens_table[,6])-1) * 100, 0)
                                  1659615043155:}
                                1659615043262:ggplot(plot_data, aes(x= year, y= value, colour = amort))+
                                  1659615043291:geom_line(aes(linetype=intervall, color = amort))+
                                  1659615043319:ylab("deviation [%]")+
                                  1659615043346:#scale_colour_viridis_d("amortization", direction = -1)+
                                  1659615043370:scale_linetype_manual(values=c("twodash", "dotted", "solid"))+
                                  1659615043394:#labs(colour = "system definitions")
                                  1659615043414:scale_colour_manual(values = c("#5ec962", "#3b528b","#440154") )
                                1659615062795:# summarized direct+indirect
                                  1659615062821:sens_table <- readRDS("~/Deforestation-calculation/Analysis_deforestation/sens_table.rds")
                                1659615064066:fun <- function(x){
                                  1659615064092:round(((x/sens_table[,6])-1) * 100, 0)
                                  1659615064122:}
                                1659615065600:plot_data <- data.frame(apply(sens_table,2, fun))
                                1659615067928:colnames(plot_data) <- colnames(sens_table)
                                1659615071292:plot_data <- plot_data %>%
                                  1659615071319:gather("key","value",-year) %>%
                                  1659615071343:mutate(amort = paste0("a", sprintf("%02d", as.numeric(substr(key, regexpr("a", key)+1,10))))) %>%
                                  1659615071365:mutate(intervall = paste0("y", sprintf("%02d", as.numeric(substr(key, regexpr("y", key)+1,regexpr("a", key)-1))))) %>%
                                  1659615071391:select(-key)
                                1659615073282:plot_data$year <- sens_table$year
                                1659615086696:ggplot(plot_data, aes(x= year, y= value, colour = amort))+
                                  1659615086726:geom_line(aes(linetype=intervall, color = amort))+
                                  1659615086754:ylab("deviation [%]")+
                                  1659615086782:#scale_colour_viridis_d("amortization", direction = -1)+
                                  1659615086813:scale_linetype_manual(values=c("twodash", "dotted", "solid"))+
                                  1659615086838:#labs(colour = "system definitions")
                                  1659615086865:scale_colour_manual(values = c("#5ec962", "#3b528b","#440154") )
                                1659957282208:footprint_CHN <- readRDS("~/Deforestation-calculation/Paper/footprint_CHN.rds")
                                1659957282246:footprint_EU <- readRDS("~/Deforestation-calculation/Paper/footprint_EU.rds")
                                1659957282299:# aggregate EU countries to EU total
                                  1659957282325:plot_EU <- footprint_EU %>%
                                  1659957282349:group_by(year, amort, var) %>%
                                  1659957282375:summarise(complete_footprint = sum(complete_footprint)) %>%
                                  1659957282408:ungroup() %>%
                                  1659957282435:mutate(year = as.character(year))
                                1659957282575:avg <- plot_EU %>%
                                  1659957282600:group_by(amort, var) %>%
                                  1659957282628:summarise(complete_footprint = sum(complete_footprint)/length(unique(plot_EU$year))) %>%
                                  1659957282651:ungroup() %>%
                                  1659957282674:mutate(year = "average")
                                1659957282739:plot_EU <- bind_rows(plot_EU, avg)%>%
                                  1659957282761:mutate(type=ifelse(year=="average","Highlighted","Normal"))%>%
                                  1659957282784:mutate(complete_footprint = complete_footprint / 100)
                                1659957282847:rm(avg)
                                1659957282905:#plot EU
                                  1659957282932:ggplot(plot_EU, aes(x=as.factor(year), y=complete_footprint, fill= type)) +
                                  1659957282961:geom_boxplot()+
                                  1659957282987:theme(legend.position="none")+
                                  1659957283016:xlab("years")+
                                  1659957283040:ylab("Footprint [km²]")+
                                  1659957283067:ggtitle("EU") +
                                  1659957283094:scale_fill_manual(values=c("#31688e", "#35b779"))
                                1659957284250:#prepare plot data for CHINA
                                  1659957284271:plot_CHN <-  footprint_CHN %>%
                                  1659957284297:group_by(year, amort, var) %>%
                                  1659957284319:summarise(complete_footprint = sum(complete_footprint)) %>%
                                  1659957284343:ungroup() %>%
                                  1659957284364:mutate(year = as.character(year))
                                1659957284891:avg <- plot_CHN %>%
                                  1659957284911:group_by(amort, var) %>%
                                  1659957284931:summarise(complete_footprint = sum(complete_footprint)/length(unique(plot_CHN$year))) %>%
                                  1659957284957:ungroup() %>%
                                  1659957284979:mutate(year = "average")
                                1659957285037:plot_CHN <- bind_rows(plot_CHN, avg)%>%
                                  1659957285056:mutate(type=ifelse(year=="average","Highlighted","Normal"))%>%
                                  1659957285076:mutate(complete_footprint = complete_footprint / 100)
                                1659957285125:# plot China
                                  1659957285146:ggplot(plot_CHN, aes(x=as.factor(year), y=complete_footprint, fill= type)) +
                                  1659957285166:geom_boxplot()+
                                  1659957285185:theme(legend.position="none")+
                                  1659957285204:xlab("years")+
                                  1659957285225:ylab("Footprint [km²]")+
                                  1659957285250:ggtitle("CHN") +
                                  1659957285272:scale_fill_manual(values=c("#31688e", "#35b779"))
                                1659957291645:View(plot_EU)
                                1659957319242:filter(plot_EU, year == 2000)
                                1659957353713:test <- filter(plot_EU, year == 2000)
                                1659957355790:View(test)
                                1659957369891:rm(test)
                                1659957771745:filter(plot_EU, year == 2001)
                                1659958532535:filter(plot_CHN, year == 2001)
                                1659960949519:#plot EU
                                  1659960949540:ggplot(plot_EU, aes(x=as.factor(year), y=complete_footprint, fill= type)) +
                                  1659960949560:geom_boxplot()+
                                  1659960949582:scale_y_continuous("Footprint [km²]", limits = c(0, 3500)) +
                                  1659960949603:theme(legend.position="none")+
                                  1659960949625:xlab("years")+
                                  1659960949647:ylab("Footprint [km²]")+
                                  1659960949670:ggtitle("EU") +
                                  1659960949696:scale_fill_manual(values=c("#31688e", "#35b779"))
                                1659960975877:#plot EU
                                  1659960975898:ggplot(plot_EU, aes(x=as.factor(year), y=complete_footprint, fill= type)) +
                                  1659960975922:geom_boxplot()+
                                  1659960975945:scale_y_continuous("Footprint [km²]", limits = c(0, 3200)) +
                                  1659960975967:theme(legend.position="none")+
                                  1659960975991:xlab("years")+
                                  1659960976015:ylab("Footprint [km²]")+
                                  1659960976037:ggtitle("EU") +
                                  1659960976057:scale_fill_manual(values=c("#31688e", "#35b779"))
                                1659960988004:# plot China
                                  1659960988026:ggplot(plot_CHN, aes(x=as.factor(year), y=complete_footprint, fill= type)) +
                                  1659960988054:geom_boxplot()+
                                  1659960988081:theme(legend.position="none")+
                                  1659960988104:xlab("years")+
                                  1659960988130:scale_y_continuous("Footprint [km²]", limits = c(0, 3200))+
                                  1659960988152:ggtitle("CHN") +
                                  1659960988180:scale_fill_manual(values=c("#31688e", "#35b779"))
                                1659961013602:# plot China
                                  1659961013623:ggplot(plot_CHN, aes(x=as.factor(year), y=complete_footprint, fill= type)) +
                                  1659961013645:geom_boxplot()+
                                  1659961013670:theme(legend.position="none")+
                                  1659961013694:xlab("years")+
                                  1659961013717:scale_y_continuous("Footprint [km²]", limits = c(0, 3500))+
                                  1659961013739:ggtitle("CHN") +
                                  1659961013761:scale_fill_manual(values=c("#31688e", "#35b779"))
                                1659961020627:#plot EU
                                  1659961020649:ggplot(plot_EU, aes(x=as.factor(year), y=complete_footprint, fill= type)) +
                                  1659961020675:geom_boxplot()+
                                  1659961020703:scale_y_continuous("Footprint [km²]", limits = c(0, 3500)) +
                                  1659961020728:theme(legend.position="none")+
                                  1659961020751:xlab("years")+
                                  1659961020772:ylab("Footprint [km²]")+
                                  1659961020796:ggtitle("EU") +
                                  1659961020817:scale_fill_manual(values=c("#31688e", "#35b779"))
                                1659961052923:l
                                1659961054491:#prepare plot data for CHINA
                                  1659961054519:plot_CHN <-  footprint_CHN %>%
                                  1659961054540:group_by(year, amort, var) %>%
                                  1659961054563:summarise(complete_footprint = sum(complete_footprint)) %>%
                                  1659961054588:ungroup() %>%
                                  1659961054614:mutate(year = as.character(year))
                                1659961063084:avg <- plot_CHN %>%
                                  1659961063104:group_by(amort, var) %>%
                                  1659961063126:summarise(complete_footprint = sum(complete_footprint)/length(unique(plot_CHN$year))) %>%
                                  1659961063150:ungroup() %>%
                                  1659961063173:mutate(year = "average")
                                1659961063763:plot_CHN <- bind_rows(plot_CHN, avg)%>%
                                  1659961063792:mutate(type=ifelse(year=="average","Highlighted","Normal"))%>%
                                  1659961063819:mutate(complete_footprint = complete_footprint / 100)
                                1659961064397:# plot China
                                  1659961064419:ggplot(plot_CHN, aes(x=as.factor(year), y=complete_footprint, fill= type)) +
                                  1659961064442:geom_boxplot()+
                                  1659961064465:theme(legend.position="none")+
                                  1659961064489:xlab("years")+
                                  1659961064512:scale_y_continuous("Footprint [km²]", limits = c(0, 3500))+
                                  1659961064536:ggtitle("CHN") +
                                  1659961064566:scale_fill_manual(values=c("#31688e", "#35b779"))
                                1659961604166:footprint_CHN <- readRDS("~/Deforestation-calculation/Paper/footprint_CHN.rds")
                                1659961604597:footprint_EU <- readRDS("~/Deforestation-calculation/Paper/footprint_EU.rds")
                                1659961609455:View(footprint_CHN)
                                1659975101074:footprint_CHN <- readRDS("~/Deforestation-calculation/Paper/footprint_CHN.rds")
                                1659975101725:footprint_EU <- readRDS("~/Deforestation-calculation/Paper/footprint_EU.rds")
                                1659975106743:footprint_CHN <- readRDS("~/Deforestation-calculation/Paper/footprint_CHN.rds")
                                1659975141049:View(footprint_CHN)
                                1659975202189:table_CHN <- footprint_CHN %>%
                                  1659975202214:filter(iso == "dir")
                                1659975205403:View(table_CHN)
                                1659975213855:table_CHN <- footprint_CHN %>%
                                  1659975213883:filter(luc == "dir")
                                1659975216110:View(table_CHN)
                                1659975326372:table_CHN <- footprint_CHN %>%
                                  1659975326400:filter(luc == "dir")%>%
                                  1659975326421:gather(key = "year", value = "complete_footprint")
                                1659975344933:table_CHN <- footprint_CHN %>%
                                  1659975344962:filter(luc == "dir")%>%
                                  1659975344986:gather()
                                1659975360623:table_CHN <- footprint_CHN %>%
                                  1659975360650:filter(luc == "dir")#%>%
                                1659975453428:table_CHN <- footprint_CHN %>%
                                  1659975453452:filter(luc == "dir")%>%
                                  1659975453477:select(!iso & !nonfood & !food)
                                1659975468002:table_CHN <- footprint_CHN %>%
                                  1659975468027:filter(luc == "dir")%>%
                                  1659975468052:select(!iso & !nonfood & !food)
                                1659975489252:table_CHN <- footprint_CHN %>%
                                  1659975489279:filter(luc == "dir")%>%
                                  1659975489302:select(!iso & !nonfood)
                                1659975518628:table_CHN <- footprint_CHN %>%
                                  1659975518651:filter(luc == "dir")%>%
                                  1659975518676:select(!iso & !nonfood &!food)
                                1659975540910:table_CHN <- footprint_CHN %>%
                                  1659975540934:filter(luc == "dir")%>%
                                  1659975540958:select(!iso & !nonfood &!food)%>%
                                  1659975540982:select(!iso)
                                1659975555805:table_CHN <- footprint_CHN %>%
                                  1659975555828:filter(luc == "dir")%>%
                                  1659975555849:select(!nonfood &!food)%>%
                                  1659975555874:select(!iso)
                                1659975566512:table_CHN <- footprint_CHN %>%
                                  1659975566539:filter(luc == "dir")%>%
                                  1659975566561:select(!nonfood &!food)
                                1659975590942:table_CHN <- footprint_CHN %>%
                                  1659975590969:filter(luc == "dir")%>%
                                  1659975590995:select(!iso)
                                1659975595508:table_CHN <- footprint_CHN %>%
                                  1659975595534:filter(luc == "dir")%>%
                                  1659975595558:select(!iso)
                                1659975605798:table_CHN <- footprint_CHN %>%
                                  1659975605823:select(!iso)
                                1659975729072:table_CHN <- footprint_CHN %>%
                                  1659975729100:filter(luc == "dir")%>%
                                  1659975729130:ungroup()%>%
                                  1659975729154:select(!iso & !nonfood &!food)
                                1659975743673:table_CHN <- footprint_CHN %>%
                                  1659975743698:filter(luc == "dir")%>%
                                  1659975743725:ungroup()%>%
                                  1659975743748:select(!iso & !nonfood &!food)%>%
                                  1659975743773:gather()
                                1659975774065:table_CHN <- footprint_CHN %>%
                                  1659975774089:filter(luc == "dir")%>%
                                  1659975774112:ungroup()%>%
                                  1659975774133:select(!iso & !nonfood &!food)#%>%
                                1659975784377:gather(key = "complete_footprint")
                                1659975787364:table_CHN <- footprint_CHN %>%
                                  1659975787388:filter(luc == "dir")%>%
                                  1659975787412:ungroup()%>%
                                  1659975787435:select(!iso & !nonfood &!food)%>%
                                  1659975787461:gather(key = "complete_footprint")
                                1659975819459:table_CHN <- footprint_CHN %>%
                                  1659975819484:filter(luc == "dir")%>%
                                  1659975819506:ungroup()%>%
                                  1659975819532:select(!iso & !nonfood &!food)#%>%
                                1659975894232:table_CHN <- footprint_CHN %>%
                                  1659975894261:filter(luc == "dir")%>%
                                  1659975894292:ungroup()%>%
                                  1659975894323:select(!iso & !nonfood &!food)%>%
                                  1659975894351:mutate(amort_var = paste0("a",sprintf("%02d", amort),"_y",sprintf("%02d",var))) %>%
                                  1659975894374:select(-amort,-var)
                                1659975917152:table_CHN <- footprint_CHN %>%
                                  1659975917180:filter(luc == "dir")%>%
                                  1659975917206:ungroup()%>%
                                  1659975917229:select(!iso & !nonfood &!food)%>%
                                  1659975917253:mutate(amort_var = paste0("a",sprintf("%02d", amort),"_y",sprintf("%02d",var))) %>%
                                  1659975917275:select(-amort,-var, -luc)
                                1659975940210:table_CHN <- footprint_CHN %>%
                                  1659975940235:filter(luc == "dir")%>%
                                  1659975940257:ungroup()%>%
                                  1659975940279:select(-iso, -nonfood, -food, -luc)%>%
                                  1659975940300:mutate(amort_var = paste0("a",sprintf("%02d", amort),"_y",sprintf("%02d",var))) %>%
                                  1659975940323:select(-amort,-var)
                                1659975976407:table_CHN <- footprint_CHN %>%
                                  1659975976430:filter(luc == "dir")%>%
                                  1659975976456:ungroup()%>%
                                  1659975976479:select(-iso, -nonfood, -food, -luc)%>%
                                  1659975976516:mutate(amort_var = paste0("a",sprintf("%02d", amort),"_y",sprintf("%02d",var))) %>%
                                  1659975976542:select(-amort,-var)%>%
                                  1659975976566:spread(amort_var, complete_footprint)
                                1659976322090:print.data.frame(table_CHN)
                                1659976395960:stargazer(table_CHN, type = 'text', out = 'out.txt')
                                1659976657735:xtable(table_CHN)
                                1659976670842:table_CHN_direct <- footprint_CHN %>%
                                  1659976670867:filter(luc == "dir")%>%
                                  1659976670890:ungroup()%>%
                                  1659976670913:select(-iso, -nonfood, -food, -luc)%>%
                                  1659976670934:mutate(amort_var = paste0("a",sprintf("%02d", amort),"_y",sprintf("%02d",var))) %>%
                                  1659976670955:select(-amort,-var)%>%
                                  1659976670979:spread(amort_var, complete_footprint)
                                1659976695960:table_CHN_indirect <- footprint_CHN %>%
                                  1659976695981:filter(luc == "indir")%>%
                                  1659976696003:ungroup()%>%
                                  1659976696025:select(-iso, -nonfood, -food, -luc)%>%
                                  1659976696045:mutate(amort_var = paste0("a",sprintf("%02d", amort),"_y",sprintf("%02d",var))) %>%
                                  1659976696068:select(-amort,-var)%>%
                                  1659976696090:spread(amort_var, complete_footprint)
                                1659976745111:table_EU_direct <- footprint_EU %>%
                                  1659976745137:filter(luc == "dir")%>%
                                  1659976745162:ungroup()%>%
                                  1659976745185:select(-iso, -nonfood, -food, -luc)%>%
                                  1659976745210:mutate(amort_var = paste0("a",sprintf("%02d", amort),"_y",sprintf("%02d",var))) %>%
                                  1659976745234:select(-amort,-var)%>%
                                  1659976745259:spread(amort_var, complete_footprint)
                                1659976761676:table_EU_direct <- footprint_EU %>%
                                  1659976761703:filter(luc == "dir")%>%
                                  1659976761729:ungroup()%>%
                                  1659976761753:select(-iso, -nonfood, -food, -luc)%>%
                                  1659976761777:mutate(amort_var = paste0("a",sprintf("%02d", amort),"_y",sprintf("%02d",var))) %>%
                                  1659976761801:select(-amort,-var)%>%
                                  1659976761824:spread(amort_var, complete_footprint)
                                1659976768705:table_EU_direct <- footprint_EU %>%
                                  1659976768733:filter(luc == "dir")#%>%
                                1659976775389:table_EU_direct <- footprint_EU %>%
                                  1659976775417:filter(luc == "dir")%>%
                                  1659976775443:ungroup()#%>%
                                1659976785034:table_EU_direct <- footprint_EU %>%
                                  1659976785066:filter(luc == "dir")%>%
                                  1659976785098:ungroup()%>%
                                  1659976785121:select(-iso, -nonfood, -food, -luc)#%>%
                                1659976793808:table_EU_direct <- footprint_EU %>%
                                  1659976793837:filter(luc == "dir")%>%
                                  1659976793864:ungroup()%>%
                                  1659976793886:select(-iso, -nonfood, -food, -luc)%>%
                                  1659976793908:mutate(amort_var = paste0("a",sprintf("%02d", amort),"_y",sprintf("%02d",var)))# %>%
                                1659976806715:table_EU_direct <- footprint_EU %>%
                                  1659976806740:filter(luc == "dir")%>%
                                  1659976806763:ungroup()%>%
                                  1659976806787:select(-iso, -nonfood, -food, -luc)%>%
                                  1659976806809:mutate(amort_var = paste0("a",sprintf("%02d", amort),"_y",sprintf("%02d",var))) %>%
                                  1659976806830:select(-amort,-var)#%>%
                                1659976811454:table_EU_direct <- footprint_EU %>%
                                  1659976811480:filter(luc == "dir")%>%
                                  1659976811507:ungroup()%>%
                                  1659976811530:select(-iso, -nonfood, -food, -luc)%>%
                                  1659976811552:mutate(amort_var = paste0("a",sprintf("%02d", amort),"_y",sprintf("%02d",var))) %>%
                                  1659976811578:select(-amort,-var)%>%
                                  1659976811604:spread(amort_var, complete_footprint)
                                1659976818249:table_EU_direct <- footprint_EU %>%
                                  1659976818277:filter(luc == "dir")%>%
                                  1659976818299:ungroup()%>%
                                  1659976818317:select(-iso, -nonfood, -food, -luc)%>%
                                  1659976818337:mutate(amort_var = paste0("a",sprintf("%02d", amort),"_y",sprintf("%02d",var))) %>%
                                  1659976818358:select(-amort,-var)#%>%
                                1659976826169:View(table_EU_direct)
                                1659976843213:table_EU_direct <- footprint_EU %>%
                                  1659976843241:filter(luc == "dir")%>%
                                  1659976843267:ungroup()%>%
                                  1659976843289:select(-iso, -nonfood, -food, -luc)%>%
                                  1659976843311:mutate(amort_var = paste0("a",sprintf("%02d", amort),"_y",sprintf("%02d",var))) %>%
                                  1659976843329:select(-amort,-var)%>%
                                  1659976843349:spread(amort_var, complete_footprint)
                                1659976889634:table_EU_direct <- footprint_EU %>%
                                  1659976889658:filter(luc == "dir")%>%
                                  1659976889679:ungroup()%>%
                                  1659976889699:select(-iso, -nonfood, -food, -luc)%>%
                                  1659976889722:mutate(amort_var = paste0("a",sprintf("%02d", amort),"_y",sprintf("%02d",var))) %>%
                                  1659976889747:select(-amort,-var)%>%
                                  1659976889774:spread(amort_var, complete_footprint)
                                1659976900629:table_CHN_indirect <- footprint_CHN %>%
                                  1659976900658:filter(luc == "indir")%>%
                                  1659976900683:ungroup()%>%
                                  1659976900707:select(-iso, -nonfood, -food, -luc)%>%
                                  1659976900732:mutate(amort_var = paste0("a",sprintf("%02d", amort),"_y",sprintf("%02d",var))) %>%
                                  1659976900760:select(-amort,-var)%>%
                                  1659976900784:spread(amort_var, complete_footprint)
                                1659976905453:table_CHN_direct <- footprint_CHN %>%
                                  1659976905477:filter(luc == "dir")%>%
                                  1659976905502:ungroup()%>%
                                  1659976905528:select(-iso, -nonfood, -food, -luc)%>%
                                  1659976905550:mutate(amort_var = paste0("a",sprintf("%02d", amort),"_y",sprintf("%02d",var))) %>%
                                  1659976905576:select(-amort,-var)%>%
                                  1659976905601:spread(amort_var, complete_footprint)
                                1659976917946:table_EU_direct <- footprint_EU %>%
                                  1659976917975:filter(luc == "dir")%>%
                                  1659976917998:ungroup()%>%
                                  1659976918022:select(-iso, -nonfood, -food, -luc)%>%
                                  1659976918046:mutate(amort_var = paste0("a",sprintf("%02d", amort),"_y",sprintf("%02d",var))) %>%
                                  1659976918071:select(-amort,-var)%>%
                                  1659976918100:spread(amort_var, complete_footprint)
                                1659976925485:table_EU_indirect <- footprint_EU %>%
                                  1659976925509:filter(luc == "indir")%>%
                                  1659976925533:ungroup()%>%
                                  1659976925557:select(-iso, -nonfood, -food, -luc)%>%
                                  1659976925578:mutate(amort_var = paste0("a",sprintf("%02d", amort),"_y",sprintf("%02d",var))) %>%
                                  1659976925600:select(-amort,-var)%>%
                                  1659976925625:spread(amort_var, complete_footprint)
                                1659976942628:table_EU_indirect <- footprint_EU %>%
                                  1659976942654:filter(luc == "indir")%>%
                                  1659976942678:ungroup()%>%
                                  1659976942699:select(-iso, -nonfood, -food, -luc)%>%
                                  1659976942722:mutate(amort_var = paste0("a",sprintf("%02d", amort),"_y",sprintf("%02d",var))) %>%
                                  1659976942746:select(-amort,-var)%>%
                                  1659976942775:ungroup()%>%
                                  1659976942803:spread(amort_var, complete_footprint)
                                1659976951031:table_EU_indirect <- footprint_EU %>%
                                  1659976951059:filter(luc == "indir")%>%
                                  1659976951088:ungroup()%>%
                                  1659976951115:select(-iso, -nonfood, -food, -luc)%>%
                                  1659976951140:mutate(amort_var = paste0("a",sprintf("%02d", amort),"_y",sprintf("%02d",var))) %>%
                                  1659976951165:select(-amort,-var)%>%
                                  1659976951194:spread(amort_var, complete_footprint)
                                1659976979945:table_EU_indirect <- footprint_EU %>%
                                  1659976979974:filter(luc == "indir")%>%
                                  1659976979998:ungroup()%>%
                                  1659976980022:select(-iso, -nonfood, -food, -luc)%>%
                                  1659976980050:mutate(amort_var = paste0("a",sprintf("%02d", amort),"_y",sprintf("%02d",var))) %>%
                                  1659976980082:select(-amort,-var)%>%
                                  1659976980107:spread(amort_var, complete_footprint)
                                1659976983268:table_EU_direct <- footprint_EU %>%
                                  1659976983292:filter(luc == "dir")%>%
                                  1659976983315:ungroup()%>%
                                  1659976983338:select(-iso, -nonfood, -food, -luc)%>%
                                  1659976983361:mutate(amort_var = paste0("a",sprintf("%02d", amort),"_y",sprintf("%02d",var))) %>%
                                  1659976983385:select(-amort,-var)%>%
                                  1659976983410:spread(amort_var, complete_footprint)
                                1659976996707:table_EU_direct <- footprint_EU %>%
                                  1659976996733:filter(luc == "dir")%>%
                                  1659976996760:ungroup()%>%
                                  1659976996784:select(-iso, -nonfood, -food, -luc)%>%
                                  1659976996810:mutate(amort_var = paste0("a",sprintf("%02d", amort),"_y",sprintf("%02d",var))) %>%
                                  1659976996829:select(-amort,-var)%>%
                                  1659976996848:spread(complete_footprint, amort_var)
                                1659977009634:table_EU_direct <- footprint_EU %>%
                                  1659977009659:filter(luc == "dir")%>%
                                  1659977009685:ungroup()%>%
                                  1659977009710:select(-iso, -nonfood, -food, -luc)%>%
                                  1659977009737:mutate(amort_var = paste0("a",sprintf("%02d", amort),"_y",sprintf("%02d",var))) %>%
                                  1659977009762:select(-amort,-var)%>%
                                  1659977009785:spread(amort_var, complete_footprint)
                                1659977173066:footprint_EU <- readRDS("~/Deforestation-calculation/Paper/footprint_EU.rds")
                                1659977177296:table_EU_direct <- footprint_EU %>%
                                  1659977177322:filter(luc == "dir")%>%
                                  1659977177345:ungroup()%>%
                                  1659977177368:select(-iso, -nonfood, -food, -luc)%>%
                                  1659977177390:mutate(amort_var = paste0("a",sprintf("%02d", amort),"_y",sprintf("%02d",var))) %>%
                                  1659977177412:select(-amort,-var)#%>%
                                1659977181732:table_EU_indirect <- footprint_EU %>%
                                  1659977181764:filter(luc == "indir")%>%
                                  1659977181793:ungroup()%>%
                                  1659977181816:select(-iso, -nonfood, -food, -luc)%>%
                                  1659977181840:mutate(amort_var = paste0("a",sprintf("%02d", amort),"_y",sprintf("%02d",var))) %>%
                                  1659977181865:select(-amort,-var)#%>%
                                1659977197598:table_EU_indirect <- footprint_EU %>%
                                  1659977197629:filter(luc == "indir")%>%
                                  1659977197659:ungroup()%>%
                                  1659977197686:select(-iso, -nonfood, -food, -luc)%>%
                                  1659977197711:mutate(amort_var = paste0("a",sprintf("%02d", amort),"_y",sprintf("%02d",var))) %>%
                                  1659977197733:select(-amort,-var)%>%
                                  1659977197757:spread(amort_var, complete_footprint) # warum geht das nicht ?????
                                1659978625744:load("~/.RData")
                                
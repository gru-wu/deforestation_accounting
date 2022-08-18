# Load libraries ----------------------------------------------------------

library(readxl)
library(tidyverse)
library(ggplot2)
library(magrittr)
library(labeling)
library(scales)

# Load data ----------------------------------------------------------

defor <- readRDS("~/Deforestation-calculation/input/defor.rds")

########################## Plots for amort. period 5 years -----------------------------------------
#
#
#  5 year period allocates the responsibility closer to the actual deforestation year
# If the results increase with the amort period it means that deforestaiton outside the time period considered was higher
# If the results decrease by increasing the amort period this meand that deforestation in the time period considered is highr than in the years before/after

#geom_point+ face_wrap (to see how dirct and indirect evolve (=increas/decrease))

#figure d1
ggplot(subset(defor, amort == 5), mapping = aes(x = year , y = y1))+ 
  geom_point(colour = "darkseagreen4", size = 2.5) +
  facet_wrap(~ luc, nrow= 2, scales = "free_y", labeller = labeller(luc = c("dir" = "direct deforestation", "indir" = "indirect deforestation"))) +
  scale_x_continuous(breaks = breaks_extended(15)) +
  ggtitle("Figure d1: deforestation data, (per year, amortized over 5 years) ")+
  labs( x = "years ", y = "deforestation (ha)") +
  theme(legend.title=element_blank())+
  theme_bw()+
  theme_light()

# here we can see that while the direct deforestation decrease, indirect deforestation increase 

#figure d2  / Thesis figure 4  

defor_fig4 <- defor %>%
  filter(amort == 5) %>%
  mutate( y10km2 = y10* 0.01)

ggplot(defor_fig4, mapping = aes(x = year , y = y10km2))+ 
  geom_point(colour = "darkseagreen4", size = 2.5) +
  facet_wrap(~ luc, nrow= 2, scales = "free_y", labeller = labeller(luc = c("dir" = "direct deforestation", "indir" = "indirect deforestation"))) +
  labs( x = "year ", y = "deforestation [km²]") +
  scale_x_continuous(breaks = breaks_extended(15))+
  #ggtitle("Figure d2: deforestation data, (10- year average, amortized over 5 years)")+
  theme(legend.title=element_blank())+
  theme_bw()+
  theme_light()
rm(defor_fig4)
#figure d3
ggplot(subset(defor, amort == 5), mapping = aes(x = year , y = y5))+ 
  geom_point(colour = "darkseagreen4", size = 2.5) +
  facet_wrap(~ luc, nrow= 2, scales = "free_y", labeller = labeller(luc = c("dir" = "direct deforestation", "indir" = "indirect deforestation"))) +
  labs( x = "years ", y = "deforestation (ha), (y5, amort5)") +
  scale_x_continuous(breaks = breaks_extended(15))+
  ggtitle("Figure d3: deforestation data, (5- year average, amortized over 5 years)")+
  theme(legend.title=element_blank())+
  theme_bw()+
  theme_light()



#geom_col (to see how total deforestation is split into direct and indirect/ to see how important the indirect factor is)

#figure d4
amort_5 <- subset(defor, amort == 5)
ggplot(amort_5) +
  geom_col(mapping = aes(x= year, y = y1, fill = luc)) +
  labs( x = "years", y = "deforestation (ha), (y1, amort5)") +
  scale_x_continuous(breaks = breaks_extended(15))+
  scale_fill_brewer(palette="Greens")+
  ggtitle("Figure d4: deforestation data, (per year, amortized over 5 years) ")+
  theme(legend.title=element_blank())+
  theme_bw()+
  theme_light()

#figure d5
ggplot(amort_5) +
  geom_area(mapping = aes(x= year, y = y5, fill = luc)) +
  labs( x = "years ", y = "deforestation (ha), (y5, amort5)") +
  scale_x_continuous(breaks = breaks_extended(15))+
  scale_fill_brewer(palette="Greens")+
  ggtitle("Figure d5: deforestation data, (5-year average, amortized over 5 years) ")+
  theme(legend.title=element_blank())+
  theme_bw()+
  theme_light()

#figure d6 / Thesis Figure 2 / Paper Figure 1
amort_5_fig2 <- amort_5 %>%
  mutate(y10km2 = y10*0.01)

ggplot(amort_5_fig2) +
  geom_area(mapping = aes(x= year, y = y10km2, fill = factor(luc, levels=c("indir", "dir")))) +
  labs(x = "year", y = "deforestation (km²)") +
  scale_x_continuous(breaks = breaks_extended(15))+
  scale_fill_brewer(palette="Greens")+
  #ggtitle("Figure d6: deforestation data, (10-year average, amortized over 5 years) ")+
  theme(legend.title=element_blank())+
  theme_bw()+
  theme_light() +
  guides(fill=guide_legend(title="luc"))

rm(amort_5_fig2)

#d6.1 / Thesis Figure 3 / Paper Figure 2
ggplot(amort_5, aes(x= year, y = (y10*100), fill = factor(luc, levels=c("indir", "dir")))) +
  geom_bar(position="fill", stat = "identity") +
  labs(x = "year", y= "share [%]") +
  scale_y_continuous(labels = scales::percent)+
  #coord_flip()+
  #ggtitle("Figure d6.1: share of direct/indirect deforestation")+
  theme(legend.title=element_blank())+
  theme_bw()+
  theme_light()+
  scale_fill_brewer(palette="Greens")+
  guides(fill=guide_legend(title="luc"))



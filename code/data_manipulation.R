library(tidyverse)


####1. Read in data####
d <- read_csv("./data/raw/spp_cover.csv") #read in species cover data
spp <- read_csv("./data/raw/spp_list.csv") #read in species attribute list


####2. Manipulate data####
####1. Read in data####
d <- read_csv("../data/raw/spp_cover.csv") #read in species cover data
spp <- read_csv("../data/raw/spp_list.csv") #read in species attribute list

d_long <- d %>% #here we convert to long ('tidy') format data
  select(PLOT_ID, YEAR, which(names(d)%in%spp$spp_code)) %>% #pick all the species cover columns
  gather(Species, Cover, -PLOT_ID, -YEAR)

d_direct <- d %>% #here we convert to long ('tidy') format data
  select(PLOT_ID, YEAR, grep("total",names(d))) %>% #pick the direct lifeform cover columns
  gather(Lifeform, direct_lf, -PLOT_ID, -YEAR)



#here we assign each species code to one lifeform (i.e. functional group):
d_long$lf <- spp[pmatch(d_long$Species,spp$spp_code, duplicates.ok = TRUE),"life_form"][[1]]
d_direct$lf <- str_split(d_direct$Lifeform, "_", simplify = TRUE)[,2] #There's a better way to do w/ regex

d_sum <- d_long %>%
  group_by(PLOT_ID,YEAR,lf) %>%
  summarise(sum_lf = sum(Cover, na.rm = TRUE))

d_direct <- d_direct %>% #simplify for the merge
  select(-Lifeform)

#here we set up a comparison data frame for plotting
d_compare <- 
  full_join(d_sum, d_direct, by = c("PLOT_ID","YEAR","lf"))

####2. Visualize data####
d_plot_old <- d_compare[d_compare$YEAR<2020,]
d_plot_old <- d_plot_old %>% 
  mutate(lf = factor(lf, levels = c("tree", "graminoid","shrub","forb","dshrub")))

#
pdf("./figures/cover_summed_vs_direct.pdf")
ggplot(d_plot_old, aes(x = sum_lf, y = direct_lf, col = lf) ) +
  geom_point() +
  geom_smooth(method = "lm") + 
  geom_abline(yintercept = 0, slope = 1) +
  theme_bw() +
  labs(x = "summed cover", y = "direct cover", col = "lifeform")
dev.off()

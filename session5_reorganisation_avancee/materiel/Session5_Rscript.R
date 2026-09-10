# Charger librairie
library(tidyverse)
library(ggpubr)

# Definir l'environnement de travail
setwd("/Users/sandra/Desktop/BPMP/Teaching/2024-2025/L3_bioinfo_2024/2024_L3_R/session5_reorganisation_avancee/")

# Charger les donnees
expt1 <- read_tsv("data/burghardt_et_al_2015_expt1.txt")

# changer de format avec pivot_longer()

expt1_long <- pivot_longer(expt1, names_to = "trait", 
                           values_to = "time.in.days",
                           cols = c(days.to.bolt, days.to.flower))

ggplot(expt1_long, aes(x=trait, y=time.in.days, col=genotype)) +
  geom_boxplot()


pivot_longer(expt1, names_to = "trait", 
             values_to = "time.in.days",
             cols = c(days.to.bolt, days.to.flower)) %>% 
  ggplot(aes(x=trait, y=time.in.days, col=genotype)) +
  geom_boxplot()

ggsave("boxplot_daystobolt_daystoflower_genotypes.pdf", 
       height = 6, width = 8)
ggsave("boxplot_daystobolt_daystoflower_genotypes.jpg")

## Exercice 2

pivot_longer(expt1, names_to = "trait", values_to = "length.mm",
             cols=c(blade.length.mm, total.leaf.length.mm)) %>% 
  ggplot(aes(x=trait, y=length.mm, fill=fluctuation)) +
  geom_boxplot(notch = TRUE)

expt1_graph <- pivot_longer(expt1, names_to = "trait", 
                            values_to = "length.mm",
             cols=c(blade.length.mm, total.leaf.length.mm)) %>% 
  ggplot(aes(x=trait, y=length.mm, fill=fluctuation)) +
  geom_boxplot(notch = TRUE)

expt1_graph + stat_compare_means()

# pivot_wider

expt1_wider <- select(expt1, plant_nb:vernalization, days.to.bolt) %>% 
  pivot_wider(names_from = fluctuation, values_from = days.to.bolt) %>% 
  unite(col = "treatment", temperature, day.length) %>% 
  ggplot(aes(x=Con, y=Var, col=treatment, shape=treatment)) +
  geom_point()

expt1_wider + stat_cor()

# JOIN

band_members

band_instruments

full_join(band_members, band_instruments, by="name")

inner_join(band_members, band_instruments, by="name")

left_join(band_members, band_instruments, by="name")

right_join(band_members, band_instruments, by="name")

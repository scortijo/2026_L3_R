# Preparation de l'environnement de travail

setwd("/Users/sandra/Desktop/BPMP/Teaching/2024-2025/L3_bioinfo_2024/2024_L3_R/session2_intro_ggplot/")
getwd()

library(tidyverse)
library(visdat)

# Chargement des donnees
my_data <- read_tsv("data/burghardt_et_al_2015_expt1.txt")

# Observation des donnees
View(my_data)
glimpse(my_data)
dim(my_data)
summary(my_data)


vis_dat(my_data)

# Beaucoup de donnees manquantes
# Enlever les lignes avec des donnees manquantes 
# dans au moins une colonne

my_data_noNA <- drop_na(my_data)

# Graphiques
ggplot(my_data, aes(x=genotype, y=days.to.flower)) +
  geom_boxplot()

ggplot(my_data, aes(x=genotype, y=days.to.flower)) +
  geom_violin()

ggplot(my_data, aes(x=days.to.flower, group=genotype, 
                    fill=genotype)) +
  geom_density(alpha=0.4)

# Pour retrouver le nom des colonnes
colnames(my_data)

# Graphique avec points et boxplot

ggplot(my_data, aes(x=genotype, y=days.to.flower)) +
  geom_jitter() +
  geom_boxplot(alpha=0)

ggplot(my_data, aes(x=genotype, y=days.to.flower)) +
  geom_boxplot() +
  geom_jitter() 


# Ajout de couleur

ggplot(my_data, aes(x=genotype, y=days.to.flower)) +
  geom_boxplot(colour="red", fill="blue")



ggplot(my_data, aes(x=genotype, y=days.to.flower, colour=fluctuation)) +
  geom_boxplot()

# Exercice 3
ggplot(my_data,aes(x=blade.length.mm, y=rosette.leaf.num, colour=genotype)) +
  geom_point()


ggplot(my_data,aes(x=blade.length.mm, y=rosette.leaf.num, colour=days.to.bolt)) +
  geom_point()


ggplot(my_data,aes(x=blade.length.mm, y=rosette.leaf.num, colour=temperature)) +
  geom_point()

# BONUS
ggplot(my_data,aes(x=blade.length.mm, y=rosette.leaf.num, colour=vernalization)) +
  geom_point() +
  scale_color_manual(values=c("V"="blue", "NV"="green"))


## boxplot et point

ggplot(my_data, aes(x=genotype, y=days.to.flower, colour=fluctuation)) +
  geom_boxplot() +
  geom_jitter()

ggplot(my_data, aes(x=genotype, y=days.to.flower)) +
  geom_boxplot(aes(colour=fluctuation)) +
  geom_jitter(colour="purple") 

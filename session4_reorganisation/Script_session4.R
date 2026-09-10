# Charger les librairies
library(tidyverse)

# Definir l'environnement de travail (session 4)
setwd("/Users/sandra/Desktop/BPMP/Teaching/2024-2025/L3_bioinfo_2024/2024_L3_R/session4_reorganisation/")

# Charger les donnees (burghardt_et_al_2015_expt1.txt) et les mettre dans un objet
expt1 <- read_tsv("data/burghardt_et_al_2015_expt1.txt")


# Selectionner des colonnes
colnames(expt1)

select(expt1, genotype, temperature)
select(expt1, -temperature)
select(expt1, contains("bolt"))

# Exercice 1: selectionnez les colonnes qui contiennent des donnees 
# mesurees en mm
colnames(expt1)
select(expt1, contains("mm"))

# BONUS: 
# Selection 1: selectionnez les colonnes dont le titre finit par "bolt"
select(expt1, ends_with("bolt"))
# Selection 2: Selectionnez les colonnes qui ne sont pas plant_nb et genotype
select(expt1, -plant_nb, - genotype)

# Filtrer des lignes
distinct(expt1, vernalization)

filter(expt1, vernalization=="V")
## nom de la colonne operateur valeur
# > superieur a
# >= superieur ou egal a
# < inferieur a (< -3, attention l'espace est essentiel)
# <= inferieur ou egal a
# == egal a (pour du texte et des chiffres)
# != different de (n'est pas egal a)
# %in% (est contenu dans une liste)

# 
# is.na() (is.na(blade.length.mm), garder les lignes avec des donnes manquantes
# dans la colonne blade.length.mm)
# !is.na (garde les lignes sans donnees manquantes pour la colonne en question)

# combiner plusieurs operations
# & ET
# | OU

filter(expt1, vernalization=="V" & fluctuation=="Var")
filter(expt1, vernalization=="V" & fluctuation=="var") # attention pas de message d'erreur
# mais ne retourne rien car il n'y a pas "var" mais "Var"
# Pour savoir ce qu'il y a dans la colonne fluctuation, utiliser distinct()
distinct(expt1, fluctuation)

filter(expt1, day.length=="8" | days.to.bolt > 85)
filter(expt1, day.length==8 | days.to.bolt > 85)

# Exercice 2 
# Cas 1: garder les plantes qui ne sont pas du background Ler et qui ont ete 
# traite avec une temperature fluctuante
filter(expt1, background!="Ler" & fluctuation=="Var")

# Cas 2: garder les plantes qui ont fleuries (bolt) en moins de 57 jours et qui 
# ont moins de 40 feuilles de rosette
filter(expt1, days.to.bolt<57 & rosette.leaf.num < 40)

# Cas 3: garder les plantes du genotype fca-6 pour qui le 
# blade.length.mm n'est pas manquant
filter(expt1, genotype=="fca-6" & !is.na(blade.length.mm))

## BONUS
# garder les lignes pour lesquelles le background contient Col
# sauvez le resultat dans un nouvel objet et verifiez les valeurs avec distinct()
distinct(expt1, background)
filter(expt1, background=="Col" | background=="Col FRI")
filter(expt1, background!="Ler")

expt1_filtered <-filter(expt1, grepl('Col', background))
distinct(expt1_filtered, background)

# exporter un objet

write_tsv(expt1_filtered, file="data/expt1_background_contains_col.txt")

# chaines de commandes: %>% (pipe)
filter(expt1, vernalization=="V") %>% 
  select(genotype, total.leaf.length.mm)


# Exercice 3: utilisez le pipe ( %>% ) pour faire les 2 operations suivantes 
# a la suite:
# garder les plantes qui ne sont pas du background Ler et qui ont ete
# traitees avec une temperature fluctuantes
# et garder les colonnes qui contiennent le genotype, la longueur de blade 
# et des informations sur le bolting

filter(expt1, background!="Ler" & fluctuation=="Var") %>% 
  select(., genotype, blade.length.mm, contains("bolt"))



# BONUS: transformez l'enchainement de commandes suivant en utilisant
# des pipes plutot que des objets intermediaires
read_tsv("data/burghardt_et_al_2015_expt1.txt") %>% 
filter( fluctuation=="Con" & day.length==16) %>% 
select(days.to.bolt:total.leaf.length.mm) %>% 
summary()

# Combiner avec un graphique
expt1 %>% 
  filter(vernalization=="V") %>% 
  ggplot(aes(x=fluctuation, y=days.to.flower)) +
  geom_boxplot()

# Exercice 4
# En utilisant %>%  faites un violin plot du temps de floraison
# pour les differents genotypes, mais uniquement pour les plantes
# ayant pousse en jours court (8h)

filter(expt1, day.length==8) %>% 
  ggplot(aes(x=genotype, y=days.to.flower)) +
  geom_violin()

## BONUS
# Representez, pour les plantes qui contiennent le mot "Col"
# dans background et qui ont pousse a 22c, un boxplot de days.to.bolt
# en fonction de vernalization, colore en fonction de la fluctuation
# Separez les graphiques pour avoir un facet par genotype
# Utilisez le theme theme_bw
## Interpretez le resultat

library(ggpubr)

filter(expt1, grepl("Col", background) & temperature==22) %>% 
  ggplot(aes(x=vernalization, y=days.to.bolt, fill=fluctuation)) +
  geom_boxplot() +
  facet_wrap(~genotype) +
  theme_bw() +
  stat_compare_means(label="p.signif", label.y=5)

# mutate()
expt1.cm <- mutate(expt1, total.leaf.length.cm= total.leaf.length.mm/10)

expt1.cm <- mutate(expt1, 
                   total.leaf.length.cm= total.leaf.length.mm/10,
                   blade.length.cm=blade.length.mm/10)

# Exercice 5
# Creez une nouvelle colonne blade.ratio avec la ratio
# de blade.length.mm et total.leaf.length.mm

expt1_ratio <- mutate(expt1, blade.ratio=blade.length.mm/total.leaf.length.mm)

## BONUS
# creez une nouvelle colonne "late_flowering" qui contient TRUE si 
# days.to.bolt est syperieur a 70 et FALSE sinon
expt1_late <-mutate(expt1, late_flowering=case_when(days.to.bolt > 70 ~ TRUE,
                                       days.to.bolt < 70 ~ FALSE))

expt1_late <-mutate(expt1, late_flowering=if_else(days.to.bolt > 70, "late", "early"))

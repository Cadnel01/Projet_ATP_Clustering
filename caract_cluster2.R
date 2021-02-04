#Cluster 2 


df_clean %>% filter(classe_hc == 2) -> Base2
Base2 <- Base2[,-c(9,10)]
#Taille
summary(Base2)
df_clean %>% 
  ggplot()+geom_boxplot(mapping = aes(x = as.character(df_clean$classe_hc), y = height,
                                      fill = as.character(df_clean$classe_hc) ))
#Resultats
#La taille moyenne des joueurs de ce cluster varient entre 173 et 190 cm avec une moyenne de 184.2
#Ces joueurs sont rélativement grands par rapport aux groupes 1 et 4
#ACE
df_clean %>% 
  ggplot()+geom_boxplot(mapping = aes(x = as.character(df_clean$classe_hc), y = ace,
                                      fill = as.character(df_clean$classe_hc) ))

#Ces joeurs réalisent en moyenne 13 services gagnants par match ce qui est inférieure à la moyenne du cluster 3
#et supérieurs aux moyennes de services gagnants des cluster 1 et 4

#Stin: Nombre de services réussis du premier coup
df_clean %>% 
  ggplot()+geom_boxplot(mapping = aes(x = as.character(df_clean$classe_hc), y = stin,
                                      fill = as.character(df_clean$classe_hc) ))
#Dans ce groupe, les joueurs ont tendance à reussir les service du 1er coup, ce qui 
#donne en moyenne envirion 124 services reussis du premier coup. Ce groupe se 
#positione à la tete des clusters en terme de service reussis au 1er coup avec un max de 178

#StWon: Nombre de fois où le joueur a gagné le premier point dès le premier service
df_clean %>% 
  ggplot()+geom_boxplot(mapping = aes(x = as.character(df_clean$classe_hc), y = stWon,
                                      fill = as.character(df_clean$classe_hc) ))
#En terme de gain du 1er point dès le 1er service, les joueurs de ce cluster s'illustrent mieux avec une moyenne
# environ 85 par match ce qui est considérablement supérieur aux moyennes des autres clusters.

# Nombre de break point sauvé bpSaved
df_clean %>% 
  ggplot()+geom_boxplot(mapping = aes(x = as.character(df_clean$classe_hc), y = bpSaved,
                                      fill = as.character(df_clean$classe_hc) ))

#Dans la catégorie des break point sauvé, ce cluster fait encore la différence
#avec une moyenne d'environ 12 par match ce qui est nettement supérieures aux moyennes

# Nombre de break point gagné bpFaced
df_clean %>% 
  ggplot()+geom_boxplot(mapping = aes(x = as.character(df_clean$classe_hc), y = bpFaced,
                                      fill = as.character(df_clean$classe_hc) ))
#Les joueurs gagnent un maximun de break point face à leurs adversaires, avec en moyenne 20 break point gagné par match
#ce qui les classe à la tete de cette cette catégorie

#En définitive, les joueurs de ce cluster ont l'avantage au niveau des break point sauvé et gagnés
#Ensuite ils gagnent généralement le 1er point à partir du 1er service tce qui justifie leur reussite
# de service au 1er coup. Ces joeurs ont une taille normale avoisinant les 184.2 cm

#Realisation de l'ACP
acp <- PCA(Base2, scale.unit = T, graph = TRUE, ncp = 3, axes=c(1,3))

# valeurs propres et inertie
acp[1]
fviz_eig(acp)

# coord - qualité - contrib des variables
acp[2]$var$coord
acp[2]$var$cos2
acp[2]$var$contrib

# coord - qualité - contrib des individus
acp[3]$ind$coord
acp[3]$ind$cos2
acp[3]$ind$contrib

#Les joueurs se segmentent en deux dimensions distinctes
#dimension1 : stin:nombre de service gagnés au 1er coup, stwon: nombre de fois gagné le 1er point du 1er coup
#On a des joueurs qui sont très bons en service, tel que Bradley Klahn

#dimension2 : Ace:service gagnants,bpSaved:point break sauvé et bpFaced: point break gagné
#On a des joueurs qui assurent ds services gagnants, sauvent et gagnent les balles de matchs
#tel que Alessandro Gianessi 
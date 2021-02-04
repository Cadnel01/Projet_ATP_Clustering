### Library -------------------------------------------------------------------------

library(tidyverse)
library(factoextra)
library(FactoMineR)
library(NbClust)
library(dendextend)
library(fpc)
library(devtools)
#install_github("larmarange/JLutils")

### Importation ---------------------------------------------------------------------

players <- read_csv("tennis_atp/atp_players.csv", col_names = FALSE)
names(players) <- c("id", "firstname", "lastname", "hand", "birthday", "nat")

df <- read_csv("Tennis_data/atp_matches_2012.csv")


### Préparation de la base ----------------------------------------------------------

# Créer un tableau agrégé des gagnants
df_winner <- df  %>%
  group_by(winner_name) %>%
  summarise(w_ace = mean(w_ace, na.rm = TRUE),
            w_ht = winner_ht, 
            w_stIn = mean(w_1stIn, na.rm = TRUE),
            w_stWon = mean(w_1stWon, na.rm = TRUE),
            w_svgms = mean(w_SvGms, na.rm = TRUE),
            w_bpSaved = mean(w_bpSaved, na.rm = TRUE),
            w_bpFaced = mean(w_bpFaced, na.rm = TRUE),
            w_min = mean(minutes, na.rm = TRUE))
df_winner <- distinct(df_winner,.keep_all = TRUE)

# Créer un tableau agrégé des perdants
df_loser <- df  %>%
  group_by(loser_name) %>%
  summarise(l_ace = mean(l_ace, na.rm = TRUE),
            l_ht = loser_ht, 
            l_stIn = mean(l_1stIn, na.rm = TRUE),
            l_stWon = mean(l_1stWon, na.rm = TRUE),
            l_svgms = mean(l_SvGms, na.rm = TRUE),
            l_bpSaved = mean(l_bpSaved, na.rm = TRUE),
            l_bpFaced = mean(l_bpFaced, na.rm = TRUE),
            l_min = mean(minutes, na.rm = TRUE))
df_loser <- distinct(df_loser,.keep_all = TRUE)


# Donner le même nom du variable au "winner_name" et "loser_name"  
df_loser <- rename(df_loser, player = loser_name )
df_winner <- rename(df_winner, player = winner_name)

# Jointure des tables
df_player <- full_join(df_winner, df_loser, by = "player")

# Combinaison des variables
df_player <- mutate(df_player, ace = w_ace + l_ace,
                    height = case_when(w_ht == l_ht ~ w_ht, w_ht != l_ht ~ l_ht),
                    stin = w_stIn  + l_stIn,
                    stWon = w_stWon + l_stWon,
                    svgms = w_svgms + l_svgms,
                    bpSaved = w_bpSaved + l_bpSaved,
                    bpFaced = w_bpFaced + l_bpFaced,
                    min = l_min + w_min) %>%
            select(ace, height, stin, stWon, svgms, bpSaved, bpFaced, min)

# Supprimer les valeurs manquantes
df_clean <- drop_na(df_player)
df_clean <- as.data.frame(df_clean)
rownames(df_clean) <- df_clean[,1]
df_clean <- df_clean[,-1]

# Etude des corrélations
pairs(df_clean)

### k-means ----------------------------------------------------------------------

set.seed(123)
df_kmeans <- as.data.frame(scale(df_clean, center=T, scale=T))

## Choix du nombre de clusters ##

# Test K-means / Methode du coude: on veut maximiser inertie inter/intra
inertie.expl <- rep(0,times=10)
for (k in 2:11){
  km <- kmeans(df_kmeans, k)
  inertie.expl[k-1] <- km$betweenss/km$totss
}

plot(x = 1:10 ,y = inertie.expl, type="b", xlab="Nb. de groupes", ylab="% inertie expliquée")
#A partir de k = 4 classes, un cluster supplémentaire n’augmente pas 
#significativementla part d’inertie expliquée par la partition. 

sol.kmeans <- kmeansruns(df_kmeans, krange=2:10, criterion="ch")
plot(1:10,sol.kmeans$crit, type="b", xlab="Nb. de groupes",ylab="Silhouette")
#Point maximum à 4 clusters

nb <- NbClust(df_kmeans, distance = "euclidean", min.nc = 2, max.nc = 10, method = "kmeans")
fviz_nbclust(nb) #4 clusters


## Kmeans ##
set.seed(123)
km <- kmeans(df_kmeans, 4)
df_clean$classe_km <- km$cluster

# Taille des clusters
l = list()
DF <- df_clean
for (i in 1:4){
  l = append(l,nrow(filter(DF, classe_km == i)))
}
l

# Comparaison des coefficients de silhouette
si <- cluster::silhouette(km$cluster,dist(df_kmeans,"euclidean"))
plot(si)
factoextra::fviz_silhouette(si) + ggplot2::ylim(-1,1)

## Visualisation ##
fviz_cluster(km, geom="point", data = df_kmeans) + ggtitle("k=3")


### CAH  --------------------------------------------------------------------------------------------

df_CAH <- as.data.frame(scale(df_clean, center=T, scale=T))

## Choix du nombre de clusters ##

# Base bruite sans les noms et les classe. Passons desormais à la distance
liste <- c("ward.D", "single", "complete", "average" , "mcquitty" , "median" , "centroid", "ward.D2")

for (i in liste){
  D = dist(df_CAH,"euclidean")
  cah <- hclust(D,method = i)
  inertie <- sort(cah$height, decreasing = TRUE)
  plot(inertie[1:20], type = "s", xlab = "Nombre de clusters", ylab = "Inertie",main=paste0("Dendogramme ", i))
  plot(cah, main = paste0("Dendogramme avec la methode ", i))
  rect.hclust(cah, k = JLutils::best.cutree(cah))
  print(paste0("Cluster ", JLutils::best.cutree(cah)," pour la methode ",i))
} 
# propose 3 ou 4 clusters en fonction de la méthode

# Matrice des distances euclidiennes entre individus
D_cah = dist(df_CAH,"euclidean")

# Classification ascendante hiérarchique
CAH <- hclust(D_cah, method = "ward.D2")
plot(CAH)

# Comparaison 3 - 4 clusters avec CAH
clu_3 <- cutree(CAH,3)
clu_4 <- cutree(CAH,4)
si_cah_3 <- cluster::silhouette(clu_3, D_cah)
plot(si_cah_3)
si_cah_4 <- cluster::silhouette(clu_4, D_cah)
plot(si_cah_4)

# Choix de 4 clusters
df_CAH$classe <- clu_4
df_clean$classe_hc <- df_CAH$classe

# Afficher le dendogramme
plot(CAH)
rect.hclust(CAH, k = 4) 
CAH <- as.dendrogram(CAH)
CAH <- CAH %>%
  color_branches(k = 4) 
plot(CAH)

# Coefficients de silhouette
si_cah <- cluster::silhouette(df_CAH$classe, D_cah)
plot(si_cah)
factoextra::fviz_silhouette(si_cah) + ggplot2::ylim(-1,1)

## Visualisation
factoextra::fviz_cluster(list(data=df_CAH, cluster=clu_3),geom="point", data = df_CAH)
factoextra::fviz_cluster(list(data=df_CAH, cluster=clu_4),geom="point", data = df_CAH)


### Comparaison des repartitions entre les deux algos ----------------------------------------

## Similarité des partitions

clusteval::cluster_similarity(df_clean$classe_km, df_clean$classe_hc, similarity = "jaccard")
clusteval::cluster_similarity(df_clean$classe_km, df_clean$classe_hc, similarity = "rand")
#les deux partionnement sont très similaires

## Inerties

inertie_cor<-function(df,p=NULL){
  if (is.null(p)){ p <- rep(1,nrow(df))}
  g <- ( p %*% as.matrix(df) ) / sum(p)
  iner <- 0
  for (i in seq(nrow(df))){ iner <- iner + sum((df[i,] - g)^2) * p[i]   }
  return(iner)
}

inertie_inter_cor<-function(df,lab){
  M<-matrix(rep(0,ncol(df)*length(unique(lab))),ncol = ncol(df))
  for (k in unique(lab)){
    M[k,]<-unname(colMeans(df[which(lab==k),]))
  }
  return(inertie_cor(data.frame(M),unname(table(lab))))
}

inertie_intra_cor <- function(df,lab){
  res <- rep(0,length(unique(lab)))
  for (i in unique(lab)) {
    res[i] <- inertie_cor(df[which(lab==i),])
  }
  return(sum(res))
}

inertie_intra_cor(df_CAH,clu_3)
inertie_inter_cor(df_CAH,clu_3)

inertie_intra_cor(df_CAH,clu_4)
inertie_inter_cor(df_CAH,clu_4)

km$tot.withinss
km$betweenss

#plus petite inertie intra avec  (petite diff)
#plus grande inertie inter avec CAH (diff plus importante)


### Analyse de chaque clusters ----------------------------------------------------------------

### Analyse globale ###

df_kmeans %>% 
  ggplot()+geom_boxplot(mapping = aes(x = as.character(km$cluster), y = height,
                                      fill = as.character(km$cluster) ))

df_kmeans %>% 
  ggplot()+geom_boxplot(mapping = aes(x = as.character(km$cluster), y = ace,
                                      fill = as.character(km$cluster) ))


### Séparation des bases ###

df_CAH %>% filter(classe_hc == 1) -> Base1
Base1 <- Base1[,-c(9,10)]

df_CAH %>% filter(classe_hc == 2) -> Base2
Base2 <- Base2[,-c(9,10)]

df_CAH %>% filter(classe_hc == 3) -> Base3
Base3 <- Base3[,-c(9,10)]

df_CAH %>% filter(classe_hc == 4) -> Base4
Base4 <- Base4[,-c(9,10)]


### Cluster 1 ###

str(Base1)
summary(Base1)
colMeans(Base1) #connaitre les moyennes pour interpréter l'ACP
diag(cov(Base1)) #dispersion des ecarts types = var influente => ace, height, stin, stwon, min

#Etude des correlations
cor(Base1) #pas d'effet taille
plot(Base1)

#Realisation de l'ACP
acp <- PCA(Base1, scale.unit = T, graph = TRUE, ncp = 2, axes=c(1,2))

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

### Cluster 2 ###


### Cluster 3 ###

# Recuperation des joueurs de ce classeurs
cluster3 <- filter(df_clean, classe_hc == 3)

# Analyse de la taille
df_clean %>% 
  ggplot()+geom_boxplot(mapping = aes(x = as.character(df_clean$classe_hc), y = height,                                      fill = as.character(df_clean$classe_hc) ))

# Taille moyenne du classeur 3
mean(cluster3$height)

# Taille minimale du classeur 3
min(cluster3$height)

# Taille maximale du classeur 3
max(cluster3$height)

# Resultat: les joueurs dans ce classeur 3 ont des tr?s grandes tailles qui
# d?passent 1m92 par rapport aux joueurs des autres classeurs qui ont une
# taille moyenne qui ne d?passe pas 1m85

# Nombre de services gagnants par match

df_clean %>% 
  ggplot()+geom_boxplot(mapping = aes(x = as.character(df_clean$classe_hc), y = ace,
                                      fill = as.character(df_clean$classe_hc) ))

# Resultat: Les joueurs dans ce classeur 3 ont tendances ? jouer des
# bons services. Le nombre moyen des services gagn?s par match pour les
# joueurs de ce classeur est ?lev? par rapport aux autres 
# 18 services gagnants par match pour ce classeur tandis que cette moyenne
# ne d?passe pas 13 services chez les autres

a <- list()
for (i in 1:4){
  a <- append(a, mean(filter(df_clean, classe_hc==i)$ace))
}
a


# Nombre de services reussis du premier coup
df_clean %>% 
  ggplot()+geom_boxplot(mapping = aes(x = as.character(df_clean$classe_hc), y = stin,
                                      fill = as.character(df_clean$classe_hc) ))
b <- list()
for (i in 1:4){
  b <- append(b, mean(filter(df_clean, classe_hc==i)$stin))
}
b
# Resultat: Nombre de service reussis du premier coup au dessus de la moyenne
# et prend la deuxieme classe parmis les classeurs

# Nombre de fois ou le joueur a gagne le premier point d?s le premier service
df_clean %>% 
  ggplot()+geom_boxplot(mapping = aes(x = as.character(df_clean$classe_hc), y = stWon,
                                      fill = as.character(df_clean$classe_hc) ))

c <- list()
for (i in 1:4){
  c <- append(c, mean(filter(df_clean, classe_hc==i)$stWon))
}
c

# Resultat: Nombre de fois o? le joueur a gagne le premier point des le
# premier service est au-dessus de la moyenne (73 par match) 


### Resultats : 
# Les joueurs de ce classeur ont une grande taille, ce qui leurs donnent 
# un grand avantage dans leurs services


### Cluster 4 ###
head(Base4)
print(paste0("le 4ème cluster est celui est un cluster qui contient ",round(nrow(Base4)/nrow(df_clean)*100,2),"% observation Les joueurs les plus connus,dans ce cluster sont nottament, Heywitt, Monfils"))

for ( i in 1:ncol(Base4)){
  df_clean[,-2]
  col<-colnames(Base4)
  if (mean(Base4[,i])>mean(df_clean[,i])) {
    print(paste0("Moyenne nettement superieur à la base de donnée pour la variable ",col[i]))
  }
  else{
    print(paste0("Moyenne nettement inférieur à la base de donnée pour la variable ",col[i]))
  }
}
#en Général ces joueurs ont des statisitisques supperieur à la moyenne pour la variable Stin,svgms,Bpsaved,Bpfaced,min       
pairs(Base4)

ggplot(data=Base4,mapping = aes(x=bpFaced,y=bpSaved))+
  geom_point()+geom_smooth(method="lm")+
  ggtitle("Balle de Break Sauvé")

ggplot(data=Base4,mapping = aes(x=stWon,y=stin))+
  geom_point()+geom_smooth(method="lm")+
  ggtitle("Nombre de service gagnant")
#plus le joueurs fais un service, plus il gagne de jeu. Cela signifie alors que les joueurs de ce cluster ont une puisse de Frappe, enormes
#afin de gagner facilement sur les services.


for (i in col){
  boxplot(Base4[,i],main=paste0("Bôite a moustache de la variable ",i))
  print(paste0("la variance est de ",i ,' ',sqrt(sd(Base4[,i]))))
}
# les joueurs sont assez petit comparer à la base de de df_clean avec une moyenne de 1m85
#les écart à la moyenne sont assez faible, sauf pour min


#en Genéral ces joueurs sont d'autant plus présent lorsqu'il faut sauver des balles de break,
a<-c("Jan Hernych","Denis Gremelmayr")
a<-which(rownames(Base4)==a)
a<-FactoMineR::PCA(Base4,ncp=4,ind.sup = c(22,44))
explor::explor(a)
#Ces joueurs se caractérise vraiment sur le nombre de break sauvé, et sur le nombre de jeu gagné après le service.Par ailleurs l'analyse a monter que les jouait beaucou plus de temps.
#par ailleurs ces joueurs se distingue aussi au niveau du service, il existe une corélation entre le nombre de serice in et le nombre de service gagné.( Présence de Nadal).
# l'analyse a montre que les joueurs suivant Hernych, Denis Gremelmayr sont pas dans le bon groupe. 

res <- explor::prepare_results(a)
explor::PCA_ind_plot(res, xax = 1, yax = 2, ind_sup = TRUE, lab_var = NULL,
                     ind_lab_min_contrib = 0, col_var = NULL, labels_size = 9, point_opacity = 0.5,
                     opacity_var = NULL, point_size = 64, ellipses = FALSE, transitions = TRUE,
                     labels_positions = NULL, xlim = c(-5.57, 5.09), ylim = c(-4.98, 5.68)
                     #le cluster 4 peut donc distinguer des joueurs avec un style de jeu assez deffensive( tels monfils) mais aussi avec des frappe de l'espace( Nadal GANG)
                     



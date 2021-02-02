### Library -------------------------------------------------------------------------

library(tidyverse)
library(factoextra)
library(NbClust)
library(dendextend)
library(fpc)
library(devtools)
#install_github("larmarange/JLutils")

### Importation ---------------------------------------------------------------------

players <- read_csv("tennis_atp/atp_players.csv", col_names = FALSE)
names(players) <- c("id", "firstname", "lastname", "hand", "birthday", "nat")

df <- read_csv("tennis_atp/atp_matches_2012.csv")


### Préparation de la base ----------------------------------------------------------
# Créer un tableau agrégé des gagnants
df_winner <- df  %>%
  group_by(winner_name) %>%
  summarise(w_ace = mean(w_ace, na.rm = TRUE),
            w_ht = winner_ht, 
            w_df = mean(w_df, na.rm = TRUE),
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
            l_df = mean(l_df, na.rm = TRUE),
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
                    dblef = w_df + l_df,
                    height = case_when(w_ht == l_ht ~ w_ht, w_ht != l_ht ~ l_ht),
                    stin = w_stIn  + l_stIn,
                    stWon = w_stWon + l_stWon,
                    svgms = w_svgms + l_svgms,
                    bpSaved = w_bpSaved + l_bpSaved,
                    bpFaced = w_bpFaced + l_bpFaced,
                    min = l_min + w_min) %>%
            select(ace,  dblef, height, stin, stWon, svgms, bpSaved, bpFaced, min)

# Supprimer les valeurs manquantes
df_clean <- drop_na(df_player)

# Etude des corrélations
pairs(df_clean[,2:10])

### k-means ----------------------------------------------------------------------

set.seed(123)
df_kmeans <- scale(df_clean[,2:10], center=T, scale=T)

## Choix du nombre de clusters ##

# Test K-means / Methode du coude: on veut maximiser inertie inter/intra
inertie.expl <- rep(0,times=10)
for (k in 2:11){
  km <- kmeans(df_kmeans, k)
  inertie.expl[k-1] <- km$betweenss/km$totss
}

plot(x=1:10 ,y=inertie.expl,type="b",xlab="Nb. de groupes",ylab="% inertie expliquée")
#A partir de k = 4 classes, un cluster supplémentaire n’augmente pas 
#significativementla part d’inertie expliquée par la partition. 

sol.kmeans <- kmeansruns(df_kmeans,krange=2:10,criterion="ch")
plot(1:10,sol.kmeans$crit, type="b", xlab="Nb. de groupes",ylab="Silhouette")
#Point maximum à 4 clusters

nb <- NbClust(df_kmeans, distance = "euclidean", min.nc = 2, max.nc = 10, method = "kmeans")
fviz_nbclust(nb) #4 clusters


## Kmeans ##

km <- kmeans(df_kmeans, 4)
df_clean$classe_km <- km$cluster

# Taille des clusters
l=list()
for (i in 1:4){
  l = append(l,nrow(filter(df_kmeans, classe_km == i)))
}

# Comparaison des coefficients de silhouette
si <- cluster::silhouette(km$cluster,dist(df_kmeans,"euclidean"))
plot(si)
factoextra::fviz_silhouette(si) + ggplot2::ylim(-1,1)

## Visualisation ##
fviz_cluster(km, geom="point", data = df_kmeans) + ggtitle("k=4")


### CAH  -------------------------------------------------------------------------

df_CAH <- scale(df_clean[,2:10], center=T, scale=T)

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
#3 clusters

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
df_CAH$joueur <- df_clean$player

# Afficher le dendogramme
rect.hclust(CAH, k = 4) 
CAH <- as.dendrogram(CAH)
CAH <- CAH %>%
  color_branches(k = 4) 
plot(CAH)

df_clean$classe_hc <- df_CAH$classe

for (i in 1:ncol(df_CAH)){
while (is.numeric(df_CAH[,i])){
  print(colnames(df_CAH[,i]))
  i=i+1}
}

# Coefficients de silhouette
si_cah <- cluster::silhouette(df_CAH$classe, D_cah)
plot(si_cah)
factoextra::fviz_silhouette(si_cah) + ggplot2::ylim(-1,1)

## Visualisation
factoextra::fviz_cluster(list(data=df_CAH, cluster=clu_4), data = df_CAH)


### Comparaison des repartitions entre les deux algos ----------------------------------------

clusteval::cluster_similarity(df_clean$classe_km, df_clean$classe_hc, similarity = "jaccard")
clusteval::cluster_similarity(df_clean$classe_km, df_clean$classe_hc, similarity = "rand")

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

inertie_intra_cor(df_CAH,clu_4)
inertie_inter_cor(df_CAH,clu_4)

km$tot.withinss
km$betweenss

#meilleurs résulats avec kmeans (plus petit inertie intra et plus grande inertie inter)


### Analyse de chaque clusters -------------------------------------

### Analyse globale ###

df_kmeans %>% 
  as.data.frame(df_kmeans) %>%
  ggplot()+geom_boxplot(mapping = aes(x = as.character(km$cluster), y = height,
                                      fill = as.character(km$cluster) ))

df_kmeans %>% 
  as.data.frame(df_kmeans) %>%
  ggplot()+geom_boxplot(mapping = aes(x = as.character(km$cluster), y = ace,
                                      fill = as.character(km$cluster) ))

### Séparation en 4 bases ###

df_clean %>% filter(classe_km == 1) -> Base1
Base1 <- Base1[,-11]
df_clean %>% filter(classe_km == 2) -> Base2
Base2 <- Base2[,-11]
df_clean %>% filter(classe_km == 3) -> Base3
Base3 <- Base3[,-11]
df_clean %>% filter(classe_km == 4) -> Base4
Base4 <- Base4[,-11]

### Cluster 1

### Cluster 2

### Cluster 3

### Cluster 4

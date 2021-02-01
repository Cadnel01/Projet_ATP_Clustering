### Library ----------------------------------------------------

library(tidyverse)
library(dplyr)

### Importation ------------------------------------------------

players <- read_csv("Tennis_data/atp_players.csv", col_names = FALSE)
names(players) <- c("id", "firstname", "lastname", "hand", "birthday", "nat")

df <- read_csv("Tennis_data/atp_matches_2012.csv")

# Créer un tableau agrégé des gagnants
df_winner <- df  %>%
  group_by(winner_name, winner_ht) %>%
  summarise(w_ace = mean(w_ace, na.rm = TRUE),
            w_df = mean(w_df, na.rm = TRUE),
            w_stIn = mean(w_1stIn, na.rm = TRUE),
            w_stWon = mean(w_1stWon, na.rm = TRUE),
            w_ndWon = mean(w_2ndWon, na.rm = TRUE),
            w_svgms = mean(w_SvGms, na.rm = TRUE),
            w_bpSaved = mean(w_bpSaved, na.rm = TRUE),
            w_bpFaced = mean(w_bpFaced, na.rm = TRUE),
            w_min = mean(minutes, na.rm = TRUE)
            )

# Créer un tableau agrégé des perdants
df_loser <- df  %>%
  group_by(loser_name, loser_ht) %>%
  summarise(l_ace = mean(l_ace, na.rm = TRUE),
            l_df = mean(l_df, na.rm = TRUE),
            l_stIn = mean(l_1stIn, na.rm = TRUE),
            l_stWon = mean(l_1stWon, na.rm = TRUE),
            l_ndWon = mean(l_2ndWon, na.rm = TRUE),
            l_svgms = mean(l_SvGms, na.rm = TRUE),
            l_bpSaved = mean(l_bpSaved, na.rm = TRUE),
            l_bpFaced = mean(l_bpFaced, na.rm = TRUE),
            l_min = mean(minutes, na.rm = TRUE))

# Donner le même nom du variable au "winner_name" et "loser_name"  
df_loser <- rename(df_loser, player = loser_name )
df_winner <- rename(df_winner, player = winner_name)

# Jointure des tables
df_player <- full_join(df_winner, df_loser, by = "player")

# Combinaison des variables
df_player <- mutate(df_player, ace = w_ace + l_ace,
                    dblef = w_df + l_df,
                    stin = w_stIn  + l_stIn,
                    stWon = w_stWon + l_stWon,
                    ndWon = w_ndWon + l_ndWon,
                    svgms = w_svgms + l_svgms,
                    bpSaved = w_bpSaved + l_bpSaved,
                    bpFaced = w_bpFaced + l_bpFaced,
                    min = l_min + w_min)
df_player <- select(df_player, winner_ht, ace,  dblef, stin,
                    stWon, ndWon, svgms, bpSaved, bpFaced, min)

# Supprimer les valeurs manquantes
df_clean <- drop_na(df_player)

# Etude des corrélations
pairs(df_clean[,2:11])

# Test K-means / Méthode du coude
inertie.expl <- rep(0,times=6)
for (k in 2:6){
  km <- kmeans(df_clean[,2:11], k)
  inertie.expl[k] <- km$betweenss/km$totss
}

ggplot()+geom_smooth(mapping = aes(x = 1:6, y = inertie.expl ))


set.seed(123)
km <- kmeans(scale(df_clean[,2:11]), 4)
df_clean$classe = km$cluster

# Taille des classeurs
l=list()
for (i in 1:4){
  l = append(l,nrow(filter(df_clean, classe == i)))
}
l

# Analyse des contenus de chaque classeurs
df_clean %>% 
  ggplot()+geom_boxplot(mapping = aes(x = as.character(classe), y = winner_ht,
                                      fill = as.character(classe) ))

df_clean %>% 
  ggplot()+geom_boxplot(mapping = aes(x = as.character(classe), y = ace,
                                      fill = as.character(classe) ))


# CAH  -----------------------------------------------------------
df_CAH=df_clean[,c(-1,-12)]
#base bruite sans les noms et les classe. Passons dÃ©sormais à la distance
liste <- c("ward.D", "single", "complete", "average" , "mcquitty" , "median" , "centroid", "ward.D2")

for ( i in liste){
D=dist(df_CAH,"euclidean")
cah<-hclust(D,method = i)
inertie <- sort(cah$height, decreasing = TRUE)
plot(inertie[1:20], type = "s", xlab = "Nombre de classes", ylab = "Inertie",main=paste0("Dendogramme ", i))
plot(cah, main=paste0("Dendogramme avec la methode ", i))
rect.hclust(cah,k=JLutils::best.cutree(cah))
print(paste0("classe ", JLutils::best.cutree(cah)," pour la methode ",i))

}
# Le ward.d2 donnes un cluster de 4
Cut<-cutree(cah,4)
df_CAH$classe<-Cut
df_CAH$joueur<-df_clean$player

for (i in 1:ncol(df_CAH)){
while (is.numeric(df_CAH[,i])){
  print(colnames(df_CAH[,i]))
  i=i+1}
}

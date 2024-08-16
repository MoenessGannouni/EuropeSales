data<- read.csv("C:/Users/MOEµNESS/OneDrive - ESPRIT/Bureau/EuropeSales/europesales.csv")
head(data)
#on veut avoir une idée générale sur notre dataset
summary(data)
str(data)
# Vérifier les valeurs manquantes par colonne
colSums(is.na(data))
# On remarque qu'on a pas des valeurs manquantes

# Examiner les variables numériques pour détecter les outliers
numeric_vars <- sapply(data, is.numeric)
numeric_data <- data[, numeric_vars]
# Visualiser les boxplots pour les variables numériques
boxplot(numeric_data)
# Calculer les z-scores pour chaque variable numérique
z_scores <- scale(numeric_data)
# Identifier les indices des outliers (z-score > 3 ou z-score < -3)
outlier_indices <- which(abs(z_scores) > 3, arr.ind = TRUE)
outlier_indices
# Extraire les indices des outliers
outlier_indices <- cbind(outlier_indices[, 1], outlier_indices[, 2])
# Afficher les valeurs des outliers dans votre dataframe
outliers_data <- data[outlier_indices[, 1], ]
outliers_data
#On va prendre quelques-unes des valeurs spécifiques des outliers détectés dans ces colonnes
# Indices des outliers détectés
outlier_indices <- cbind(c(72, 95, 296, 350, 379), c(3, 3, 3, 3, 3))  # Exemple d'indices des outliers
# Afficher les valeurs des outliers dans votre dataframe
outliers_data <- data[outlier_indices[, 1], ]
outliers_data

# Définir les limites acceptables pour chaque colonne
acceptable_limit_revenue <- quantile(data$Total.Revenue, 0.95)# Limite supérieure à 95e percentile
acceptable_limit_cost <- quantile(data$Total.Cost, 0.95) # Limite supérieure à 95e percentile
acceptable_limit_profit <- quantile(data$Total.Profit, 0.95)   # Limite supérieure à 95e percentile
# Remplacer les valeurs aberrantes par la médiane
data$Total.Revenue[data$Total.Revenue > acceptable_limit_revenue] <- median(data$Total.Revenue, na.rm = TRUE)
data$Total.Cost[data$Total.Cost > acceptable_limit_cost] <- median(data$Total.Cost, na.rm = TRUE)
data$Total.Profit[data$Total.Profit > acceptable_limit_profit] <- median(data$Total.Profit, na.rm = TRUE)
# Vérifier les statistiques mises à jour
summary(data[c("Total.Revenue", "Total.Cost", "Total.Profit")])


# Sélection de la colonne "Order.Priority"
order_priority <- data$Order.Priority
# Conversion de la colonne en facteur numérique
order_priority_numeric <- as.numeric(as.factor(order_priority))

# Préparation des données pour le clustering
data_for_clustering <- data.frame(Order_Priority_Numeric = order_priority_numeric)
# Exécution de K-means
set.seed(123)  # Fixation de la graine aléatoire pour la reproductibilité
kmeans_model <- kmeans(data_for_clustering, centers = 4)  # Nombre de clusters à identifier
# Résumé du modèle K-means
print(kmeans_model)

install.packages("factoextra")
library(factoextra)  # Pour les visualisations

# Visualisation des clusters avec les centres
fviz_cluster(kmeans_model, data = data_for_clustering, geom = "point", stand = FALSE, ellipse.type = "convex")

# Ajouter les étiquettes des centres des clusters
fviz_cluster(kmeans_model, data = data_for_clustering, geom = "point", stand = FALSE, ellipse.type = "convex") +
  geom_text(aes(label = paste("Cluster", cluster)), size = 4, vjust = -1)

# Personnaliser les graphiques
# Exemple : couleur et forme des points, couleur des ellipses
fviz_cluster(kmeans_model, data = data_for_clustering, geom = "point", stand = FALSE, ellipse.type = "convex",
             pointsize = 3, palette = "jco", ggtheme = theme_minimal()) +
  geom_text(aes(label = paste("Cluster", cluster)), size = 4, vjust = -1)


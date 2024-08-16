data2<- read.csv("C:/Users/MOEµNESS/OneDrive - ESPRIT/Bureau/EuropeSales/europesales.csv")
head(data2)
#on veut avoir une idée générale sur notre dataset
summary(data2)
str(data2)
# Vérifier les valeurs manquantes par colonne
colSums(is.na(data2))
# On remarque qu'on a pas des valeurs manquantes

# Examiner les variables numériques pour détecter les outliers
numeric_vars <- sapply(data, is.numeric)
numeric_data <- data2[, numeric_vars]
# Visualiser les boxplots pour les variables numériques
boxplot(numeric_data)

# Convertir Order.Priority en valeurs numériques
data$Order.Priority <- as.factor(data$Order.Priority)
data$Order.Priority <- as.numeric(data$Order.Priority)

# Sélection des variables pour le clustering
data_for_clustering <- data[, c("Order.Priority", "Unit.Price", "Total.Revenue", "Total.Cost")]
# Standardisation des données
scaled_data <- scale(data_for_clustering)  # Standardisation des données
# Application de K-means avec 4 clusters
set.seed(123)  # Pour la reproductibilité
kmeans_result <- kmeans(scaled_data, centers = 4)
# Affichage des résultats
print(kmeans_result)

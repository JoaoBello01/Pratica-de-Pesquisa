library(daltoolbox)
library(tidyverse)
library(arulesCBA)
library(arulesSequences)
library(arulesViz)

#1a)
media<- lapply(wine.data, mean) %>% as_tibble()
desvio_padrão <- lapply(wine.data, sd) %>% as_tibble()

#1b)
media_classe <- wine.data %>% group_by(Class) %>% summarise_all(mean)

desvio_classe <- wine.data %>% group_by(Class) %>% summarise_all(sd)

#1c)

atributos_2 <- c("Alcohol","Malic_acid","Ash", "Alcalinity_of_ash", "Magnesium", "Total_phenols", "Flavanoids", "Nonflavanoid_phenols", "Proanthocyanins",
                 "Color_intensity", "Hue", "OD280.OD315_of_diluted_wines", "Proline")

for (i in length(atributos_2)) {
  densidade_por_classe <- plot_density_class(select(wine.data,atributos_2[i], Class), class = "Class")
  plot(densidade_por_classe)
  
}

plot_density_class(select(wine.data, Alcohol, Class), class = "Class")

ggplot(wine.data, aes(Alcohol, colour = Class)) +
  geom_density()

#1d) 

atributos_2 <- c("Alcohol","Malic_acid","Ash", "Alcalinity_of_ash", "Magnesium", "Total_phenols", "Flavanoids", "Nonflavanoid_phenols", "Proanthocyanins",
                 "Color_intensity", "Hue", "OD280.OD315_of_diluted_wines", "Proline")

for (i in length(atributos_2)) {
  boxplot_por_classe <- plot_boxplot_class(select(wine.data,atributos_2[i], Class), class = "Class")
  plot(boxplot_por_classe)
  
}


#1e) 

combinacoes <- combn(length(colnames(wine.data)), 2) 

for (i in 1:ncol(combinacoes)) {
  atributo1 <- wine.data[, combinacoes[1, i]]
  atributo2 <- wine.data[, combinacoes[2, i]]
  
  nome_atributo1 <- colnames(wine.data)[combinacoes[1, i]]
  nome_atributo2 <- colnames(wine.data)[combinacoes[2, i]]
  
  scatter<- plot_scatter(wine.data %>% select(x = nome_atributo1, value = nome_atributo2),
                         label_x = nome_atributo1, label_y = nome_atributo2)
  plot(scatter)
}

#2a)

atributos <- colnames(select(wine.data, -1))

criar_categorias <- function(atributo, num_categorias = 3) {
  min_valor <- min(wine.data[[atributo]])
  max_valor <- max(wine.data[[atributo]])
  cortes <- (max_valor - min_valor) / num_categorias
  
  lev <- cut(wine.data[[atributo]], breaks = c(min_valor, min_valor + cortes, min_valor + 2*cortes, max_valor), ordered = TRUE)
  levels(lev) <- c("baixa", "média", "alta")
  return(lev)
}

categorias_dataframe <- as.data.frame(lapply(atributos, criar_categorias))
colnames(categorias_dataframe) <- atributos
categorias_por_atributo <- as_tibble(categorias_dataframe)

#2b)
lev <- cut(wine.data$Class, breaks = 0:3, ordered = TRUE)
levels(lev) <- c("baixa", "média", "alta")
wine.data_factor <- wine.data %>% mutate(Class = lev)

cm <- categ_mapping("Class")
wine.data_cm <- transform(cm, wine.data_factor)

#5
wine.data_disc<- discretizeDF.supervised(Class ~., wine.data_factor)
attributes(wine.data_disc$Alcohol)
Wine.data_trans <- as(wine.data_disc, "transactions")

rules <- apriori(Wine.data_trans, parameter = list(supp = 0.5, conf = 0.9, target = "rules")) 
inspect(rules)
rules_a <- as(rules, "data.frame")

imrules <- interestMeasure(rules, transactions = Wine.data_trans)

plot(rules, engine = "ggplot2", main = NULL) + 
  scale_color_gradient2(low = "red", mid = "gray90", high = "blue", 
                        midpoint = 1.16, limits = c(1.03,1.30)) +
  labs(x = "Supp.", y = "Conf.", color = "Lift") + 
  theme_classic()


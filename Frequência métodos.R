#library(tidyverse)


library(RColorBrewer)
library(ggplot2)

dados <- c("D-A", "D-B", "D", "D-A", "D-A", "D-A", "A-C", "D-A-C", "C", "D-A-C", "D-B-C", "D-A",
           "D-A", "D-A", "D-A-B", "D-B-C", "D-B", "D-A-B", "A-C", "D-A", "D-B", "D-A", "D-A",
           "D-A", "D-A", "D-A", "D", "D", "D-A-B", "A", "A-B", "D-A-B", "D-A", "D-A-C", "D-A",
           "C", "D-A", "D-A", "D-B-C", "D-C", "D-B", "D-A-B", "D-B", "D-A-C", "D-B", "D-A-B-C",
           "D-C", "D-A", "B", "D", "D-A", "D-C", "D-C")


# Contagem de frequência de cada categoria
freq_count <- table(dados)

# Converter em um data.frame e ordenar por frequência em ordem decrescente
dados_df <- data.frame(Categoria = names(freq_count), Frequencia = as.numeric(freq_count))
dados_df <- dados_df[order(dados_df$Frequencia, decreasing = TRUE), ]

# Reordenar os fatores na variável "dados" para refletir a ordem de frequência
dados <- factor(dados, levels = dados_df$Categoria)

# Criar o gráfico de barras ordenado
ggplot(data = data.frame(dados)) +
  geom_bar(mapping = aes(x = dados, fill = dados), color = "black") +
  labs(title = "", x = "Methods", y = "Frequency") +
  scale_y_continuous(breaks = seq(0, 20, 2)) +
  scale_fill_brewer(palette = "Pastel1") +
  guides(fill = FALSE) +
  theme_minimal() +
  theme(axis.text = element_text(family = "Arial", size = 12, colour = "black"),
        axis.title = element_text(colour = "black"),
        text = element_text(family = "Arial", size = 12))





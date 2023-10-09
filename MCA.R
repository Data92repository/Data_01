#install.packages("tidyverse")
#install.packages("FactoMineR")
#install.packages("factoextra")
#install.packages("ggrepel")

library(tidyverse)
library(FactoMineR)
library(factoextra)
library(ggrepel)



pdca <- dados %>% select(1,2,3,4) 


#view(pdca)





pdca <- na.omit(pdca)


res.mca <- MCA(pdca)


# carregar o pacote FactoMineR
library(FactoMineR)

# executar o MCA
res.mca <- MCA(pdca)

# obter as coordenadas das colunas para as duas primeiras dimensões
dimdesc(res.mca, axes = 1:2)$col$coord


####


get_mca_var(res.mca)

fviz_mca_var(res.mca, repel = T)



# Change the theme and use only points
fviz_mca_ind(res.mca, col.ind="cos2", geom = "point") +
  scale_color_gradient2(low="white", mid="blue",
                        high="red", midpoint=0.4)+ theme_minimal()



# Color by the contributions
fviz_mca_ind(res.mca, col.ind="contrib",  repel = T) +
  scale_color_gradient2(low="white", mid="blue",
                        high="red", midpoint=1.5)



# escolher um fator exemplo coluna n, colocar o nome da coluna

grupo <- as.factor(pdca$`Ambiente`)


grupo


# Change the color by groups, add ellipses 2

p <- fviz_mca_biplot(res.mca, col.ind = "cos2", label="var",ggtheme=theme(axis.title=element_text(size=5, face="bold")), 
                col.var ="black", habillage=grupo, addEllipses=TRUE, ellipse.level=0.95, repel = T) +  theme_minimal()
p





# Realizar a análise MCA
res.mca <- MCA(dados)

# Acessar a matriz de valores de contribuição das colunas
contribuicao <- res.mca$var$contrib

# Criar o data.frame
tabela <- data.frame(Nome = rownames(contribuicao),
                     Contribuição = apply(contribuicao, 1, max))

# Ordenar a tabela pela coluna "Contribuição" em ordem decrescente
tabela_ordenada <- tabela[order(-tabela$Contribuição), ]

# Imprimir a tabela ordenada
print(tabela_ordenada)



# Carregar a biblioteca FactoMineR para realizar a análise MCA
library(FactoMineR)

# Realizar a análise MCA
res.mca <- MCA(dados)

# Acessar a matriz de valores de contribuição das colunas
contribuicao <- res.mca$var$contrib

# Criar o data.frame
tabela <- data.frame(Nome = rownames(contribuicao),
                     Contribuição = apply(contribuicao, 1, max))

# Ordenar a tabela pela coluna "Contribuição" em ordem decrescente
tabela_ordenada <- tabela[order(-tabela$Contribuição), ]

# Formatar a coluna "Contribuição" com no mínimo 3 casas decimais
tabela_ordenada$Contribuição <- format(tabela_ordenada$Contribuição, nsmall = 3)

# Imprimir a tabela ordenada
print(tabela_ordenada)



####
p + scale_color_manual(values=c("#3B66F1", "#92D050", "#FF3737")) + scale_fill_manual(values=c("#3B66F1", "#92D050", "#FF3737")) 
+ theme(axis.text.x = element_text(size=100))
#



# Change the color by groups, add ellipses
#fviz_mca_biplot(res.mca, label="var",   font.submain  = 10, col.var ="blue",
 #               habillage=grupo, addEllipses=TRUE, ellipse.level=0.95) +
  #theme_minimal()






#Error in parse(text = x) : :1:9: unexpected symbol 1: Opinion on_courses_needed ^

# Change the color by groups, add ellipses
fviz_mca_biplot(res.mca, label="none", col.var ="blue",
                habillage=grupo, addEllipses=TRUE, ellipse.level=0.95) +
  theme_minimal() + labs(fill = "Species", color = "var", alpha = "var") 





fviz_mca_biplot(res.mca, label="none", col.var ="blue", habillage=grupo,
                addEllipses=TRUE, ellipse.level=0.95, ellipse.type = "confidence") +
  scale_color_discrete(name = "var") +
  scale_fill_discrete(name = "Species") +
  theme_minimal()



#+ labs(habillage=none, fill = "Caracteristicas", label="var")


fviz_mca_biplot(res.mca,
             geom.ind = "point", # show points only (nbut not "text")
             col.ind = iris$Species, # color by groups
             palette = c("#00AFBB", "#E7B800", "#FC4E07"),
             addEllipses = TRUE, # Concentration ellipses
             legend.title = "grupo")



# Color individuals by groups

fviz_mca_ind(res.mca, label="none", habillage=grupo) 

####V?deo Tutorial R | Analise de correspond?ncia m?ltipla ACM ou MCA no R

fviz_mca_biplot(res.mca, habillage = grupo, addEllipses = T, repel = T)


###

p <- fviz_mca_ind(res.mca, label="none", habillage=grupo,
                  addEllipses=TRUE, ellipse.level=0.95)
print(p)

# Change group colors using RColorBrewer color palettes
p + scale_color_brewer(palette="Dark2") +
  theme_minimal()


p + scale_color_brewer(palette="Paired") +
  theme_minimal()

#o tamanho da elipse de concentra??o na probabilidade normal.
#valor l?gico. Se TRUE, desenha elipses ao redor dos indiv?duos quando habillage! = "Nenhum"



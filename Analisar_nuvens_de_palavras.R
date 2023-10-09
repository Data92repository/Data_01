#install.packages("tidyverse")
#install.packages("tidytext")
#install.packages("textreadr")
#install.packages("tm")
#install.packages("wordcloud")
#install.packages("igraph")
#install.packages("ggraph")
#install.packages("ggplot2")
#install.packages("dplyr")
#install.packages("pdftools")
#install.packages("RRPP")
#install.packages("Rcpp")


# Pacotes e Fun??es
library(tidyverse) # Manipula??o eficiente de dados
library(tidytext) # Manipula??o eficiente de texto
library(pdftools) # Leitura de PDF para texto
library(tm) # Pacote de minera??o de texto com stopwords
library(wordcloud) # Gr?fico nuvem de palavras
library(igraph)
library(ggraph)
library(ggplot2)
library(dplyr)
library(RRPP)

# Fun??o para normalizar texto
NormalizaParaTextMining <- function(texto) {
  texto %>%
    chartr(
      old = "????????????????????????????????????????????????Ǵ`^~?:.!?&$@#0123456789",
      new = "aaaaaAAAAAeeeeEEEEiiiiIIIIoooooOOOOOuuuuUUUUyyYcC                         ",
      x = .
    ) %>% # Elimina acentos e caracteres desnecess?rios
    str_squish() %>% # Elimina espa?os excedentes
    tolower() %>% # Converte para min?sculo
    return() # Retorno da fun??o
}

# Lista de palavras para remover
palavrasRemover <- c(stopwords(kind = "pt"), letters, "?", "t?", "musica") %>%
  as_tibble() %>% 
  rename(Palavra = value) %>% 
  mutate(Palavra = NormalizaParaTextMining(Palavra))

# Arquivo PDF
arquivoPdf <- "C:/Users/Vinicius/Documents/R/Adaptação de métodos de avaliação em cartografia/palavras_grupos_2.pdf"

TidyT <- pdftools::pdf_text(arquivoPdf) %>% 
  as_tibble() %>% 
  rename(text = value)

CleanW <- TidyT  %>% 
  unnest_tokens(Palavra, text) %>% 
  mutate(Palavra = NormalizaParaTextMining(Palavra)) %>% 
  anti_join(palavrasRemover)

frequenciaPalavras <- CleanW %>% 
  count(Palavra, sort = TRUE) %>% 
  filter(Palavra != "") %>%
  arrange(desc(n)) 

# Visualiza frequ?ncia de palavras
DT::datatable(frequenciaPalavras)




 frequenciaPalavras %>%
  arrange(desc(n)) %>%
  head(n = 10) %>%
  ggplot(aes(reorder(Palavra, n), n)) +
  geom_col(color = "black", fill = "#87CEFA") +
  geom_text(aes(label = n), hjust = -0.1, size = 3) + 
  coord_flip() + 
  labs(title = "", x = "", y = "") + theme_bw()



 frequenciaPalavras %>%
   arrange(desc(n)) %>%
   head(n = 10) %>%
   ggplot(aes(reorder(Palavra, n), n)) +
   geom_col(color = "black", fill = "#87CEFA") +
   geom_text(aes(label = n), hjust = -0.1, size = 4) + 
   coord_flip() + 
   labs(title = "", x = "", y = "") + 
   theme_bw() +
   theme(axis.text.x = element_text(color = "black"),
         axis.text.y = element_text(color = "black"))
 

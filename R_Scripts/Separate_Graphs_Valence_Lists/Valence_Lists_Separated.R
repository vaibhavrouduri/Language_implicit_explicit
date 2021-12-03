## Graphs for each Valence list
library(readxl)
library(tidyverse)
embeddings.5 <- read_excel("C:/Users/Vaibhav/Desktop/Thesis work/Final Work/WEAT_across_embeddings_across_valenced_lists.xlsx", 
                           sheet = "All Possible")
embeddings.5.1 <- embeddings.5
embeddings.5.1 <- embeddings.5.1 %>% dplyr::rename("Embedding" = "...1")

embeddings.5.1 <- gather(embeddings.5.1, key = "ValenceList", value = "WEAT", 
                         'Valenced 8 D Value')

embeddings.5.1$ValenceList <-  extract_numeric(embeddings.5.1$ValenceList)

'%!in%' <- function(x,y)!('%in%'(x,y))

embeddings.5.1 <- embeddings.5.1 %>% filter(!is.na(embeddings.5.1$Embedding), 
                                            Embedding %!in% c("European American - Good/African American - Bad",
                                                              "Simple - Good/Difficult - Bad",
                                                              "Abstaining - Good/Drinking - Bad",
                                                              "White - Good/Asian - Bad",
                                                              "Young People - Good/Old People - Bad"))

#Separate embeddings
embeddings.5.1 %>%
  filter(ValenceList != "NA") %>%
  ggplot(aes(ValenceList, WEAT, col = Embedding)) + geom_point() + theme(legend.position = "bottom") + 
  theme_bw() +
  facet_grid(Embedding~IAT) +
  geom_hline(yintercept = 0, col = "red", lty = 3) +
  scale_x_continuous(breaks = c(8))

#Combined embeddings
embeddings.5.1 %>%
  filter(ValenceList != "NA") %>%
  ggplot(aes(ValenceList, WEAT, col = Embedding)) + geom_point() + 
  theme_bw() +
  theme(legend.position = "bottom") + 
  facet_grid(~IAT) +
  geom_hline(yintercept = 0, col = "red", lty = 3) +
  scale_x_continuous(breaks = c(8))



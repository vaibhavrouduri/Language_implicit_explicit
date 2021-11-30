library(readxl)
library(tidyverse)
embeddings.5 <- read_excel("C:/Users/Vaibhav/Desktop/Thesis work/Final Work/WEAT_across_embeddings_across_valenced_lists.xlsx", 
                           sheet = "All Possible")

embeddings.5 <- embeddings.5 %>% dplyr::rename("Embedding" = "...1")

embeddings.5.long <- gather(embeddings.5, key = "ValenceList", value = "WEAT", 
                            'Valenced 1 D Value', 'Valenced 2 D Value', 'Valenced 3 D Value', 'Valenced 4 D Value',
                            'Valenced 5 D Value', 'Valenced 6 D Value', 'Valenced 7 D Value', 
                            'Valenced 8 D Value')

embeddings.5.long$ValenceList <-  extract_numeric(embeddings.5.long$ValenceList)

'%!in%' <- function(x,y)!('%in%'(x,y))

embeddings.5.long <- embeddings.5.long %>% filter(!is.na(embeddings.5.long$Embedding), 
                                                  Embedding %!in% c("European American - Good/African American - Bad",
                                                                    "Simple - Good/Difficult - Bad",
                                                                    "Abstaining - Good/Drinking - Bad",
                                                                    "White - Good/Asian - Bad",
                                                                    "Young People - Good/Old People - Bad"))

## Separate Embeddings
embeddings.5.long %>%
  filter(ValenceList != "NA") %>%
  ggplot(aes(ValenceList, WEAT, col = Embedding)) + geom_point() + theme(legend.position = "bottom") + 
  theme_bw() +
  facet_grid(Embedding~IAT) +
  geom_hline(yintercept = 0, col = "red", lty = 3) +
  scale_x_continuous(breaks = c(1, 2, 3, 4, 5, 6, 7, 8, 9))

## Combined Embeddings
embeddings.5.long %>%
  filter(ValenceList != "NA") %>%
  ggplot(aes(ValenceList, WEAT, col = Embedding)) + geom_point() + 
  theme_bw() +
  theme(legend.position = "bottom") + 
  facet_grid(~IAT) +
  geom_hline(yintercept = 0, col = "red", lty = 3) +
  scale_x_continuous(breaks = c(1, 2, 3, 4, 5, 6, 7, 8, 9))

## Continuous_Combined_embeddings
embeddings.5.long %>%
  filter(ValenceList != "NA") %>%
  ggplot(aes(ValenceList, WEAT, col = Embedding)) + geom_density(stat = 'identity') + 
  theme_bw() +
  theme(legend.position = "bottom") + 
  facet_grid(~IAT) +
  geom_hline(yintercept = 0, col = "red", lty = 3) +
  scale_x_continuous(breaks = c(1, 2, 3, 4, 5, 6, 7, 8, 9))

## Continous_separate_embeddings
embeddings.5.long %>%
  filter(ValenceList != "NA") %>%
  ggplot(aes(ValenceList, WEAT, col = Embedding)) + geom_density(stat = 'identity') + theme(legend.position = "bottom") + 
  theme_bw() +
  facet_grid(Embedding~IAT) +
  geom_hline(yintercept = 0, col = "red", lty = 3) +
  scale_x_continuous(breaks = c(1, 2, 3, 4, 5, 6, 7, 8, 9))


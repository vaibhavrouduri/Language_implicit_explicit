library(readxl)
library(tidyverse)
library(dplyr)
embeddings.data <- read_excel("C:/Users/Vaibhav/Desktop/Thesis work/Final Work/WEAT_across_embeddings_across_valenced_lists.xlsx", 
                              sheet = "All Possible")

embeddings.data <- embeddings.data %>% dplyr::rename("Embedding" = "...1")

embeddings.data.long <- gather(embeddings.data, key = "ValenceList", value = "WEAT", 
                               'Valenced 1 D Value', 'Valenced 2 D Value', 'Valenced 3 D Value', 'Valenced 4 D Value',
                               'Valenced 5 D Value', 'Valenced 6 D Value', 'Valenced 7 D Value', 
                               'Valenced 8 D Value')

embeddings.data.long <- embeddings.data.long %>% filter(!is.na(embeddings.data.long$IAT))

embeddings.data.long %>%
  ggplot(aes(WEAT, fill = IAT))+ geom_density(alpha = 0.3) +
  theme(legend.position = "none") +
  facet_grid(~IAT)+ geom_vline(xintercept = 0, col = "red", lty = 3) 



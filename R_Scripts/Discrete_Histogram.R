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

hist(embeddings.data.long[embeddings.data.long$IAT == "Abstain/Drink",]$WEAT)
hist(embeddings.data.long[embeddings.data.long$IAT == "Age",]$WEAT)
hist(embeddings.data.long[embeddings.data.long$IAT == "Black/White",]$WEAT)
hist(embeddings.data.long[embeddings.data.long$IAT == "Simple/Difficult",]$WEAT)
hist(embeddings.data.long[embeddings.data.long$IAT == "White/Asian",]$WEAT)



pacman::p_load(tidyverse,fs,janitor,knitr)

files <- fs::dir_ls("ShinyApp/data/Changi&MarinaBarrage/") 

# Read all files and clean the column names
ChangiMB <- files %>%
  map_df(~ read_csv(.x, 
                    locale = locale(encoding = "latin1"),
                    col_types = cols(.default = "character")
  ) %>% 
    janitor::clean_names()
  ) 
# Save the Output
write.csv(ChangiMB, "Changi&MarinaBarrage.csv", row.names = FALSE)


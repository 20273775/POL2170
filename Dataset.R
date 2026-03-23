# 1. Configuration et Bibliothèques
# install.packages("tidyverse") # Déjà fait
library(rvest)
library(tidyverse)
library(httr)

eur_to_usd <- 1.1541  # Taux Mars 2026
# Note : 1:124 est très élevé. En 2026, le catalogue complet tient sur ~20-25 pages.
# Si le site vous bloque, réduisez ce nombre à 25.
pages <- 1:12

# 2. Mapping des pays (Inchangé)
country_map <- c(
  "BYD"="China", "Nio"="China", "Zeekr"="China", "Xpeng"="China", "MG"="China", 
  "Omoda"="China", "Jaecoo"="China", "Leapmotor"="China", "Xiaomi"="China", 
  "Hongqi"="China", "Denza"="China", "Yangwang"="China", "Voyah"="China", "GWM"="China",
  "Tesla"="USA", "Lucid"="USA", "Rivian"="USA", "Ford"="USA", "Chevrolet"="USA", 
  "Cadillac"="USA", "Fisker"="USA", "GMC"="USA", "Hummer"="USA",
  "BMW"="Germany", "Mercedes"="Germany", "Audi"="Germany", "Volkswagen"="Germany", 
  "VW"="Germany", "Porsche"="Germany", "Opel"="Germany", "Smart"="Germany",
  "Renault"="France", "Peugeot"="France", "Citroen"="France", "DS"="France", "Alpine"="France",
  "Hyundai"="South Korea", "Kia"="South Korea", "Genesis"="South Korea",
  "Toyota"="Japan", "Nissan"="Japan", "Honda"="Japan", "Lexus"="Japan", "Mazda"="Japan", "Subaru"="Japan",
  "Fiat"="Italy", "Abarth"="Italy", "Alfa"="Italy", "Maserati"="Italy", "Lancia"="Italy",
  "Volvo"="Sweden", "Polestar"="Sweden", "Lotus"="UK", "Jaguar"="UK", "Land"="UK", 
  "Mini"="UK", "Rolls-Royce"="UK", "Skoda"="Czech Republic", "Cupra"="Spain", 
  "VinFast"="Vietnam", "Rimac"="Croatia"
)

# 3. Fonction de Scraping
scrape_full_site <- function(p) {
  Sys.sleep(2) # Délai de sécurité augmenté pour éviter le blocage
  url <- paste0("https://ev-database.org/#group=vehicle-group&s=1&p=", p)
  
  tryCatch({
    res <- GET(url, add_headers(`User-Agent` = "Mozilla/5.0"))
    if(status_code(res) != 200) return(NULL)
    
    page <- read_html(res)
    
    models <- page %>% html_elements(".title") %>% html_text2()
    prices <- page %>% html_elements(".pricing .country_de") %>% html_text2()
    ranges <- page %>% html_elements(".erange_real") %>% html_text2()
    
    n <- min(length(models), length(prices), length(ranges))
    if(n == 0) return(NULL)
    
    tibble(
      Model = models[1:n],
      Price_Raw = prices[1:n],
      Range_Raw = ranges[1:n]
    )
  }, error = function(e) return(NULL))
}

# Mise à jour de la Section 4 pour ne plus rien rater
ev_full_dataset_final <- map_df(pages, scrape_full_site) %>%
  mutate(
    # 1. On nettoie les prix et autonomies immédiatement
    Price_EUR = as.numeric(str_remove_all(Price_Raw, "[^0-9]")),
    Range_km = as.numeric(str_extract(Range_Raw, "[0-9]+")),
    Brand = word(Model, 1),
    
    # 2. On extrait l'année plus intelligemment (cherche 2024, 25 ou 26 n'importe où)
    Year_Detected = str_extract(Model, "202[4-6]|MY2[4-6]"),
    
    # 3. TRANSFORMATION MY -> YEAR (Numérique)
    Year = case_when(
      str_detect(Year_Detected, "24") ~ 2024,
      str_detect(Year_Detected, "25") ~ 2025,
      str_detect(Year_Detected, "26") ~ 2026,
      # Si aucune année n'est dans le titre, on vérifie si c'est une Tesla (souvent sans année)
      is.na(Year_Detected) & Brand == "Tesla" ~ 2026, 
      TRUE ~ NA_real_
    )
  ) %>%
  # 4. FILTRE : On garde 2024-2026 OU les modèles sans date qui ont un prix (nouveautés)
  filter(!is.na(Year) | (is.na(Year) & Price_EUR > 10000)) %>%
  
  # 5. Mapping Pays avec correctifs pour les noms composés
  mutate(
    Country = case_when(
      str_detect(Model, "Land Rover|Range Rover") ~ "UK",
      str_detect(Model, "Rolls-Royce") ~ "UK",
      str_detect(Model, "Alfa Romeo") ~ "Italy",
      TRUE ~ country_map[Brand]
    ),
    Price_USD = round(Price_EUR * eur_to_usd, 0)
  ) %>%
  
  # 6. PROTECTION "TOUTES OPTIONS"
  # On garde les variantes si le prix OU l'autonomie change (ex: Model 3 RWD vs AWD)
  distinct(Model, Price_EUR, Range_km, .keep_all = TRUE) %>%
  
  drop_na(Country, Price_USD) %>%
  select(Country, Year, Brand, Model, Price_USD, Range_km) %>%
  arrange(Brand, Price_USD)

# Vérification immédiate pour Tesla
tesla_check <- ev_full_dataset_final %>% filter(Brand == "Tesla")
print(tesla_check)
# 5. Création des classes d'autonomie (Dataset de travail)
df_global_2026 <- ev_full_dataset_extended %>%
  mutate(
    Autonomy_Class = case_when(
      Range_km < 250 ~ "1. Citadine (<250km)",
      Range_km >= 250 & Range_km < 450 ~ "2. Standard (250-450km)",
      Range_km >= 450 & Range_km < 650 ~ "3. Long Range (450-650km)",
      Range_km >= 650 ~ "4. Ultra (>650km)"
    )
  )

# Affichage des 150 premières lignes
print(df_global_2026, n = 150)


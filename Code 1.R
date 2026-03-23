# Installation des packages et chargement des données 
if (!require("writexl")) install.packages("writexl")
library(rvest)
library(tidyverse)
library(httr)
library(writexl)

# --- 1. CONFIGURATION ---
eur_to_usd <- 1.1541  # Taux effectif Mars 2026
pages <- 1:12         # Échantillon de 12 pages (120 véhicules environ)

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

# --- 2. FONCTION DE SCRAPING ---
scrape_full_site <- function(p) {
  Sys.sleep(2) # Sécurité pour éviter le blocage IP
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
    
    tibble(Model = models[1:n], Price_Raw = prices[1:n], Range_Raw = ranges[1:n])
  }, error = function(e) return(NULL))
}

# --- 3. TRAITEMENT ET NETTOYAGE ---
df_final_clean <- map_df(pages, scrape_full_site) %>%
  mutate(
    # Nettoyage des valeurs numériques
    Price_EUR = as.numeric(str_remove_all(Price_Raw, "[^0-9]")),
    Range_km = as.numeric(str_extract(Range_Raw, "[0-9]+")),
    Brand = word(Model, 1),
    
    # Transformation intelligente de l'année
    Year_Detected = str_extract(Model, "202[4-6]|MY2[4-6]"),
    Year = case_when(
      str_detect(Year_Detected, "24") ~ 2024,
      str_detect(Year_Detected, "25") ~ 2025,
      str_detect(Year_Detected, "26") ~ 2026,
      is.na(Year_Detected) & Brand == "Tesla" ~ 2026, # Fix Tesla Highland/2026
      TRUE ~ 2026 # Par défaut nouveautés
    ),
    
    # Mapping pays avec exceptions
    Country = case_when(
      str_detect(Model, "Land Rover|Range Rover") ~ "UK",
      str_detect(Model, "Rolls-Royce") ~ "UK",
      str_detect(Model, "Alfa Romeo") ~ "Italy",
      TRUE ~ country_map[Brand]
    ),
    Price_USD = round(Price_EUR * eur_to_usd, 0),
    
    # Création des classes d'autonomie
    Autonomy_Class = case_when(
      Range_km < 250 ~ "1. Citadine (<250km)",
      Range_km >= 250 & Range_km < 450 ~ "2. Standard (250-450km)",
      Range_km >= 450 & Range_km < 650 ~ "3. Long Range (450-650km)",
      Range_km >= 650 ~ "4. Ultra (>650km)"
    )
  ) %>%
  # Suppression des doublons techniques
  distinct(Model, Price_USD, Range_km, .keep_all = TRUE) %>%
  drop_na(Country, Price_USD, Autonomy_Class) %>%
  
  # Sélection finale et organisation
  select(Year, Country, Brand, Model, Price_USD, Range_km, Autonomy_Class) %>%
  arrange(Year, Country, Autonomy_Class, Price_USD)

# Partie 2 du Code
# Calcul du prix médian par pays
summary_stats <- df_final_clean %>%
  group_by(Country) %>%
  summarise(
    Nombre_Modeles = n(),
    Prix_Median_USD = median(Price_USD, na.rm = TRUE),
    Range_Median_km = median(Range_km, na.rm = TRUE)
  ) %>%
  arrange(Prix_Median_USD)


# --- 4. VÉRIFICATION ---
print(head(df_final_clean, 20))

# --- 5. Création d'un Database en CSV et d'un fichier excel --- 

# 1. S'assurer que le dossier existe
if (!dir.exists("dataset")) dir.create("dataset")

write_xlsx(
  list("Donnees_Brutes" = df_final_clean, "Synthese" = summary_stats), 
  path = "dataset/Rapport_Complet_EV.xlsx"
)

write_csv(
  df_final_clean, 
  path = "dataset/EV2025-2026.csv")
  
 

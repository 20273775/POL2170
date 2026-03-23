# Calcul du prix médian par pays
analyse_prix <- df_final_clean %>%
  group_by(Country) %>%
  summarise(
    Nombre_Modeles = n(),
    Prix_Median_USD = median(Price_USD, na.rm = TRUE),
    Range_Median_km = median(Range_km, na.rm = TRUE)
  ) %>%
  arrange(Prix_Median_USD)

# Affichage
print(analyse_prix)

# Créer un spreadsheet Excel
write_xlsx(list(Data_Brute = df_final_clean, Synthese_Prix = analyse_prix), 
           "Analyse_EV_Mediane_2026.xlsx")
# Install and load the packages
install.packages(c('dplyr', 'ggplot2', 'factoextra', 'tidyr'))
library(dplyr)
library(factoextra)
library(ggplot2)
library(tidyr) # do zmiany formatu na "długi" (potrzebne do wykresu)

# 1. Usuwamy pakiety, które powodują konflikt
pkgs_to_remove <- c("factoextra", "ggplot2", "dplyr", "tidyr", "rlang")
remove.packages(pkgs_to_remove)

# 2. Instalujemy FUNDAMENT (rlang) jako pierwszy
# UWAGA: Jeśli R zapyta "Do you want to install from sources...", wybierz "No" / "Nie".
install.packages("rlang", dependencies = TRUE)

# 3. Instalujemy resztę pakietów
install.packages(c("ggplot2", "dplyr", "tidyr", "factoextra"), dependencies = TRUE)

# 4. Test - czy biblioteki się ładują?
library(rlang)
library(ggplot2)
library(factoextra)
library(dplyr)

print("Sukces! Zależności naprawione.")

# Read the dane.csv
df <- read.csv("dane.csv")

# Change the column names for better readability
df <- df %>%
  rename_with(~ gsub("\\.[0-4]+\\.", "", .x))


#df_num <- df[, 11:126]
# Make sure all variables are numeric
#df_num <- as.data.frame(lapply(df_num, as.numeric))

df_num <- read.csv("df_num.csv")

# wektor lat
years <- 2019:2023

# lista z ramkami danych dla każdego roku
dfs_by_year <- lapply(
  years,
  \(yr) {
    pat <- paste0("_", yr, "$")
    cols <- grepl(pat, names(df_num))          # wybierz kolumny danego roku
    df_num[, cols, drop = FALSE]
  }
)

# nadaj nazwy elementom listy
names(dfs_by_year) <- paste0("df_num_", years)

# opcjonalnie: wyciągnij do osobnych obiektów
df_num_2019 <- dfs_by_year[["df_num_2019"]]
df_num_2020 <- dfs_by_year[["df_num_2020"]]
df_num_2021 <- dfs_by_year[["df_num_2021"]]
df_num_2022 <- dfs_by_year[["df_num_2022"]]
df_num_2023 <- dfs_by_year[["df_num_2023"]]

# --- KROK 1: Zdefiniuj listę swoich ramek danych dla poszczególnych lat ---
# Upewnij się, że wpisujesz tu dokładne nazwy zmiennych, które utworzyłeś
lista_lat <- list(
  "2019" = df_num_2019,  
  "2020" = df_num_2020,
  "2021" = df_num_2021,
  "2022" = df_num_2022,
  "2023" = df_num_2023
)

# --- KROK 2: Funkcja wykonująca tożsamą analizę (skalowanie -> łokieć -> klastry) ---
wykonaj_analize_roczna <- function(df_input, rok) {
  
  message(paste("\n--- Rozpoczynam analizę dla roku:", rok, "---"))
  
  # 1. Skalowanie danych (niezbędne dla k-means)
  df_scaled <- scale(df_input)
  
  # 2. Wyznaczenie optymalnej liczby klastrów (Metoda Łokcia)
  # To wygeneruje wykres sugerujący liczbę klastrów dla danego roku
  p_elbow <- fviz_nbclust(df_scaled, kmeans, method = "wss") +
    labs(subtitle = paste("Metoda łokcia - Rok", rok))
  print(p_elbow)
  
  # 3. Właściwe klastrowanie (K-means)
  # UWAGA: Tutaj wpisana jest liczba klastrów centers = 4 (standardowo).
  # Jeśli wykres łokcia dla danego roku sugeruje inną liczbę, zmień ten parametr lub ustaw go dynamicznie.
  set.seed(123)
  km_res <- kmeans(df_scaled, centers = 3, nstart = 25)
  
  # 4. Wizualizacja klastrów (PCA Biplot)
  p_cluster <- fviz_cluster(km_res, data = df_scaled,
                            geom = "point", # lub "point" i "text"
                            ellipse.type = "convex", 
                            ggtheme = theme_minimal(),
                            main = paste("Wizualizacja klastrów - Rok", rok))
  print(p_cluster)
  
  # Zwracamy wynik klastrowania, jeśli chciałbyś go zapisać
  return(list(model = km_res, dane_skalowane = df_scaled))
}

# --- KROK 3: Pętla uruchamiająca analizę dla każdego roku ---

wyniki_analizy <- list() # Tu zapiszą się wyniki

for (rok in names(lista_lat)) {
  # Wywołanie funkcji dla konkretnego roku
  wyniki_analizy[[rok]] <- wykonaj_analize_roczna(lista_lat[[rok]], rok)
  
  # Opcjonalnie: dodaj pauzę, żeby zdążyć zobaczyć wykresy, jeśli RStudio je nadpisuje
  Sys.sleep(2) 
}
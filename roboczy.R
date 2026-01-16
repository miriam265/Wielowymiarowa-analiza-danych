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


df_num <- df[, 11:126]
# Make sure all variables are numeric
df_num <- as.data.frame(lapply(df_num, as.numeric))


# Run PCA (with standarization)
pca <- prcomp(df_num, center = TRUE, scale. = TRUE)


print(pca)
summary(pca)

# Wykres wartości własnych (procent wyjaśnionej wariancji)
fviz_eig(pca, 
         addlabels = TRUE,       # Pokazuje procenty na słupkach
         ylim = c(0, 30),        # Zakres osi Y (dostosuj jeśli PC1 ma więcej %)
         main = "Wykres osypiska - Procent wyjaśnionej wariancji")


# Funkcja pomocnicza do wyciągania TOP 10 zmiennych dla danej składowej
pokaz_top_zmienne <- function(nr_skladowej) {
  # Pobieramy ładunki dla wybranej składowej (kolumna PCx)
  ladunki <- pca$rotation[, nr_skladowej]
  
  # Sortujemy wg siły wpływu (wartość bezwzględna - nieważne czy plus czy minus)
  top10 <- sort(abs(ladunki), decreasing = TRUE)[1:10]
  
  # Wyświetlamy oryginalne wartości (z minusem lub plusem) dla tych topowych
  wynik <- ladunki[names(top10)]
  print(paste("--- NAJWAŻNIEJSZE ZMIENNE DLA SKŁADOWEJ PC", nr_skladowej, "---"))
  print(wynik)
}

# Wyświetl wyniki dla 4 składowych
pokaz_top_zmienne(1)
pokaz_top_zmienne(2)
pokaz_top_zmienne(3)
pokaz_top_zmienne(4)

scores <- pca$x
head(scores,10)

ranking_PC1 <- data.frame(
  powiat = df$NazwaJST,
  PC1 = scores[,1]
)

ranking_PC1 <- ranking_PC1[order(-ranking_PC1$PC1), ]



# 1. Wybierz 4 główne składowe (Twoje "profile")
scores <- pca$x[, 1:4]

# 2. Oblicz odległości i drzewo klastrowe (dendrogram)
d <- dist(scores, method = "euclidean")
hc <- hclust(d, method = "ward.D2")

# 3. Wyświetl dendrogram, żeby ocenić ile grup wyciąć
fviz_dend(hc, 
          k = 3,                 # Ile grup
          cex = 0.5,             # Wielkość tekstu (nieważne, bo ukrywamy)
          k_colors = "jco",      # Ładna paleta kolorów
          rect = TRUE,           # Ramki wokół klastrów
          rect_border = "jco",   # Kolor ramek
          rect_fill = TRUE,      # Wypełnienie tła ramek
          show_labels = FALSE,   # <--- KLUCZOWE: Ukrywamy gąszcz napisów
          main = "Podział powiatów na grupy (Dendrogram)"
)

# 4. Podziel na grupy (załóżmy k=4, zmień jeśli wolisz 3)
k_grup <- 3
grupy <- cutree(hc, k = k_grup)

# 5. Dodaj numer grupy do swoich oryginalnych danych
# Zakładam, że Twoja ramka z danymi nazywa się "df" lub "dane"
df$Cluster <- as.factor(grupy)

# Sprawdź liczebność grup
table(df$Cluster)





# Zdefiniuj funkcję do szybkiego rysowania trendów
rysuj_trend <- function(dane_wejsciowe, nazwa_wskaznika_prefix, tytul_wykresu) {
  
  # 1. Szukamy kolumn pasujących do wzorca (np. zaczynających się od "WL3_")
  cols <- grep(paste0("^", nazwa_wskaznika_prefix, "_"), names(dane_wejsciowe), value = TRUE)
  
  # 2. DIAGNOSTYKA: Jeśli nie znaleziono kolumn, sprawdzamy dlaczego
  if (length(cols) == 0) {
    message(paste("❌ BŁĄD: Nie znaleziono kolumn dla prefiksu:", nazwa_wskaznika_prefix))
    message("Sprawdź, czy w Twoim df nazwy mają format 'PREFIKS_ROK' (z podłogą).")
    message("Przykładowe nazwy kolumn w Twoim df:")
    print(head(names(dane_wejsciowe), 10))
    return(NULL)
  }
  
  print(paste("✅ Sukces! Znaleziono", length(cols), "kolumn dla", nazwa_wskaznika_prefix))
  
  # 3. Przetwarzanie danych (poprawione summarise i across)
  df_plot <- dane_wejsciowe %>%
    select(Cluster, all_of(cols)) %>%
    group_by(Cluster) %>%
    summarise(across(everything(), ~ mean(.x, na.rm = TRUE))) %>% # Nowa składnia (tylda)
    pivot_longer(cols = -Cluster, names_to = "Rok", values_to = "Wartosc") %>%
    mutate(Rok = as.numeric(gsub(".*_", "", Rok))) # Wyciąga rok po "_"
  
  # 4. Rysowanie (zaktualizowane geom_line)
  ggplot(df_plot, aes(x = Rok, y = Wartosc, color = Cluster, group = Cluster)) +
    geom_line(linewidth = 1.2) + # 'size' jest przestarzałe, używamy 'linewidth'
    geom_point(size = 3) +
    theme_minimal() +
    labs(title = tytul_wykresu, 
         subtitle = paste("Średnia wartość w grupach dla:", nazwa_wskaznika_prefix),
         y = "Wartość wskaźnika") +
    theme(legend.position = "bottom")
}

# --- WYWOŁANIE FUNKCJI ---
# Zwróć uwagę, że teraz podajemy 'df' jako pierwszy argument!

# 1. Zadłużenie (WL3)
rysuj_trend(df, "WL3", "Dynamika Zadłużenia (Zobowiązania na mieszkańca)")

# 2. Nadwyżka Operacyjna (WL2 - sprawdź czy masz tę kolumnę, jeśli nie to np. WB3)
rysuj_trend(df, "WL2", "Zdolność Inwestycyjna (Nadwyżka Operacyjna)")

# 3. Jeśli WL2 nie zadziała, spróbuj WB3 (częsty zamiennik w tych danych)
rysuj_trend(df, "WB3", "Zdolność Inwestycyjna (Relacja nadwyżki)")

# TU MOŻNA DODAĆ JESZCZE CAŁE MNÓSTWO WYKRESÓW DLA INNYCH WSKAŹNIKÓW

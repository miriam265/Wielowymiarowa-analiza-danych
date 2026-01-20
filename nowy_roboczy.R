# ==============================================================================
# ANALIZA WIELOWYMIAROWA POWIATÓW - HELLWIG, PCA, KLASTROWANIE
# ==============================================================================

# --- KROK 1: INSTALACJA I WCZYTANIE PAKIETÓW ---
#install.packages(c("ggplot2", "dplyr", "tidyr", "factoextra", "cluster"), dependencies = TRUE)
library(dplyr)
library(factoextra)
library(ggplot2)
library(tidyr)
library(cluster)

# --- KROK 2: WCZYTANIE I PRZYGOTOWANIE DANYCH ---

# Wczytanie danych
df <- read.csv("dane.csv")

# Czyszczenie nazw kolumn
df <- df %>%
  rename_with(~ gsub("\\.[0-4]+\\.", "", .x))

# Wydzielenie zmiennych numerycznych (kolumny 11-126)
df_num <- df[, 11:126]
df_num <- as.data.frame(lapply(df_num, as.numeric))

# Obliczenie zmienności i filtrowanie zmiennych
zmiennosc <- apply(df_num, 2, function(x) sd(x, na.rm=TRUE) / mean(x, na.rm=TRUE))
df_wybrane <- df_num[, abs(zmiennosc) > 0.1]

message("--- WYBRANE ZMIENNE (zmienność > 10%) ---")
print(colnames(df_wybrane))

# --- KROK 3: UNIFIKACJA ZMIENNYCH (STYMULANT/DESTYMULANT) ---

# Definicja destymulant zgodnie z Ministerstwem Finansów
wzorce_destymulant <- c("^WZ", "^WL3", "^WB5")

# Identyfikacja kolumn-destymulant
cols_destymulanty <- grep(paste(wzorce_destymulant, collapse = "|"), 
                          names(df_wybrane), value = TRUE)

message("--- ZIDENTYFIKOWANO JAKO DESTYMULANTY ---")
print(cols_destymulanty)

# Transformacja destymulant na stymulanty (odwrócenie znaku)
df_norm <- df_wybrane
df_norm[cols_destymulanty] <- df_norm[cols_destymulanty] * -1

# Standaryzacja wszystkich zmiennych
df_scaled <- scale(df_norm)
df_scaled <- as.data.frame(df_scaled)

message("✅ Zmienne ujednolicone i ustandaryzowane")

# --- KROK 4: OBLICZENIE WAG OPARTYCH NA WSPÓŁCZYNNIKU ZMIENNOŚCI ---

# Obliczenie współczynników zmienności dla wybranych zmiennych
zmiennosc_wybrane <- apply(df_norm, 2, function(x) sd(x, na.rm=TRUE) / mean(abs(x), na.rm=TRUE))

# Normalizacja wag (suma = 1)
wagi <- abs(zmiennosc_wybrane) / sum(abs(zmiennosc_wybrane))

message("--- WAGI OPARTE NA WSPÓŁCZYNNIKU ZMIENNOŚCI ---")
print(head(sort(wagi, decreasing = TRUE), 10))

# Utworzenie macierzy diagonalnej wag (do ważonej odległości)
W <- diag(wagi)

message("✅ Wagi obliczone")

# --- KROK 5: RANKING HELLWIGA Z WAGAMI ---

# Ważona standaryzacja (mnożenie przez pierwiastek z wagi)
df_weighted <- sweep(df_scaled, 2, sqrt(wagi), "*")

# Wyznaczenie wzorca (maksimum dla każdej zmiennej)
wzorzec <- apply(df_weighted, 2, max)

# Obliczenie ważonej odległości euklidesowej od wzorca
odleglosci <- apply(df_weighted, 1, function(row) {
  sqrt(sum((row - wzorzec)^2))
})

# Normalizacja wskaźnika Hellwiga (0-1)
d0 <- mean(odleglosci) + 2 * sd(odleglosci)
tmr_hellwig <- 1 - (odleglosci / d0)
tmr_hellwig[tmr_hellwig < 0] <- 0

# Dodanie wyniku do głównej ramki danych
df$HELLWIG_SCORE <- tmr_hellwig

message("✅ Ranking Hellwiga (z wagami) obliczony")

# --- KROK 6: ANALIZA GŁÓWNYCH SKŁADOWYCH (PCA) ---

# Przeprowadzenie PCA na danych ustandaryzowanych (bez wag - PCA sam je wyznacza)
pca <- prcomp(df_scaled, center = FALSE, scale. = FALSE)

# Wykres osypiska (wariancja wyjaśniona)
fviz_eig(pca, addlabels = TRUE, ylim = c(0, 40), 
         main = "Wykres osypiska - PCA")

# Funkcja do analizy ładunków składowych
pokaz_top_zmienne <- function(nr_skladowej) {
  ladunki <- pca$rotation[, nr_skladowej]
  top10 <- sort(abs(ladunki), decreasing = TRUE)[1:10]
  wynik <- ladunki[names(top10)]
  print(paste("--- ZMIENNE TWORZĄCE SKŁADOWĄ PC", nr_skladowej, "---"))
  print(wynik)
}

# Analiza głównych składowych
pokaz_top_zmienne(1)
pokaz_top_zmienne(2)

# Zapisanie wyników PC1
df$PC1_SCORE <- pca$x[, 1]

message("✅ PCA przeprowadzone")

# --- KROK 7: PORÓWNANIE METOD ---

# Korelacja między Hellwigiem a PC1
korelacja <- cor(df$HELLWIG_SCORE, df$PC1_SCORE)
print(paste("Korelacja między Hellwigiem a PCA:", round(korelacja, 3)))

# Ranking TOP 10 powiatów
ranking_final <- df %>%
  select(NazwaJST, HELLWIG_SCORE, PC1_SCORE) %>%
  arrange(desc(HELLWIG_SCORE))

print("--- TOP 10 POWIATÓW (HELLWIG) ---")
print(head(ranking_final, 10))

# --- KROK 8: KLASTROWANIE HIERARCHICZNE ---

# Dane do klastrowania (4 pierwsze składowe PCA)
dane_do_klastrowania <- pca$x[, 1:4]

# Automatyczny dobór liczby klastrów (metoda Silhouette)
p_sil <- fviz_nbclust(dane_do_klastrowania, FUN = hcut, 
                      method = "silhouette", k.max = 7) +
  labs(title = "Optymalna liczba klastrów (Metoda Silhouette - Hierarchiczne)",
       subtitle = "Wybierz liczbę, gdzie słupek jest najwyższy")
print(p_sil)

# Obliczenie klastrów hierarchicznych
d <- dist(dane_do_klastrowania, method = "euclidean")
hc <- hclust(d, method = "ward.D2")

# Wizualizacja dendrogramu
k_grup <- 3  # Zmień zgodnie z wynikiem metody Silhouette
fviz_dend(hc, 
          k = k_grup,
          cex = 0.5,
          k_colors = "jco",
          rect = TRUE,
          rect_border = "jco",
          rect_fill = TRUE,
          show_labels = FALSE,
          main = "Podział powiatów na grupy (Dendrogram - Hierarchiczne)")

# Przypisanie klastrów hierarchicznych
grupy_hierarchiczne <- cutree(hc, k = k_grup)
df$Cluster_Hierarchical <- as.factor(grupy_hierarchiczne)

print("--- LICZEBNOŚĆ GRUP (HIERARCHICZNE) ---")
print(table(df$Cluster_Hierarchical))

message("✅ Klastrowanie hierarchiczne zakończone")

# --- KROK 9: KLASTROWANIE NIEHIERARCHICZNE (PAM) ---

# Automatyczny dobór liczby klastrów dla PAM
p_sil_pam <- fviz_nbclust(dane_do_klastrowania, FUN = pam, 
                          method = "silhouette", k.max = 7) +
  labs(title = "Optymalna liczba klastrów (Metoda Silhouette - PAM)",
       subtitle = "Wybierz liczbę, gdzie słupek jest najwyższy")
print(p_sil_pam)

# Klastrowanie PAM
k_pam <- 3  # Zmień zgodnie z wynikiem metody Silhouette
pam_result <- pam(dane_do_klastrowania, k = k_pam)

# Przypisanie klastrów PAM
df$Cluster_PAM <- as.factor(pam_result$clustering)

print("--- LICZEBNOŚĆ GRUP (PAM) ---")
print(table(df$Cluster_PAM))

# Wizualizacja klastrów PAM
fviz_cluster(pam_result, 
             data = dane_do_klastrowania,
             palette = "jco",
             ggtheme = theme_minimal(),
             main = "Wizualizacja klastrów (PAM)",
             geom = "point",
             ellipse.type = "convex")

# Statystyki jakości klastrowania PAM
print("--- STATYSTYKI PAM ---")
print(pam_result$silinfo$avg.width)  # Średnia szerokość sylwetki

message("✅ Klastrowanie PAM zakończone")

# --- KROK 10: PORÓWNANIE METOD KLASTROWANIA ---

# Tabela krzyżowa: zgodność klastrów hierarchicznych i PAM
tabela_zgodnosci <- table(Hierarchiczne = df$Cluster_Hierarchical, 
                          PAM = df$Cluster_PAM)
print("--- ZGODNOŚĆ METOD KLASTROWANIA ---")
print(tabela_zgodnosci)

# Współczynnik zgodności (Adjusted Rand Index)
library(mclust)
ari <- adjustedRandIndex(df$Cluster_Hierarchical, df$Cluster_PAM)
print(paste("Adjusted Rand Index (zgodność metod):", round(ari, 3)))

# --- KROK 11: WIZUALIZACJA TRENDÓW ---

# Funkcja do rysowania trendów w czasie
rysuj_trend <- function(dane_wejsciowe, nazwa_wskaznika_prefix, tytul_wykresu, metoda_klaster = "Hierarchical") {
  
  # Wybór kolumny klastra
  col_cluster <- ifelse(metoda_klaster == "PAM", "Cluster_PAM", "Cluster_Hierarchical")
  
  # Szukanie kolumn pasujących do wzorca
  cols <- grep(paste0("^", nazwa_wskaznika_prefix, "_"), 
               names(dane_wejsciowe), value = TRUE)
  
  if (length(cols) == 0) {
    message(paste("❌ BŁĄD: Nie znaleziono kolumn dla:", nazwa_wskaznika_prefix))
    return(NULL)
  }
  
  # Agregacja danych (średnia dla klastra w każdym roku)
  df_temp <- dane_wejsciowe %>%
    rename(Cluster = !!sym(col_cluster))
  
  df_summary <- df_temp %>%
    group_by(Cluster) %>%
    summarise(across(all_of(cols), ~ mean(.x, na.rm = TRUE)))
  
  # Transformacja do formy długiej
  df_plot <- df_summary %>%
    pivot_longer(cols = -Cluster, names_to = "Rok_raw", values_to = "Wartosc") %>%
    mutate(Rok = as.numeric(gsub(".*_", "", Rok_raw)))
  
  # Wykres
  p <- ggplot(df_plot, aes(x = Rok, y = Wartosc, color = Cluster, group = Cluster)) +
    geom_line(linewidth = 1.5) +
    geom_point(size = 3) +
    theme_minimal() +
    scale_color_brewer(palette = "Set1") +
    labs(title = paste(tytul_wykresu, "-", metoda_klaster), 
         subtitle = paste("Średnia wartość wskaźnika", nazwa_wskaznika_prefix, "w grupach"),
         y = "Średnia Wartość",
         x = "Rok") +
    theme(legend.position = "bottom",
          plot.title = element_text(face = "bold"))
  
  print(p)
}

# Generowanie wykresów trendów dla klastrowania hierarchicznego
print("=== WYKRESY DLA KLASTROWANIA HIERARCHICZNEGO ===")
rysuj_trend(df, "WL3", "Zadłużenie (Wskaźnik Zobowiązań)", "Hierarchical")
rysuj_trend(df, "WL2", "Nadwyżka Operacyjna", "Hierarchical")
rysuj_trend(df, "WB5", "Obciążenie wynagrodzeniami", "Hierarchical")
rysuj_trend(df, "WB1", "Dochody bieżące", "Hierarchical")

# Generowanie wykresów trendów dla klastrowania PAM
print("=== WYKRESY DLA KLASTROWANIA PAM ===")
rysuj_trend(df, "WL3", "Zadłużenie (Wskaźnik Zobowiązań)", "PAM")
rysuj_trend(df, "WL2", "Nadwyżka Operacyjna", "PAM")
rysuj_trend(df, "WB5", "Obciążenie wynagrodzeniami", "PAM")
rysuj_trend(df, "WB1", "Dochody bieżące", "PAM")

message("✅ ANALIZA ZAKOŃCZONA")
library(dplyr)
library(tidyr)
library(stringr)

df <- dane

# 1) Naprawa nagłówków, jeśli są V1, V2... a prawdziwe nazwy są w 1. wierszu
if (all(grepl("^V[0-9]+$", names(df)))) {
  first_row <- as.character(unlist(df[1, ], use.names = FALSE))
  if (any(grepl("2019|2020|2021|2022|2023", first_row))) {
    names(df) <- first_row
    df <- df[-1, , drop = FALSE]
  }
}

# 2) Id obiektu
id_col <- if ("powiat" %in% names(df)) "powiat" else if ("NazwaJST" %in% names(df)) "NazwaJST" else names(df)[1]
obiekty <- as.character(df[[id_col]])

# 3) Konwersja na numeric dla wszystkich kolumn poza id
to_num <- function(x){
  x <- unlist(x, use.names = FALSE)
  x <- gsub(",", ".", as.character(x))
  suppressWarnings(as.numeric(x))
}

# 4) Lata i destymulanty
years <- 2019:2023
destim_prefix <- c("WL3")  # zobowiązania

# 5) Wybór kolumn dla roku (szuka tekstu roku w NAZWIE kolumny)
X_year <- function(df, year){
  nms <- names(df)
  idx <- grep(as.character(year), nms, fixed = TRUE)
  idx <- setdiff(idx, which(nms == id_col))
  if (length(idx) == 0) return(NULL)
  X <- df[, idx, drop = FALSE]
  X <- as.data.frame(lapply(X, to_num), check.names = FALSE)
  keep <- sapply(X, function(v) any(!is.na(v)))
  X <- X[, keep, drop = FALSE]
  if (ncol(X) == 0) return(NULL)
  X
}

destim_to_stim <- function(X){
  base <- names(X)
  base <- sub("\\[.*?\\]", "", base)
  base <- sub("\\..*?$", "", base)
  base <- sub("_.*?$", "", base)
  for (j in seq_along(base)) {
    if (base[j] %in% destim_prefix) {
      X[[j]] <- max(X[[j]], na.rm = TRUE) - X[[j]]
    }
  }
  X
}

clean_impute <- function(X){
  X <- X[, colSums(is.na(X)) < nrow(X), drop = FALSE]
  keep <- sapply(X, function(v){
    vv <- v[!is.na(v)]
    length(vv) > 1 && sd(vv) > 0
  })
  keep[is.na(keep)] <- FALSE
  X <- X[, which(keep), drop = FALSE]
  if (ncol(X) == 0) return(NULL)
  for (j in seq_along(X)) {
    if (anyNA(X[[j]])) X[[j]][is.na(X[[j]])] <- median(X[[j]], na.rm = TRUE)
  }
  X
}

tmr_year <- function(df, year, obiekty){
  X <- X_year(df, year)
  if (is.null(X)) return(NULL)
  
  X <- destim_to_stim(X)
  X <- clean_impute(X)
  if (is.null(X)) return(NULL)
  
  Z <- as.data.frame(scale(X))
  for (j in seq_along(Z)) {
    if (anyNA(Z[[j]])) Z[[j]][is.na(Z[[j]])] <- median(Z[[j]], na.rm = TRUE)
  }
  
  # CV
  V <- apply(X, 2, function(x) sd(x, na.rm = TRUE) / mean(x, na.rm = TRUE))
  V[!is.finite(V)] <- 0
  w_cv <- if (sum(V) == 0) rep(1/ncol(X), ncol(X)) else V / sum(V)
  TMR_cv <- rowSums(sweep(Z, 2, w_cv, `*`))
  
  # CRITIC
  sigma <- apply(Z, 2, sd, na.rm = TRUE)
  R <- cor(Z, use = "pairwise.complete.obs")
  R[is.na(R)] <- 0
  C <- sigma * apply(1 - R, 1, sum)
  C[!is.finite(C)] <- 0
  w_critic <- if (sum(C) == 0) rep(1/ncol(Z), ncol(Z)) else C / sum(C)
  TMR_critic <- rowSums(sweep(Z, 2, w_critic, `*`))
  
  # Entropia
  Zpos <- Z - apply(Z, 2, min, na.rm = TRUE) + 1e-6
  p <- sweep(Zpos, 2, colSums(Zpos), "/")
  p[p <= 0] <- 1e-12
  k <- 1 / log(nrow(p))
  E <- -k * colSums(p * log(p))
  d <- 1 - E
  d[!is.finite(d)] <- 0
  w_entropy <- if (sum(d) == 0) rep(1/ncol(Z), ncol(Z)) else d / sum(d)
  TMR_entropy <- rowSums(sweep(Z, 2, w_entropy, `*`))
  
  data.frame(
    Obiekt = obiekty,
    Rok = year,
    TMR_cv = TMR_cv,
    TMR_critic = TMR_critic,
    TMR_entropy = TMR_entropy,
    rank_cv = rank(-TMR_cv, ties.method = "average"),
    rank_critic = rank(-TMR_critic, ties.method = "average"),
    rank_entropy = rank(-TMR_entropy, ties.method = "average")
  )
}

res <- lapply(years, function(y) tmr_year(df, y, obiekty))
res <- Filter(Negate(is.null), res)

tmr_all <- bind_rows(res)

if (!"Rok" %in% names(tmr_all)) {
  stop("Nie policzono TMR: nadal nie znaleziono kolumn z latami w nazwach. Sprawdź names(dane) po naprawie.")
}

tmr_all %>% filter(Rok == 2019) %>% arrange(rank_cv) %>% head(10)
tmr_all %>% filter(Rok == 2023) %>% arrange(rank_cv) %>% head(10)

write.csv(tmr_all, "wyniki_TMR_2019_2023.csv", row.names = FALSE)

# =============================================================================
#  Décomposition harmonique de la température de surface — figures complètes
#  1. Sélection du nombre d'harmoniques K par AIC/BIC
#  2. Décomposition sur la série complète (1991-2023)
#  3. Zoom sur une année (2000) avec flèches d'anomalie
#  4. Extraction des coefficients (theta0, theta1, ak, bk) pour l'annexe
# =============================================================================

library(dplyr)
library(tidyr)
library(ggplot2)
library(patchwork)

load("temp_v4.RData")

# =============================================================================
#  1. Sélection du nombre d'harmoniques K
# =============================================================================

select_K_harmonics <- function(df_long, K_max = 12) {
  
  temp_surface_mensuelle <- df_long %>%
    dplyr::group_by(time) %>%
    dplyr::summarise(temp_moy_bassin = mean(temperature, na.rm = TRUE)) %>%
    dplyr::rename(date = time) %>%
    dplyr::arrange(date) %>%
    dplyr::mutate(
      mois           = as.numeric(format(date, "%m")),
      annee          = as.numeric(format(date, "%Y")),
      annee_continue = annee + (mois - 1) / 12
    )
  
  results <- lapply(0:K_max, function(K) {
    
    if (K == 0) {
      formule <- as.formula("temp_moy_bassin ~ annee_continue")
    } else {
      harm_terms <- unlist(lapply(seq_len(K), function(k) {
        c(sprintf("sin(2*pi*%d*annee_continue)", k),
          sprintf("cos(2*pi*%d*annee_continue)", k))
      }))
      formule <- as.formula(paste("temp_moy_bassin ~ annee_continue +", paste(harm_terms, collapse = " + ")))
    }
    
    m <- lm(formule, data = temp_surface_mensuelle)
    
    data.frame(K = K, AIC = AIC(m), BIC = BIC(m), R2 = summary(m)$r.squared)
  })
  
  bind_rows(results)
}

k_selection <- select_K_harmonics(df_long, K_max = 12)
print(k_selection)

K_best_aic <- k_selection$K[which.min(k_selection$AIC)]
K_best_bic <- k_selection$K[which.min(k_selection$BIC)]
cat("K optimal par AIC :", K_best_aic, "| par BIC :", K_best_bic, "\n")

k_selection_long <- k_selection %>%
  tidyr::pivot_longer(cols = c(AIC, BIC), names_to = "score", values_to = "valeur")

p_k_selection <- ggplot(k_selection_long, aes(x = K, y = valeur, colour = score)) +
  geom_line() +
  geom_point(size = 2) +
  geom_vline(xintercept = K_best_aic, linetype = "dashed", colour = "grey50") +
  scale_colour_manual(name = "Score", values = c("AIC" = "#2166AC", "BIC" = "#B2182B")) +
  scale_x_continuous(breaks = 0:12) +
  labs(x = "Nombre d'harmoniques K", y = "Valeur du score") +
  theme_minimal(base_size = 10)

p_k_selection

ggsave("../../rapport/rapport_final/figures/results/detrending/00_selection_K.pdf",
       plot = p_k_selection, width = 16, height = 10, units = "cm")

write.csv(k_selection,
          "../../rapport/rapport_final/figures/results/detrending/k_selection_table.csv",
          row.names = FALSE)


# =============================================================================
#  2. Décomposition harmonique complète (K retenu)
# =============================================================================

prepare_decomposition <- function(df_long, K_HARMONICS = 5) {
  
  temp_surface_mensuelle <- df_long %>%
    dplyr::group_by(time) %>%
    dplyr::summarise(temp_moy_bassin = mean(temperature, na.rm = TRUE)) %>%
    dplyr::rename(date = time) %>%
    dplyr::arrange(date) %>%
    dplyr::mutate(
      mois           = as.numeric(format(date, "%m")),
      annee          = as.numeric(format(date, "%Y")),
      annee_continue = annee + (mois - 1) / 12
    )
  
  harm_terms <- unlist(lapply(seq_len(K_HARMONICS), function(k) {
    c(sprintf("sin(2*pi*%d*annee_continue)", k),
      sprintf("cos(2*pi*%d*annee_continue)", k))
  }))
  
  formule_decomp <- as.formula(
    paste("temp_moy_bassin ~ annee_continue +", paste(harm_terms, collapse = " + "))
  )
  
  lm_decomp <- lm(formule_decomp, data = temp_surface_mensuelle)
  coefs <- coef(lm_decomp)
  
  trend_component    <- coefs["(Intercept)"] + coefs["annee_continue"] * temp_surface_mensuelle$annee_continue
  fitted_full         <- fitted(lm_decomp)
  seasonal_component <- fitted_full - trend_component
  
  temp_surface_mensuelle <- temp_surface_mensuelle %>%
    dplyr::mutate(
      trend_component    = trend_component,
      seasonal_component = seasonal_component,
      fitted_full        = fitted_full,
      temp_anomaly_harm  = residuals(lm_decomp)
    )
  
  list(data = temp_surface_mensuelle, model = lm_decomp)
}

decomp_result <- prepare_decomposition(df_long, K_HARMONICS = 5)
decomp_df <- decomp_result$data
lm_decomp <- decomp_result$model

cat("R2 detrending harmonique (K = 5) :", summary(lm_decomp)$r.squared, "\n")
cat("SD anomalie harmonique :", sd(decomp_df$temp_anomaly_harm), "\n")

decomp_zoom_periode <- decomp_df %>%
  dplyr::filter(annee >= 1995, annee <= 2005)

p_full <- ggplot(decomp_zoom_periode, aes(x = date)) +
  geom_line(aes(y = temp_moy_bassin, colour = "Température brute"), linewidth = 0.4) +
  geom_line(aes(y = trend_component, colour = "Tendance linéaire"), linewidth = 0.7) +
  geom_line(aes(y = trend_component + seasonal_component, colour = "Climatologie saisonnière"), linewidth = 0.5, alpha = 0.8) +
  scale_colour_manual(
    name = NULL,
    values = c("Température brute" = "grey40",
               "Tendance linéaire" = "#E69F00",
               "Climatologie saisonnière" = "#2CA02C")
  ) +
  labs(x = NULL, y = "Température (°C)") +
  theme_minimal(base_size = 9) +
  theme(legend.position = "top")

p_anomaly <- ggplot(decomp_zoom_periode, aes(x = date, y = temp_anomaly_harm)) +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey50") +
  geom_line(colour = "#762A83", linewidth = 0.5) +
  labs(x = "Année", y = "Anomalie thermique (°C)") +
  theme_minimal(base_size = 9)

p_full_combined <- p_full / p_anomaly + plot_layout(heights = c(2, 1))
p_full_combined

ggsave("../../rapport/rapport_final/figures/results/detrending/01_decomposition_complete.pdf",
       plot = p_full_combined, width = 18, height = 12, units = "cm")


# =============================================================================
#  3. Zoom sur une année (2000), avec flèches pour les anomalies
# =============================================================================

decomp_zoom <- decomp_df %>% dplyr::filter(annee == 2000)

p_zoom <- ggplot(decomp_zoom, aes(x = date)) +
  geom_line(aes(y = trend_component), colour = "#E69F00", linewidth = 0.8) +
  geom_line(aes(y = trend_component + seasonal_component), colour = "#2CA02C", linewidth = 0.8) +
  geom_segment(aes(x = date, xend = date,
                    y = trend_component + seasonal_component,
                    yend = temp_moy_bassin),
               colour = "#762A83", linewidth = 0.5,
               arrow = arrow(length = unit(0.15, "cm"), type = "closed")) +
  geom_point(aes(y = temp_moy_bassin), colour = "grey20", size = 1.5) +
  labs(title = "Décomposition harmonique — zoom sur l'année 2000",
       x = "Mois", y = "Température (°C)") +
  scale_x_date(date_labels = "%b") +
  theme_minimal(base_size = 10)

p_zoom

ggsave("../../rapport/rapport_final/figures/results/detrending/02_decomposition_zoom_2000.pdf",
       plot = p_zoom, width = 16, height = 10, units = "cm")


# =============================================================================
#  4. Extraction des coefficients pour l'annexe (theta0, theta1, ak, bk)
# =============================================================================

s <- summary(lm_decomp)
coef_table_harmonique <- as.data.frame(s$coefficients)
coef_table_harmonique$terme <- rownames(coef_table_harmonique)
rownames(coef_table_harmonique) <- NULL
coef_table_harmonique <- coef_table_harmonique[, c("terme", "Estimate", "Std. Error", "t value", "Pr(>|t|)")]
names(coef_table_harmonique) <- c("terme", "estimate", "se", "t_value", "p_value")

print(coef_table_harmonique)

write.csv(coef_table_harmonique,
          "../../rapport/rapport_final/figures/results/detrending/coefs_harmoniques.csv",
          row.names = FALSE)
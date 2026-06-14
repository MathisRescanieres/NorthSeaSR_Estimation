# =============================================================================
#  Effets estimés — gratia::draw, plots individuels par espèce (PDF)
#  Sortie : /figures/results/gratia/<espece>/
# =============================================================================

library(gratia)
library(ggplot2)

out_root <- "/home/mathis/NorthSeaSR_Estimation/SR_Estimation/rapport/rapport_miproj/figures/results/gratia"

for (sp in names(gam_models_final)) {

  cat("gratia :", sp, "...\n")

  sp_slug <- gsub(" ", "_", sp)
  out_dir <- file.path(out_root, sp_slug)
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

  m           <- gam_models_final[[sp]]
  term_labels <- gratia::smooths(m)

  # ── 1. Tous les effets lisses ───────────────────────────────────────────────
  p_all <- gratia::draw(m, residuals = FALSE, rug = FALSE)
  ggsave(file.path(out_dir, "01_all_smooths.pdf"),
         plot = p_all, width = 14, height = 10, device = "pdf")

  # ── 2. Surface Age × Taille (terme 1) ─────────────────────────────────────
  p_te <- gratia::draw(m, select = 1)
  ggsave(file.path(out_dir, "02_surface_age_taille.pdf"),
         plot = p_te, width = 7, height = 6, device = "pdf")

  # ── 3. Tendance de cohorte f2 (si présente) ───────────────────────────────
  f2_idx <- grep("Cohort_num_sc", term_labels)
  if (length(f2_idx) > 0) {
    p_f2 <- gratia::draw(m, select = f2_idx)
    ggsave(file.path(out_dir, "03_cohort_trend.pdf"),
           plot = p_f2, width = 7, height = 5, device = "pdf")
  }

  # ── 4. Effet spatial (si présent) ─────────────────────────────────────────
  f3_idx <- grep("Latitude,Longitude", term_labels)
  if (length(f3_idx) > 0) {
    p_f3 <- gratia::draw(m, select = f3_idx)
    ggsave(file.path(out_dir, "04_spatial.pdf"),
           plot = p_f3, width = 6, height = 6, device = "pdf")
  }

  # ── 5. Effets aléatoires de cohorte (si présents) ─────────────────────────
  bc_idx <- grep("Cohort_fact", term_labels)
  if (length(bc_idx) > 0) {
    p_bc <- gratia::draw(m, select = bc_idx)
    ggsave(file.path(out_dir, "05_re_cohort.pdf"),
           plot = p_bc, width = 8, height = 5, device = "pdf")
  }

  # ── 6. Effets aléatoires d'aire (si présents) ─────────────────────────────
  ur_idx <- grep("Area_fact", term_labels)
  if (length(ur_idx) > 0) {
    p_ur <- gratia::draw(m, select = ur_idx)
    ggsave(file.path(out_dir, "06_re_area.pdf"),
           plot = p_ur, width = 6, height = 5, device = "pdf")
  }

  cat("  Done →", out_dir, "\n\n")
}

# ==============================================================================
# build_visualization_pdfs.R  —  version rapide
# ==============================================================================

library(pdftools)
library(magick)

build_visualization_pdfs <- function(dir_eof, depths = NULL) {

  dir_viz   <- file.path(dir_eof, "visualization")
  dir_v_map <- file.path(dir_viz, "maps")
  dir_v_ts  <- file.path(dir_viz, "timeseries")

  for (d in c(dir_viz, dir_v_map, dir_v_ts)) {
    if (!dir.exists(d)) dir.create(d, recursive = TRUE)
  }

  .process_type <- function(type_label) {

    dir_src  <- file.path(dir_eof, type_label)
    dir_dest <- file.path(dir_viz, type_label)

    month_dirs <- sort(list.dirs(dir_src, recursive = FALSE, full.names = TRUE))

    for (month_dir in month_dirs) {

      month_name    <- basename(month_dir)
      dir_out_month <- file.path(dir_dest, month_name)
      if (!dir.exists(dir_out_month)) dir.create(dir_out_month, recursive = TRUE)

      depth_dirs <- sort(list.dirs(month_dir, recursive = FALSE, full.names = TRUE))

      for (depth_dir in depth_dirs) {

        depth_name <- basename(depth_dir)

        # -- filtre profondeurs ------------------------------------------------
        if (!is.null(depths)) {
          depth_val <- as.numeric(gsub("depth_", "", depth_name))
          if (!depth_val %in% depths) next
        }
        # ----------------------------------------------------------------------

        pdfs_raw <- list.files(depth_dir, pattern = "\\.pdf$", full.names = TRUE)

        if (length(pdfs_raw) == 0) {
          cat("  [SKIP] aucun PDF :", depth_dir, "\n")
          next
        }

        # -- tri numérique sur le numéro de PC extrait du nom de fichier -------
        pc_nums <- as.numeric(regmatches(
          basename(pdfs_raw),
          regexpr("(?<=_PC)[0-9]+", basename(pdfs_raw), perl = TRUE)
        ))

        # fallback : si le pattern _PC n'est pas trouvé, tri alphabétique
        if (any(is.na(pc_nums))) {
          pdfs <- sort(pdfs_raw)
        } else {
          pdfs <- pdfs_raw[order(pc_nums)]
        }
        # ----------------------------------------------------------------------

        cat(sprintf("  → %s | %s | %s — %d PDF(s) ... ",
                    type_label, month_name, depth_name, length(pdfs)))

        file_out <- file.path(dir_out_month, paste0(depth_name, ".pdf"))

        t0 <- proc.time()
        .assemble_patch_pdf_fast(pdfs, file_out)
        cat(round((proc.time() - t0)["elapsed"], 1), "s\n")
      }
    }
  }

  cat(">>> Construction des PDFs : maps\n")
  .process_type("maps")

  cat(">>> Construction des PDFs : timeseries\n")
  .process_type("timeseries")

  cat("\n✔ Visualisations créées dans :", dir_viz, "\n")
  invisible(dir_viz)
}


# ==============================================================================
# .assemble_patch_pdf_fast
# ==============================================================================

.assemble_patch_pdf_fast <- function(pdf_paths, file_out,
                                      n_rows = 3,
                                      n_cols = 2,
                                      sep_px = 8,
                                      dpi    = 120) {

  patch_size <- n_rows * n_cols

  # -- 1. rasterisation --------------------------------------------------------
  thumbs <- lapply(pdf_paths, function(f) {
    magick::image_read(
      pdftools::pdf_render_page(f, page = 1, dpi = dpi, numeric = FALSE)
    )
  })

  n_pages <- length(thumbs)

  # -- 2. padding du dernier patch si incomplet --------------------------------
  n_patches <- ceiling(n_pages / patch_size)
  n_total   <- n_patches * patch_size

  if (n_total > n_pages) {
    blank <- magick::image_blank(
      magick::image_info(thumbs[[1]])$width,
      magick::image_info(thumbs[[1]])$height,
      color = "white"
    )
    for (i in (n_pages + 1):n_total) thumbs[[i]] <- blank
  }

  # -- 3. construction des patches ---------------------------------------------
  sep_img <- magick::image_blank(
    sep_px,
    magick::image_info(thumbs[[1]])$height * n_rows,
    color = "black"
  )

  patch_imgs <- vector("list", n_patches)

  for (p in seq_len(n_patches)) {

    idx_start <- (p - 1) * patch_size + 1
    patch     <- thumbs[idx_start:(idx_start + patch_size - 1)]

    rows_imgs <- vector("list", n_rows)
    for (r in seq_len(n_rows)) {
      row_idx        <- ((r - 1) * n_cols + 1):(r * n_cols)
      rows_imgs[[r]] <- magick::image_append(
        magick::image_join(patch[row_idx]),
        stack = FALSE
      )
    }

    patch_imgs[[p]] <- magick::image_append(
      magick::image_join(rows_imgs),
      stack = TRUE
    )
  }

  # -- 4. intercaler les barres noires -----------------------------------------
  pieces <- vector("list", 2 * n_patches - 1)
  for (p in seq_len(n_patches)) {
    pieces[[2 * p - 1]] <- patch_imgs[[p]]
    if (p < n_patches) pieces[[2 * p]] <- sep_img
  }

  # -- 5. assemblage final + export --------------------------------------------
  final <- magick::image_append(magick::image_join(pieces), stack = FALSE)

  magick::image_write(final, path = file_out, format = "pdf",
                      density = paste0(dpi, "x", dpi))

  invisible(file_out)
}
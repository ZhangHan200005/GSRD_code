# 02_make_manuscript_figures.R
# Purpose:
#   Generate manuscript figures for the GSRD data paper.
#   This script merges the core plotting logic from earlier figure scripts
#   into one file and uses relative paths for GitHub-friendly execution.
#
# Expected main input:
#   data/GSRD_main.csv
#
# Optional auxiliary inputs:
#   data/auxiliary/global_climate_space.csv   # columns: MAP, Tg, percent_bin
#   data/auxiliary/site_climate.csv           # columns: MAP, Tg, PFT
#   data/auxiliary/sptrait_plot.csv           # for Figure 3-style phylogeny trait plot
#
# Output directory:
#   outputs/figures/

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(ggplot2)
  library(scales)
  library(patchwork)
  library(hexbin)
  library(ggExtra)
  library(sf)
  library(rnaturalearth)
})

input_file <- "data/GSRD_main.csv"
aux_dir    <- "data/auxiliary"
output_dir <- "outputs/figures"

dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

if (!file.exists(input_file)) {
  stop("Input file not found: ", input_file)
}

gsrd <- readr::read_csv(input_file, show_col_types = FALSE)

required_cols <- c(
  "LAT", "LON", "PFT", "GROUP", "SEASONAL", "MEASUREMENT",
  "MAP", "Tg", "LNRS25", "RS_MASS25"
)
missing_cols <- setdiff(required_cols, names(gsrd))
if (length(missing_cols) > 0) {
  stop("Missing required columns: ", paste(missing_cols, collapse = ", "))
}

# -------------------------
# Helper functions
# -------------------------
theme_gsrd <- function() {
  theme_bw() +
    theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.border = element_rect(fill = NA, colour = "black", linewidth = 1),
      axis.title = element_text(size = 13),
      axis.text = element_text(size = 10),
      legend.title = element_blank()
    )
}

make_measurement_category <- function(measurement, seasonal) {
  dplyr::case_when(
    measurement == "LAB" ~ "Lab",
    measurement == "FIELD" & seasonal == "Y" ~ "Field & multiple seasons",
    measurement == "FIELD" ~ "Field",
    TRUE ~ NA_character_
  )
}

save_plot <- function(plot_obj, filename, width, height, dpi = 300) {
  ggsave(
    filename = file.path(output_dir, filename),
    plot = plot_obj,
    width = width,
    height = height,
    dpi = dpi
  )
}

# -------------------------
# Data preparation
# -------------------------
gsrd <- gsrd %>%
  mutate(
    measurement_category = make_measurement_category(MEASUREMENT, SEASONAL),
    PFT = factor(
      PFT,
      levels = c(
        "Evergreen Needle-leaved",
        "Deciduous Needle-leaved",
        "Deciduous Broadleaved",
        "Evergreen Broadleaved"
      )
    ),
    GROUP = factor(GROUP, levels = c("Angiosperms", "Gymnosperms")),
    measurement_category = factor(
      measurement_category,
      levels = c("Field", "Field & multiple seasons", "Lab")
    )
  )

# -------------------------
# Figure 1A: global site distribution
# -------------------------
unique_site <- gsrd %>%
  filter(!is.na(LAT), !is.na(LON), !is.na(measurement_category)) %>%
  group_by(LAT, LON, measurement_category) %>%
  summarise(.groups = "drop")

world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")
site_sf <- st_as_sf(unique_site, coords = c("LON", "LAT"), crs = st_crs(world))

p1a <- ggplot() +
  geom_sf(data = world, fill = "white", colour = "grey40", linewidth = 0.25) +
  geom_sf(
    data = site_sf,
    aes(fill = measurement_category),
    shape = 21, colour = "black", size = 2, stroke = 0.2
  ) +
  coord_sf(expand = FALSE) +
  labs(x = "Longitude", y = "Latitude") +
  theme_bw() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "right",
    legend.title = element_blank()
  )

save_plot(p1a, "Figure1A_site_distribution.png", width = 9, height = 4.8)

# -------------------------
# Figure 1B/C: optional climate-space panels
# -------------------------
global_climate_file <- file.path(aux_dir, "global_climate_space.csv")
site_climate_file   <- file.path(aux_dir, "site_climate.csv")

if (file.exists(global_climate_file) && file.exists(site_climate_file)) {
  climate_bg <- readr::read_csv(global_climate_file, show_col_types = FALSE)
  site_climate <- readr::read_csv(site_climate_file, show_col_types = FALSE)

  p1b <- ggplot() +
    geom_point(
      data = climate_bg,
      aes(x = Tg, y = MAP, alpha = percent_bin),
      shape = 15, colour = "grey30", size = 2
    ) +
    scale_alpha_continuous(range = c(0.2, 1), guide = "none") +
    geom_point(
      data = site_climate,
      aes(x = Tg, y = MAP),
      colour = "red", size = 1.5
    ) +
    labs(x = "Tg (°C)", y = "MAP (mm yr-1)") +
    theme_gsrd()

  p1c <- ggplot(site_climate, aes(x = Tg, y = MAP, fill = PFT)) +
    geom_point(shape = 21, colour = "white", size = 2.5, stroke = 0.2) +
    labs(x = "Tg (°C)", y = "MAP (mm yr-1)") +
    theme_gsrd()

  save_plot(p1b + p1c, "Figure1BC_climate_space.png", width = 10, height = 4.5)
} else {
  message("Skipping Figure 1B/C: optional auxiliary climate files not found.")
}

# -------------------------
# Figure 2: distributions across categories
# -------------------------
rsdata_single <- gsrd %>%
  filter(SEASONAL == "N")

count_data_pft <- rsdata_single %>%
  filter(!is.na(PFT), !is.na(LNRS25)) %>%
  group_by(PFT) %>%
  summarise(n = n(), .groups = "drop")

count_data_group <- rsdata_single %>%
  filter(!is.na(GROUP), !is.na(LNRS25)) %>%
  group_by(GROUP) %>%
  summarise(n = n(), .groups = "drop")

count_data_measurement <- gsrd %>%
  filter(!is.na(measurement_category), !is.na(LNRS25)) %>%
  group_by(measurement_category) %>%
  summarise(n = n(), .groups = "drop")

y_min <- floor(min(gsrd$LNRS25, na.rm = TRUE))
y_max <- ceiling(max(gsrd$LNRS25, na.rm = TRUE))

p2a <- ggplot(rsdata_single, aes(x = PFT, y = LNRS25, fill = PFT)) +
  geom_violin(trim = FALSE, colour = "grey20") +
  geom_boxplot(width = 0.12, fill = "white", outlier.shape = NA) +
  geom_text(
    data = count_data_pft,
    aes(x = PFT, y = y_min + 0.3, label = n),
    inherit.aes = FALSE,
    size = 4
  ) +
  annotate("text", x = 0.6, y = y_min + 0.3, label = "(N)", size = 4) +
  labs(
    x = "Plant Functional Type",
    y = expression(paste("ln ", RS[MASS25], " (nmol ", CO[2], " g"^-1, " s"^-1, ")"))
  ) +
  coord_cartesian(ylim = c(y_min - 0.5, y_max + 0.5)) +
  theme_gsrd() +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5))

p2b <- ggplot(rsdata_single, aes(x = GROUP, y = LNRS25, fill = GROUP)) +
  geom_violin(trim = FALSE, colour = "grey20") +
  geom_boxplot(width = 0.12, fill = "white", outlier.shape = NA) +
  geom_text(
    data = count_data_group,
    aes(x = GROUP, y = y_min + 0.3, label = n),
    inherit.aes = FALSE,
    size = 4
  ) +
  labs(x = "Taxonomic group", y = NULL) +
  coord_cartesian(ylim = c(y_min - 0.5, y_max + 0.5)) +
  theme_gsrd()

p2c <- ggplot(gsrd, aes(x = measurement_category, y = LNRS25, fill = measurement_category)) +
  geom_violin(trim = FALSE, colour = "grey20") +
  geom_boxplot(width = 0.12, fill = "white", outlier.shape = NA) +
  geom_text(
    data = count_data_measurement,
    aes(x = measurement_category, y = y_min + 0.3, label = n),
    inherit.aes = FALSE,
    size = 4
  ) +
  labs(x = "Measurement", y = NULL) +
  coord_cartesian(ylim = c(y_min - 0.5, y_max + 0.5)) +
  theme_gsrd()

fig2 <- p2a + p2b + p2c + plot_annotation(tag_levels = "a")
save_plot(fig2, "Figure2_distributions.png", width = 13, height = 5)

# -------------------------
# Figure 3: optional trait matrix / phylogeny panel
# -------------------------
sptrait_file <- file.path(aux_dir, "sptrait_plot.csv")
if (file.exists(sptrait_file)) {
  sptrait <- readr::read_csv(sptrait_file, show_col_types = FALSE)

  p3a <- ggplot(sptrait) +
    geom_point(
      aes(
        x = control,
        y = -order,
        fill = fill_rsarea,
        size = log10(n_rsarea + 1)
      ),
      shape = 21, alpha = 0.8, colour = "red"
    ) +
    scale_fill_gradient(low = "red", high = "grey") +
    coord_fixed() +
    theme_gsrd() +
    theme(axis.title = element_blank())

  p3b <- ggplot(sptrait) +
    geom_point(
      aes(
        x = control,
        y = -order,
        fill = fill_rsvol,
        size = log10(n_rsvol + 1)
      ),
      shape = 21, alpha = 0.8, colour = "red"
    ) +
    scale_fill_gradient(low = "red", high = "grey") +
    coord_fixed() +
    theme_gsrd() +
    theme(axis.title = element_blank())

  p3c <- ggplot(sptrait) +
    geom_point(
      aes(
        x = control,
        y = -order,
        fill = fill_rsmass,
        size = log10(n_rsmass + 1)
      ),
      shape = 21, alpha = 0.8, colour = "red"
    ) +
    scale_fill_gradient(low = "red", high = "grey") +
    coord_fixed() +
    theme_gsrd() +
    theme(axis.title = element_blank())

  p3d <- ggplot(sptrait) +
    geom_point(
      aes(
        x = control,
        y = -order,
        fill = log(mean_rs25mass),
        size = log10(n_rs25mass + 1)
      ),
      shape = 21, alpha = 0.8, colour = "black"
    ) +
    scale_fill_gradient(low = "#FBFEE1", high = "#EA9155") +
    coord_fixed() +
    theme_gsrd() +
    theme(axis.title = element_blank())

  fig3 <- p3a + p3b + p3c + p3d + plot_layout(ncol = 4)
  save_plot(fig3, "Figure3_phylogeny_trait_matrix.png", width = 14, height = 5)
} else {
  message("Skipping Figure 3: optional sptrait_plot.csv not found.")
}

# -------------------------
# Figure 4: climate relationships
# -------------------------
rsdata_field_single <- gsrd %>%
  filter(SEASONAL == "N", MEASUREMENT == "FIELD") %>%
  filter(!is.na(MAP), !is.na(Tg), !is.na(LNRS25))

p4a <- ggplot(rsdata_field_single, aes(MAP, LNRS25)) +
  geom_hex(colour = "white", bins = 20) +
  scale_fill_fermenter(palette = "Blues", direction = 1) +
  labs(x = "MAP (mm yr-1)", y = expression(paste("ln ", RS[MASS25]))) +
  theme_gsrd()

p4b <- ggplot(rsdata_field_single, aes(Tg, LNRS25)) +
  geom_hex(colour = "white", bins = 20) +
  scale_fill_fermenter(palette = "Blues", direction = 1) +
  labs(x = "Tg (°C)", y = expression(paste("ln ", RS[MASS25]))) +
  theme_gsrd()

# ggMarginal returns grobs, so save separately
p4a_marginal <- ggMarginal(p4a, type = "histogram", size = 3, fill = "#3D82AC", colour = "white")
p4b_marginal <- ggMarginal(p4b, type = "histogram", size = 3, fill = "#3D82AC", colour = "white")

ggsave(file.path(output_dir, "Figure4A_lnRS25_vs_MAP.png"), p4a_marginal, width = 6, height = 5, dpi = 300)
ggsave(file.path(output_dir, "Figure4B_lnRS25_vs_Tg.png"), p4b_marginal, width = 6, height = 5, dpi = 300)

message("Figure outputs written to: ", normalizePath(output_dir))

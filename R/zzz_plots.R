
# load packages
library("dplyr")
library("ggplot2")
library("ggthemes")


# Utility functions ------------------------------------------------------------
# function for parsing axis labels
parse_labels <- function(labels, ...) parse(text = labels, ...)


# Colors and labels for plots --------------------------------------------------

# colors to be used in plots
colors_ICS <- ggthemes::colorblind_pal()(4)[c(3, 1, 2, 4)]
names(colors_ICS) <- c("Oracle", "Var", "Med", "Normal")
colors_PCA <- scales::hue_pal()(4)[c(4, 3)]
names(colors_PCA) <- c("80%", "k-1")
colors <- c(colors_ICS, colors_PCA)

# labels to be used in plots
clusters_label <- "Cluster sizes"
criterion_label <- "Component selection"


# Import results for dimension reduction ---------------------------------------

# import results and renaming certain variables, dimension reduction methods, component selection
# criteria, etc.
# load file
load("results/results_r=5.RData")
res_df <- results %>%
  # rename some more variables for consistency
  # rename(number = Number, q = k) %>%
  rename(outliers = epsilon) %>%
  # recode names of cluster settings, scatter matrices, and selection criteria
  mutate(outliers = ifelse(outliers == 0, "No outliers",
                           sprintf("%d%% outliers", 100 * outliers)),
         criterion = recode(criterion,
                            "discriminatory" = "Oracle",
                            "var" = "Var",
                            "med" = "Med",
                            "normal" = "Normal"),
         method = recode(method, "pam" = "PAM")
  ) %>%
  # add variable indicating ICS or PCA
  mutate(type = ifelse(scatter %in% c("COV", "RMCD[0.75]"), "PCA", "ICS"))
# TODO: add if type = initial?

# evaluation measure to be plotted
measure <- "eta2"
measure_label <- "Discriminatory power"


# Determine for which methods to take a closer look ----------------------------

# filter on component selection criteria
keep_outliers <- "No outliers"
keep_crit <- c("Oracle", "Med", "Normal", "80%", "k-1")
res_df_overview <- res_df %>%
  filter(outliers %in% keep_outliers,
         criterion %in% keep_crit)

# order methods by average variance based on different selection criteria
# (this ordering is not used directly, but serves as a basis for a manual
# ordering in the plot with an overview of all dimension reduction methods
# and all component selection criteria)
order_df <- res_df_overview %>%
  filter(criterion != "Oracle") %>%
  group_by(scatter, criterion) %>%
  summarize(variance = var(.data[[measure]], na.rm = TRUE),
            .groups = "drop") %>%
  group_by(scatter) %>%
  summarize(variance = min(variance),
            .groups = "drop") %>%
  arrange(variance)
order_df$scatter

# order of dimension reduction methods in plot
scatter_ordered <- c(
  # best performing scatter pairs for ICS
  "LCOV-COV", "TCOV-COV", "TCOV-UCOV",
  # MCD-COV scatter pairs
  sprintf("MCD[0.%d]-COV", c(10, 20, 25, 50, 75)),
  sprintf("RMCD[0.%d]-COV", c(10, 20, 25, 50, 75)),
  # MCD-MCD scatter pairs
  "MCD[0.25]-MCD[0.95]", "RMCD[0.25]-RMCD[0.95]",
  # other scatter pairs for ICS
  "MLC-COV", "COV-COV[4]",
  # scatter matrices for PCA
  "RMCD[0.75]", "COV"
)

# create plot
text_size_factor <- 10/6.5
res_df_overview %>%
  mutate(scatter = factor(scatter, levels = scatter_ordered),
         criterion = factor(criterion, levels = keep_crit)) %>%
  ggplot(mapping = aes_string(x = "scatter", y = measure, color = "criterion",
                              fill = "criterion")) +
  geom_boxplot(alpha = 0.4, position = position_dodge2(reverse = TRUE)) +
  coord_flip() +
  scale_x_discrete(limits = rev, labels = parse_labels) +
  scale_color_manual(criterion_label,  values = colors[keep_crit]) +
  scale_fill_manual(criterion_label, values = colors[keep_crit]) +
  theme_bw() +
  theme(axis.title = element_text(size = 11 * text_size_factor),
        axis.text = element_text(size = 9 * text_size_factor),
        legend.title = element_text(size = 11 * text_size_factor),
        legend.text = element_text(size = 9 * text_size_factor),
        strip.text = element_text(size = 10 * text_size_factor)) +
  labs(x = NULL, y = measure_label) +
  facet_grid(type ~ ., scales = "free_y", space = "free_y")

# save plot to file (pdf)
# file_plot = sprintf("figures/reduc_dim/%s_scatter_all.pdf", measure)
# ggsave(file_plot, width = 10, height = 9.5)

# save plot to file (png)
# file_plot = sprintf("figures/reduc_dim/%s_scatter_all.png", measure)
# ggsave(file_plot, width = 10, height = 9.5, unit = "in", dpi = 250)


# Evaluation of stability of selection of components ---------------------------

# We use the measure of variation for categorical data as discussed in
# Agresti (1990; p.24). This measure is 0 if we always select the same
# components, and it has the maximum value (M-1)/M if the M options are
# selected uniformly. Since we have different possibilities for different
# number of clusters, we normalize the measure to have maximum value 1.
# Here, due to the way we select the components, the number of choices is
# the same as the number of clusters.

# selection criteria to keep
keep_outliers <- "No outliers"
keep_crit <- c("Oracle",  "Med", "Normal")

# compute how often certain components were selected
# TODO: not possible
res_df_selected <- res_df %>%
  filter(outliers %in% keep_outliers,
         criterion %in% keep_crit) %>%
  group_by(n, p, q, clusters, outliers, criterion, scatter, selected) %>%
  summarize(nb_selected = n(), .groups = "drop")

# compute normalized variation in which components are selected

res_df_variation <- res_df_selected %>%
  #group_by(n, p, q, clusters, outliers, criterion, scatter) %>%
  group_by( q, clusters, outliers, criterion, scatter) %>%
  summarize(nb_iter = sum(nb_selected),
            variation = 1 - sum((nb_selected / nb_iter)^2),
            .groups = "drop") %>%
  mutate(normalization = ifelse(criterion == "Normal",
                                (1 + choose(p+1, 2)) / choose(p+1, 2),
                                q / (q-1)),
         normalized_variation = normalization * variation)

# create plot
set.seed(20221020)  # make jitter reproducible
res_df_variation %>%
  mutate(scatter = factor(scatter, levels = scatter_ordered),
         criterion = factor(criterion, levels = keep_crit)) %>%
  ggplot(mapping = aes(x = scatter, y = normalized_variation,
                       color = criterion, fill = criterion)) +
  geom_jitter(color = "grey", size = 2/3, alpha = 0.5) +
  geom_boxplot(alpha = 0.4) +
  coord_flip() +
  scale_x_discrete(limits = rev, labels = parse_labels) +
  scale_color_manual(values = colors[keep_crit]) +
  scale_fill_manual(values = colors[keep_crit]) +
  theme_bw() +
  theme(axis.title = element_text(size = 11),
        axis.text = element_text(size = 9),
        axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
        legend.position = "none",
        legend.title = element_text(size = 11),
        legend.text = element_text(size = 9),
        strip.text = element_text(size = 10)) +
  labs(x = NULL, y = "Normalized variation in selection of components") +
  facet_grid(. ~ criterion)

# save plot to file
file_plot <- "figures/reduc_dim/variation_component_selection.pdf"
ggsave(file_plot, width = 6.5, height = 4)


# Discriminatory power of best performing dimension reduction methods ----------

# which methods to select
keep_outliers <- c("No outliers", "2% outliers", "5% outliers")
keep_crit <- c("Oracle", "Med", "80%")
keep_scatter <- c("LCOV-COV", "TCOV-COV", "TCOV-UCOV", "RMCD[0.75]")
keep_clusters <- c("50-50", "70-30", "80-20", "90-10", "95-5",
                   "33-33-33", "20-50-30", "10-80-10",
                   "20-20-20-20-20", "10-10-20-20-40")

## overall results across cluster settings

# select subset of methods
res_df_selected <- res_df %>%
  filter(outliers %in% keep_outliers,
         criterion %in% keep_crit,
         scatter %in% keep_scatter)

# create plot
res_df_selected %>%
  mutate(outliers = factor(outliers, levels = keep_outliers),
         criterion = factor(criterion, levels = keep_crit),
         scatter = factor(scatter, levels = keep_scatter)) %>%
  ggplot(mapping = aes_string(x = "scatter", y = measure, color = "criterion",
                              fill = "criterion")) +
  # geom_boxplot(alpha = 0.4, position = position_dodge2(reverse = TRUE)) +
  # coord_flip() +
  # scale_x_discrete(limits = rev, labels = parse_labels) +
  geom_boxplot(alpha = 0.4) +
  scale_x_discrete(labels = parse_labels) +
  scale_color_manual(criterion_label, values = colors[keep_crit]) +
  scale_fill_manual(criterion_label, values = colors[keep_crit]) +
  theme_bw() +
  theme(axis.title = element_text(size = 11),
        axis.text = element_text(size = 9),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
        legend.title = element_text(size = 11),
        legend.text = element_text(size = 9)) +
  labs(x = NULL, y = measure_label) +
  facet_grid(. ~ outliers)

# save plot to file
file_plot = sprintf("figures/reduc_dim/%s_scatter_best.pdf", measure)
ggsave(file_plot, width = 6.5, height = 3)

file_plot = sprintf("figures/reduc_dim/%s_scatter_best.png", measure)
ggsave(file_plot, width = 6.5, height = 3, unit = "in", dpi = 250)

## results for selected cluster settings

# select subset of cluster settings
res_df_clusters <- res_df_selected %>%
  filter(clusters %in% keep_clusters)

# create plot
text_size_factor <- 8/6.5
res_df_clusters %>%
  mutate(outliers = factor(outliers, levels = keep_outliers),
         clusters = factor(clusters, levels = keep_clusters),
         criterion = factor(criterion, levels = keep_crit),
         scatter = factor(scatter, levels = keep_scatter)) %>%
  ggplot(mapping = aes_string(x = "clusters", y = measure,
                              color = "criterion", fill = "criterion")) +
  geom_boxplot(alpha = 0.4) +
  scale_color_manual(criterion_label, values = colors[keep_crit]) +
  scale_fill_manual(criterion_label, values = colors[keep_crit]) +
  theme_bw() +
  theme(axis.title = element_text(size = 11 * text_size_factor),
        axis.text = element_text(size = 9 * text_size_factor),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
        legend.position = "top",
        legend.title = element_text(size = 11 * text_size_factor),
        legend.text = element_text(size = 9 * text_size_factor),
        panel.spacing.y = unit(0.5, "lines"),
        strip.text = element_text(size = 10 * text_size_factor)) +
  labs(x = clusters_label, y = measure_label) +
  facet_grid(outliers ~ scatter, labeller = labeller(scatter = label_parsed))

# save plot to file
file_plot = sprintf("figures/reduc_dim/%s_scatter_best_clusters.pdf", measure)
ggsave(file_plot, width = 8, height = 8)
file_plot = sprintf("figures/reduc_dim/%s_scatter_best_clusters.png", measure)
ggsave(file_plot, width = 8, height = 8, unit = "in", dpi = 250)

# Discriminatory power of MCD-COV scatter pairs --------------------------------

# which methods to select
keep_outliers <- c("No outliers", "2% outliers", "5% outliers")
keep_crit <- c("Oracle", "Normal")
keep_scatter <- sprintf("MCD[0.%d]-COV", c(10, 25, 50))
keep_clusters <- c("50-50", "55-45", "60-40", "70-30", "80-20", "90-10", "95-5",
                   "33-33-33", "10-80-10", "20-20-20-20-20", "10-10-20-20-40")

## overall results across cluster settings

# select subset of methods
res_df_selected <- res_df %>%
  filter(outliers %in% keep_outliers,
         criterion %in% keep_crit,
         scatter %in% keep_scatter)

# create plot
res_df_selected %>%
  mutate(outliers = factor(outliers, levels = keep_outliers),
         criterion = factor(criterion, levels = keep_crit),
         scatter = factor(scatter, levels = keep_scatter)) %>%
  ggplot(mapping = aes_string(x = "scatter", y = measure, color = "criterion",
                              fill = "criterion")) +
  # geom_boxplot(alpha = 0.4, position = position_dodge2(reverse = TRUE)) +
  # coord_flip() +
  # scale_x_discrete(limits = rev, labels = parse_labels) +
  geom_boxplot(alpha = 0.4) +
  scale_x_discrete(labels = parse_labels) +
  scale_color_manual(criterion_label, values = colors[keep_crit]) +
  scale_fill_manual(criterion_label, values = colors[keep_crit]) +
  theme_bw() +
  theme(axis.title = element_text(size = 11),
        axis.text = element_text(size = 9),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
        legend.title = element_text(size = 11),
        legend.text = element_text(size = 9)) +
  labs(x = NULL, y = measure_label) +
  facet_grid(. ~ outliers)

# save plot to file
file_plot = sprintf("figures/reduc_dim/%s_scatter_MCD.pdf", measure)
ggsave(file_plot, width = 6.5, height = 3)

file_plot = sprintf("figures/reduc_dim/%s_scatter_MCD.png", measure)
ggsave(file_plot, width = 6.5, height = 3, unit = "in", dpi = 250)


## results for selected cluster settings

# select subset of cluster settings
res_df_clusters <- res_df_selected %>%
  filter(clusters %in% keep_clusters)

# create plot
text_size_factor <- 8/6.5
res_df_clusters %>%
  mutate(outliers = factor(outliers, levels = keep_outliers),
         clusters = factor(clusters, levels = keep_clusters),
         criterion = factor(criterion, levels = keep_crit),
         scatter = factor(scatter, levels = keep_scatter)) %>%
  ggplot(mapping = aes_string(x = "clusters", y = measure,
                              color = "criterion", fill = "criterion")) +
  geom_boxplot(alpha = 0.4) +
  scale_color_manual(criterion_label, values = colors[keep_crit]) +
  scale_fill_manual(criterion_label, values = colors[keep_crit]) +
  theme_bw() +
  theme(axis.title = element_text(size = 11 * text_size_factor),
        axis.text = element_text(size = 9 * text_size_factor),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
        legend.position = "top",
        legend.title = element_text(size = 11 * text_size_factor),
        legend.text = element_text(size = 9 * text_size_factor),
        strip.text = element_text(size = 10 * text_size_factor)) +
  labs(x = clusters_label, y = measure_label) +
  facet_grid(outliers ~ scatter, labeller = labeller(scatter = label_parsed))

# save plot to file
file_plot = sprintf("figures/reduc_dim/%s_scatter_MCD_clusters.pdf", measure)
ggsave(file_plot, width = 8, height = 8)

file_plot = sprintf("figures/reduc_dim/%s_scatter_MCD_clusters.png", measure)
ggsave(file_plot, width = 8, height = 8, unit = "in", dpi = 250)

# Discriminatory power of COV-COV4  scatter pair -------------------------------

# which methods to select
keep_outliers <- c("No outliers", "2% outliers", "5% outliers")
keep_crit <- c("Oracle", "Med")
keep_scatter <- "COV-COV[4]"

# select subset of cluster settings
res_df_clusters <- res_df %>%
  filter(outliers %in% keep_outliers,
         criterion %in% keep_crit,
         scatter %in% keep_scatter)

# order cluster settings according to number of clusters and variation in
# cluster sizes
cluster_df <- unique(res_df_clusters[, c("q", "clusters")])
cluster_list <- split(cluster_df, cluster_df$q)
order_list <- lapply(cluster_list, function(df) {
  mat <- stringr::str_split(df$clusters, pattern = "-", simplify = TRUE)
  mode(mat) <- "numeric"  # keeps information on dimensions
  variation <- apply(mat, 1, function(x) 1 - sum((x/100)^2))
  cbind(df, variation)
})
order_df <- do.call(rbind, order_list)
cluster_order <- order(order_df$q, -order_df$variation)
cluster_levels <- order_df$clusters[cluster_order]

# create plot
text_size_factor <- 8/6.5
res_df_clusters %>%
  mutate(outliers = factor(outliers, levels = keep_outliers),
         clusters = factor(clusters, levels = cluster_levels),
         criterion = factor(criterion, levels = keep_crit),
         scatter = factor(scatter, levels = keep_scatter)) %>%
  ggplot(mapping = aes_string(x = "clusters", y = measure,
                              color = "criterion", fill = "criterion")) +
  geom_boxplot(alpha = 0.4) +
  scale_color_manual(criterion_label, values = colors[keep_crit]) +
  scale_fill_manual(criterion_label, values = colors[keep_crit]) +
  theme_bw() +
  theme(axis.title = element_text(size = 11 * text_size_factor),
        axis.text = element_text(size = 9 * text_size_factor),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
        legend.position = "top",
        legend.title = element_text(size = 11 * text_size_factor),
        legend.text = element_text(size = 9 * text_size_factor),
        strip.text = element_text(size = 10 * text_size_factor)) +
  labs(x = clusters_label, y = measure_label) +
  facet_grid(outliers ~ scatter, labeller = labeller(scatter = label_parsed))

# save plot to file
file_plot = sprintf("figures/reduc_dim/%s_scatter_COV-COV4_clusters.pdf",
                    measure)
ggsave(file_plot, width = 8, height = 8)
file_plot = sprintf("figures/reduc_dim/%s_scatter_COV-COV4_clusters.png",
                    measure)
ggsave(file_plot, width = 8, height = 8, unit = "in", dpi = 250)

# check selected components
df_selection <- res_df_clusters %>%
  mutate(outliers = factor(outliers, levels = keep_outliers),
         clusters = factor(clusters, levels = cluster_levels),
         criterion = factor(criterion, levels = keep_crit),
         scatter = factor(scatter, levels = keep_scatter),
         selected = as.factor(selected)) %>%
  group_by(outliers, clusters, criterion, scatter, selected) %>%
  summarize(Frequency = n(),
            .groups = "drop") %>%
  tidyr::pivot_wider(names_from = criterion, values_from = Frequency)


# Import results for clustering ------------------------------------------------

# evaluation measure to be plotted
measure <- "ARI"
measure_label <- "Adjusted Rand index"

# additional color to be used in plots
color_NA <- "black"


# ARI of best performing dimension reduction methods ---------------------------

# which methods to select
keep_outliers <- c("No outliers", "2% outliers", "5% outliers")
keep_crit <- c("Med", "80%")
keep_scatter <- c("Observed~data", "LCOV-COV", "TCOV-COV", "TCOV-UCOV",
                  "RMCD[0.75]")
keep_clusters <- c("50-50", "70-30", "80-20", "90-10",
                   "33-33-33", "20-50-30", "10-80-10",
                   "20-20-20-20-20", "10-10-20-20-40")
keep_method <- c("PAM", "kmeans", "tkmeans")

## overall results across cluster settings

# select subset of methods
res_df_selected <- res_df %>%
  filter(outliers %in% keep_outliers,
         is.na(criterion) | criterion %in% keep_crit,
         scatter %in% keep_scatter,
         method %in% keep_method)

# create plot
text_size_factor <- 8/6.5
res_df_selected %>%
  mutate(outliers = factor(outliers, levels = keep_outliers),
         criterion = factor(criterion, levels = keep_crit),
         scatter = factor(scatter, levels = keep_scatter),
         method = factor(method, levels = keep_method)) %>%
  ggplot(mapping = aes_string(x = "scatter", y = measure, color = "criterion",
                              fill = "criterion")) +
  # geom_boxplot(alpha = 0.4, position = position_dodge2(reverse = TRUE)) +
  # coord_flip() +
  # scale_x_discrete(limits = rev, labels = parse_labels) +
  geom_boxplot(alpha = 0.4) +
  scale_x_discrete(labels = parse_labels) +
  scale_color_manual(criterion_label, values = colors[keep_crit],
                     na.value = color_NA) +
  scale_fill_manual(criterion_label, values = colors[keep_crit],
                    na.value = color_NA) +
  theme_bw() +
  theme(axis.title = element_text(size = 11 * text_size_factor),
        axis.text = element_text(size = 9 * text_size_factor),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
        legend.title = element_text(size = 11 * text_size_factor),
        legend.text = element_text(size = 9 * text_size_factor)) +
  labs(x = NULL, y = measure_label) +
  facet_grid(method ~ outliers)

# save plot to file
file_plot = sprintf("figures/clustering/%s_scatter_best.pdf", measure)
ggsave(file_plot, width = 8, height = 6)
file_plot = sprintf("figures/clustering/%s_scatter_best.png", measure)
ggsave(file_plot, width = 8, height = 6, unit = "in", dpi = 250)

## results for selected cluster settings

# loop over contamination level and create plot
outlier_suffix <- c("no_outliers", "0.02_outliers", "0.05_outliers")
for (i in seq_along(keep_outliers)) {

  # select subset of cluster settings
  res_df_clusters <- res_df_selected %>%
    filter(outliers %in% keep_outliers[i],
           clusters %in% keep_clusters)

  # create plot
  text_size_factor <- 8/6.5
  res_df_clusters %>%
    mutate(clusters = factor(clusters, levels = cluster_levels),
           criterion = factor(criterion, levels = keep_crit),
           scatter = factor(scatter, levels = keep_scatter),
           method = factor(method, levels = keep_method)) %>%
    ggplot(mapping = aes_string(x = "clusters", y = measure,
                                color = "criterion", fill = "criterion")) +
    geom_boxplot(alpha = 0.4) +
    scale_color_manual(criterion_label, values = colors[keep_crit],
                       na.value = color_NA) +
    scale_fill_manual(criterion_label, values = colors[keep_crit],
                      na.value = color_NA) +
    theme_bw() +
    theme(axis.title = element_text(size = 11 * text_size_factor),
          axis.text = element_text(size = 9 * text_size_factor),
          axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
          legend.position = "top",
          legend.title = element_text(size = 11 * text_size_factor),
          legend.text = element_text(size = 9 * text_size_factor),
          strip.text = element_text(size = 10 * text_size_factor)) +
    labs(x = clusters_label, y = measure_label) +
    facet_grid(method ~ scatter, labeller = label_parsed)

  # save plot to file
  # file_plot = sprintf("figures/clustering/%s_scatter_best_clusters_%s.pdf",
  #                     measure, outlier_suffix[i])
  # ggsave(file_plot, width = 8, height = 7)

  file_plot = sprintf("figures/clustering/%s_scatter_best_clusters_%s.png",
                      measure, outlier_suffix[i])
  ggsave(file_plot, width = 8, height = 7, unit = "in", dpi = 250)

}


# ARI of MCD-COV scatter pairs -------------------------------------------------

# which methods to select
keep_outliers <- c("No outliers", "2% outliers", "5% outliers")
keep_crit <- "Normal"
keep_scatter <- c("Observed~data", sprintf("MCD[0.%d]-COV", c(10, 25, 50)))
keep_clusters <- c("50-50", "55-45", "60-40", "70-30", "80-20", "90-10", "95-5",
                   "33-33-33", "10-80-10", "20-20-20-20-20", "10-10-20-20-40")
keep_method <- c("PAM", "kmeans", "tkmeans")

## overall results across cluster settings

# select subset of methods
res_df_selected <- res_df %>%
  filter(outliers %in% keep_outliers,
         is.na(criterion) | criterion %in% keep_crit,
         scatter %in% keep_scatter,
         method %in% keep_method)

# create plot
text_size_factor <- 8/6.5
res_df_selected %>%
  mutate(outliers = factor(outliers, levels = keep_outliers),
         criterion = factor(criterion, levels = keep_crit),
         scatter = factor(scatter, levels = keep_scatter),
         method = factor(method, levels = keep_method)) %>%
  ggplot(mapping = aes_string(x = "scatter", y = measure, color = "criterion",
                              fill = "criterion")) +
  # geom_boxplot(alpha = 0.4, position = position_dodge2(reverse = TRUE)) +
  # coord_flip() +
  # scale_x_discrete(limits = rev, labels = parse_labels) +
  geom_boxplot(alpha = 0.4) +
  scale_x_discrete(labels = parse_labels) +
  scale_color_manual(criterion_label, values = colors[keep_crit],
                     na.value = color_NA) +
  scale_fill_manual(criterion_label, values = colors[keep_crit],
                    na.value = color_NA) +
  theme_bw() +
  theme(axis.title = element_text(size = 11 * text_size_factor),
        axis.text = element_text(size = 9 * text_size_factor),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
        legend.title = element_text(size = 11 * text_size_factor),
        legend.text = element_text(size = 9 * text_size_factor)) +
  labs(x = NULL, y = measure_label) +
  facet_grid(method ~ outliers)

# save plot to file
file_plot = sprintf("figures/clustering/%s_scatter_MCD.pdf", measure)
ggsave(file_plot, width = 8, height = 6)
file_plot = sprintf("figures/clustering/%s_scatter_MCD.png", measure)
ggsave(file_plot, width = 8, height = 6, unit = "in", dpi = 250)

## results for selected cluster settings

# loop over contamination level and create plot
outlier_suffix <- c("no_outliers", "0.02_outliers", "0.05_outliers")
for (i in seq_along(keep_outliers)) {

  # select subset of cluster settings
  res_df_clusters <- res_df_selected %>%
    filter(outliers %in% keep_outliers[i],
           clusters %in% keep_clusters)

  # create plot
  text_size_factor <- 8/6.5
  res_df_clusters %>%
    mutate(clusters = factor(clusters, levels = cluster_levels),
           criterion = factor(criterion, levels = keep_crit),
           scatter = factor(scatter, levels = keep_scatter),
           method = factor(method, levels = keep_method)) %>%
    ggplot(mapping = aes_string(x = "clusters", y = measure,
                                color = "criterion", fill = "criterion")) +
    geom_boxplot(alpha = 0.4) +
    scale_color_manual(criterion_label, values = colors[keep_crit],
                       na.value = color_NA) +
    scale_fill_manual(criterion_label, values = colors[keep_crit],
                      na.value = color_NA) +
    theme_bw() +
    theme(axis.title = element_text(size = 11 * text_size_factor),
          axis.text = element_text(size = 9 * text_size_factor),
          axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
          legend.position = "top",
          legend.title = element_text(size = 11 * text_size_factor),
          legend.text = element_text(size = 9 * text_size_factor),
          strip.text = element_text(size = 10 * text_size_factor)) +
    labs(x = clusters_label, y = measure_label) +
    facet_grid(method ~ scatter, labeller = label_parsed)

  # save plot to file
  # file_plot = sprintf("figures/clustering/%s_scatter_MCD_clusters_%s.pdf",
  #                     measure, outlier_suffix[i])
  # ggsave(file_plot, width = 8, height = 7)
  file_plot = sprintf("figures/clustering/%s_scatter_MCD_clusters_%s.png",
                      measure, outlier_suffix[i])
  ggsave(file_plot, width = 8, height = 7,  unit = "in", dpi = 250)

}


# ARI of COV-COV4  scatter pair ------------------------------------------------

# which methods to select
keep_outliers <- c("No outliers", "2% outliers", "5% outliers")
keep_crit <- "Med"
keep_scatter <- c("Observed~data", "COV-COV[4]")

# select subset of cluster settings
res_df_selected <- res_df %>%
  filter(outliers %in% keep_outliers,
         is.na(criterion) | criterion %in% keep_crit,
         scatter %in% keep_scatter)

# loop over contamination level and create plot
outlier_suffix <- c("no_outliers", "0.02_outliers", "0.05_outliers")
for (i in seq_along(keep_outliers)) {

  # select subset of cluster settings
  res_df_clusters <- res_df_selected %>%
    filter(outliers %in% keep_outliers[i])

  # create plot
  text_size_factor <- 8/6.5
  res_df_clusters %>%
    mutate(clusters = factor(clusters, levels = cluster_levels),
           criterion = factor(criterion, levels = keep_crit),
           scatter = factor(scatter, levels = keep_scatter),
           method = factor(method, levels = keep_method)) %>%
    ggplot(mapping = aes_string(x = "clusters", y = measure,
                                color = "criterion", fill = "criterion")) +
    geom_boxplot(alpha = 0.4) +
    scale_color_manual(criterion_label, values = colors[keep_crit],
                       na.value = color_NA) +
    scale_fill_manual(criterion_label, values = colors[keep_crit],
                      na.value = color_NA) +
    theme_bw() +
    theme(axis.title = element_text(size = 11 * text_size_factor),
          axis.text = element_text(size = 9 * text_size_factor),
          axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
          legend.position = "top",
          legend.title = element_text(size = 11 * text_size_factor),
          legend.text = element_text(size = 9 * text_size_factor),
          strip.text = element_text(size = 10 * text_size_factor)) +
    labs(x = clusters_label, y = measure_label) +
    facet_grid(method ~ scatter, labeller = label_parsed)

  # save plot to file
  # file_plot = sprintf("figures/clustering/%s_scatter_COV-COV4_clusters_%s.pdf",
  #                     measure, outlier_suffix[i])
  # ggsave(file_plot, width = 8, height = 7.5)
  file_plot = sprintf("figures/clustering/%s_scatter_COV-COV4_clusters_%s.png",
                      measure, outlier_suffix[i])
  ggsave(file_plot, width = 8, height = 7.5,  unit = "in", dpi = 250)

}
Footer

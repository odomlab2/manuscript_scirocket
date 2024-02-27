# Author: J. van Riet
# Function: Plot benchmarks of the sci-rocket workflow.

# Import libraries. ----

library(dplyr)

# Import data. ----

files_benchmark <- list.files('/home/j103t/odomLab/manuscript_scirocket/workflow/benchmarks', full.names = T)

data_benchmark_raw <- dplyr::bind_rows(pbapply::pblapply(files_benchmark, function(x){
    data <- readr::read_tsv(x) %>%
        dplyr::mutate(
            step = gsub('_test_.*', '', basename(x)),
            step = gsub('_zebra.*|_mouse.*', '', step),
            step = gsub('_sx42b.*', '', step),
            experiment = dplyr::if_else(grepl('nextseq|zebrafish', x), 'Zebrafish (Hashing)', 'Haplotyping (F1 mice)'),
            experiment = dplyr::if_else(grepl('sx42b', x), 'FCG', experiment)
        )

    return(data)
}, cl = 20))

# Calc. mean + SE
data_benchmark <- data_benchmark_raw %>%
    dplyr::group_by(step, experiment) %>%
    dplyr::summarise(
        mean_m = mean(s / 60),
        sd_m = sd(s / 60),
        mean_io_in = mean(io_in / 1024),
        sd_io_in = sd(io_in / 1024),
        mean_io_out = mean(io_out / 1024),
        sd_io_out = sd(io_out / 1024),
        mean_max_rss = mean(max_rss / 1024),
        sd_max_rss = sd(max_rss / 1024),
        mean_mean_load = mean(mean_load / 100),
        sd_mean_load = sd(mean_load / 100)
    ) %>%
    dplyr::filter(experiment == 'FCG') %>%
    dplyr::mutate(
        step = factor(step, levels = c('bcl2fastq', 'split_R1', 'split_R2', 'demultiplex_fastq_split', 'gather_demultiplexed_sequencing', 'gather_demultiplexed_samples', 'trim_fastp', 'generate_index_STAR', 'starSolo_align', 'sambamba_index', 'sci_dash')),
        step = dplyr::recode_factor(
            step,
            bcl2fastq = 'Converting BCL (**bcl2fastq**)',
            split_R1 = "Splitting R1 into chunks",
            split_R2 = "Splitting R2 into chunks",
            demultiplex_fastq_split = "Barcode demultiplexing (on chunks)",
            gather_demultiplexed_sequencing = "Merging experiment-based files",
            gather_demultiplexed_samples = "Merging sample-based files",
            trim_fastp = "Trimming (**fastp**)",
            generate_index_STAR = "Generating alignment index (**STAR**)",
            starSolo_align = "Alignment and UMI counting (**STARSolo**)",
            sambamba_index = "Generating BAM indexes (**sambamba**)",
            sci_dash = "Generating interactive dashboard"
        ))

# Plot benchmarks
theme_job <- ggplot2::theme(
    text = ggplot2::element_text(),
    axis.text = ggtext::element_markdown(size = 8),
    axis.title.x = ggtext::element_markdown(size = 10, face = "bold"),
    axis.title.y = ggtext::element_markdown(size = 10, face = "bold"),
    legend.title = ggplot2::element_text(face = "bold"),
    legend.text = ggplot2::element_text(size = 10),
    legend.position = "bottom",
    legend.key.size = ggplot2::unit(0.1, "cm"),
    legend.key.width = ggplot2::unit(0.1, "cm"),
    legend.key.height = ggplot2::unit(0.1, "cm"),
    legend.key = ggplot2::element_rect(fill = "white", color = "black", linewidth = 0),
    strip.background = ggplot2::element_rect(fill = "grey50"),
    strip.text = ggplot2::element_text(color = "white"),
    panel.grid.major.y = ggplot2::element_line(color = "grey75", linetype = "dotted", linewidth = ggplot2::rel(.75)),
    panel.grid.minor = ggplot2::element_blank(),
    panel.background = ggplot2::element_blank(),
    plot.background = ggplot2::element_rect(colour = "white"),
    plot.margin = ggplot2::unit(c(10, 5, 5, 5), "mm"),
    axis.line = ggplot2::element_line(colour = "black", linewidth = ggplot2::rel(1))
)

colors <- hues::iwanthue(n=11)

plot1 <- ggplot2::ggplot(data_benchmark, mapping = ggplot2::aes(x = step, y = mean_m, fill = step, label = round(mean_m, 1))) +
    ggplot2::geom_bar(stat = 'identity', color = 'black', lwd = 0.3) +
    ggplot2::geom_errorbar(ggplot2::aes(ymin=mean_m-sd_m, ymax=mean_m+sd_m), color = 'black', width=.2, position=ggplot2::position_dodge(.9)) +
    ggplot2::labs(x = NULL, y = 'Runtime<br><sub>(in minutes)</sub>') +
    ggplot2::geom_text(nudge_y = 1, size = 2, fontface = "bold") +
    ggplot2::scale_fill_manual(values = colors, guide = 'none') +
    ggplot2::scale_y_continuous(limits = c(0, 600),breaks = seq(0, 600, 60)) +
    theme_job +
    ggplot2::theme(
        axis.text.x = ggplot2::element_blank()
    ) +
    ggplot2::facet_grid(~experiment)

plot2 <- ggplot2::ggplot(data_benchmark, mapping = ggplot2::aes(x = step, y = mean_mean_load, fill = step, label = round(mean_mean_load, 1))) +
    ggplot2::geom_bar(stat = 'identity', color = 'black', lwd = 0.3) +
    ggplot2::geom_errorbar(ggplot2::aes(ymin=mean_mean_load-sd_mean_load, ymax=mean_mean_load+sd_mean_load), width=.2, position=ggplot2::position_dodge(.9)) +
    ggplot2::labs(x = NULL, y = 'Mean load<br><sub>(CPU / runtime / 100)<sub>') +
    ggplot2::geom_text(nudge_y = 1, size = 2, fontface = "bold") +
    ggplot2::scale_fill_manual(values = colors, guide = 'none') +
    ggplot2::scale_y_continuous(limits = c(0, 30)) +
    theme_job +
    ggplot2::theme(
        strip.background = ggplot2::element_blank(),
        strip.text.x = ggplot2::element_blank(),
        axis.text.x = ggplot2::element_blank()
    ) +
    ggplot2::facet_grid(~experiment)

plot3 <- ggplot2::ggplot(data_benchmark, mapping = ggplot2::aes(x = step, y = mean_max_rss, fill = step, label = round(mean_max_rss, 1))) +
    ggplot2::geom_bar(stat = 'identity', color = 'black', lwd = 0.3) +
    ggplot2::geom_errorbar(ggplot2::aes(ymin=mean_max_rss-sd_max_rss, ymax=mean_max_rss+sd_max_rss), width=.2, position=ggplot2::position_dodge(.9)) +
    ggplot2::labs(x = NULL, y = 'Max. RSS<br><sub>(GB)</sub>') +
    ggplot2::geom_text(nudge_y = 1, size = 2, fontface = "bold") +
    ggplot2::scale_fill_manual(values = colors, guide = 'none') +
    ggplot2::scale_y_continuous(limits = c(0, 50)) +
    theme_job +
    ggplot2::theme(
        strip.background = ggplot2::element_blank(),
        strip.text.x = ggplot2::element_blank(),
        axis.text.x = ggplot2::element_blank()
    ) +
    ggplot2::facet_grid(~experiment)

plot4 <- ggplot2::ggplot(data_benchmark, mapping = ggplot2::aes(x = step, y = mean_io_in, fill = step, label = round(mean_io_in, 1))) +
    ggplot2::geom_bar(stat = 'identity', color = 'black', lwd = 0.3) +
    ggplot2::geom_errorbar(ggplot2::aes(ymin=mean_io_in-sd_io_in, ymax=mean_io_in+sd_io_in), width=.2, position=ggplot2::position_dodge(.9)) +
    ggplot2::labs(x = NULL, y = 'Max. IO<br><sub>Reading (GB)</sub>') +
    ggplot2::geom_text(nudge_y = 1, size = 2, fontface = "bold") +
    ggplot2::scale_fill_manual(values = colors, guide = 'none') +
    ggplot2::scale_y_continuous(limits = c(0, 2000)) +
    theme_job +
    ggplot2::theme(
        strip.background = ggplot2::element_blank(),
        strip.text.x = ggplot2::element_blank(),
        axis.text.x = ggplot2::element_blank()
    ) +
    ggplot2::facet_grid(~experiment)

plot5 <- ggplot2::ggplot(data_benchmark, mapping = ggplot2::aes(x = step, y = mean_io_out, fill = step, label = round(mean_io_out, 1))) +
    ggplot2::geom_bar(stat = 'identity', color = 'black', lwd = 0.3) +
    ggplot2::geom_errorbar(ggplot2::aes(ymin=mean_io_out-sd_io_out, ymax=mean_io_out+sd_io_out), width=.2, position=ggplot2::position_dodge(.9)) +
    ggplot2::labs(x = NULL, y = 'Max. IO<br><sub>Writing (GB)</sub>') +
    ggplot2::geom_text(nudge_y = 1, size = 2, fontface = "bold") +
    ggplot2::scale_fill_manual(values = colors, guide = 'none') +
    ggplot2::scale_y_continuous(limits = c(0, 1500)) +
    theme_job +
    ggplot2::theme(
        strip.background = ggplot2::element_blank(),
        strip.text.x = ggplot2::element_blank(),
        axis.text.x = ggtext::element_markdown(angle = 45, hjust = 1, vjust = 1)
    ) +
    ggplot2::facet_grid(~experiment)


plot1 + plot2 + plot3 + plot4 + plot5 + patchwork::plot_layout(ncol = 1, guides = 'collect')


# Determine time per 1M reads demultiplexing. ----

x <- readr::read_tsv('/home/j103t/odomLab/manuscript_scirocket/workflow/logs/step2_demultiplexing_reads/demultiplex_fastq_split_sx42b_1-of-25.log', col_names = 'line') %>%
    dplyr::filter(grepl("INFO: Done:", line)) %>%
    dplyr::mutate(
        n_reads = as.integer(gsub(' read-pairs.*', '', gsub('.*INFO: Done: ', '', line))),
        time = lubridate::as_datetime(gsub(' -.*', '', line))
    )

x$time <- x$time - min(x$time)

ggplot2::ggplot(x, ggplot2::aes(x = n_reads, y = time)) +
    ggplot2::geom_point(size = 1, shape = 21) +
    ggplot2::scale_x_continuous(labels = scales::unit_format(suffix = ' million', scale = 0.000001)) +
    ggplot2::labs(x = 'No. read-pairs', y = 'Time (in seconds)') +
    ggpmisc::stat_poly_eq(ggpmisc::use_label(c("eq", "R2")), formula = x~y, method = 'lm') +
    theme_job

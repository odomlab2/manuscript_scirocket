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
    panel.grid.major.y = ggplot2::element_line(color = "grey90", linetype = "dotted", linewidth = ggplot2::rel(.66)),
    panel.grid.minor = ggplot2::element_blank(),
    panel.background = ggplot2::element_blank(),
    plot.background = ggplot2::element_rect(colour = "white"),
    axis.line = ggplot2::element_line(colour = "black", linewidth = ggplot2::rel(1)),
    plot.margin = ggplot2::unit(c(3, 3, 3, 3), "mm")
)

generate_benchmarking_plot <- function(data, ylimits_runtime = c(0, 625), ylimits_maxio_read = c(0, 2000), ylimits_maxio_write = c(0, 2000), nudge_runtime = 25, nudge_io = 100){
    
    colors <- hues::iwanthue(n=11)
    
    plot1 <- ggplot2::ggplot(data, mapping = ggplot2::aes(x = step, y = mean_m, fill = step, label = round(mean_m, 1))) +
        ggplot2::geom_bar(stat = 'identity', color = 'black', lwd = 0.3) +
        ggplot2::geom_errorbar(ggplot2::aes(ymin=mean_m-sd_m, ymax=mean_m+sd_m), color = 'black', width=.1, position=ggplot2::position_dodge(.9)) +
        ggplot2::labs(x = NULL, y = 'Runtime<br><sub>(in minutes)</sub>') +
        ggplot2::geom_text(nudge_y = nudge_runtime, size = 2, fontface = "bold") +
        ggplot2::scale_fill_manual(values = colors, guide = 'none') +
        ggplot2::scale_color_manual(values = colors, guide = 'none') +
        ggplot2::scale_y_continuous(limits = ylimits_runtime, expand = c(0,0)) +
        theme_job +
        ggplot2::theme(
            axis.text.x = ggplot2::element_blank()
        )
    
    plot2 <- ggplot2::ggplot(data, mapping = ggplot2::aes(x = step, y = mean_mean_load, fill = step, label = round(mean_mean_load, 1))) +
        ggplot2::geom_bar(stat = 'identity', color = 'black', lwd = 0.3) +
        ggplot2::geom_errorbar(ggplot2::aes(ymin=mean_mean_load-sd_mean_load, ymax=mean_mean_load+sd_mean_load), width=.2, position=ggplot2::position_dodge(.9)) +
        ggplot2::labs(x = NULL, y = 'Mean load<br><sub>(CPU / runtime / 100)<sub>') +
        ggplot2::geom_text(nudge_y = 2.5, size = 2, fontface = "bold") +
        ggplot2::scale_fill_manual(values = colors, guide = 'none') +
        ggplot2::scale_y_continuous(limits = c(0, 30), expand = c(0,0)) +
        theme_job +
        ggplot2::theme(
            strip.background = ggplot2::element_blank(),
            strip.text.x = ggplot2::element_blank(),
            axis.text.x = ggplot2::element_blank()
        )
    
    plot3 <- ggplot2::ggplot(data, mapping = ggplot2::aes(x = step, y = mean_max_rss, fill = step, label = round(mean_max_rss, 1))) +
        ggplot2::geom_bar(stat = 'identity', color = 'black', lwd = 0.3) +
        ggplot2::geom_errorbar(ggplot2::aes(ymin=mean_max_rss-sd_max_rss, ymax=mean_max_rss+sd_max_rss), width=.2, position=ggplot2::position_dodge(.9)) +
        ggplot2::labs(x = NULL, y = 'Max. RSS<br><sub>(GB)</sub>') +
        ggplot2::geom_text(nudge_y = 2.5, size = 2, fontface = "bold") +
        ggplot2::scale_fill_manual(values = colors, guide = 'none') +
        ggplot2::scale_y_continuous(limits = c(0, 75), expand = c(0,0)) +
        theme_job +
        ggplot2::theme(
            strip.background = ggplot2::element_blank(),
            strip.text.x = ggplot2::element_blank(),
            axis.text.x = ggplot2::element_blank()
        )
    
    plot4 <- ggplot2::ggplot(data, mapping = ggplot2::aes(x = step, y = mean_io_in, fill = step, label = round(mean_io_in, 1))) +
        ggplot2::geom_bar(stat = 'identity', color = 'black', lwd = 0.3) +
        ggplot2::geom_errorbar(ggplot2::aes(ymin=mean_io_in-sd_io_in, ymax=mean_io_in+sd_io_in), width=.2, position=ggplot2::position_dodge(.9)) +
        ggplot2::labs(x = NULL, y = 'Max. IO<br><sub>Reading (GB)</sub>') +
        ggplot2::geom_text(nudge_y = nudge_io, size = 2, fontface = "bold") +
        ggplot2::scale_fill_manual(values = colors, guide = 'none') +
        ggplot2::scale_y_continuous(limits = ylimits_maxio_read, expand = c(0,0)) +
        theme_job +
        ggplot2::theme(
            strip.background = ggplot2::element_blank(),
            strip.text.x = ggplot2::element_blank(),
            axis.text.x = ggplot2::element_blank()
        )
    
    plot5 <- ggplot2::ggplot(data, mapping = ggplot2::aes(x = step, y = mean_io_out, fill = step, label = round(mean_io_out, 1))) +
        ggplot2::geom_bar(stat = 'identity', color = 'black', lwd = 0.3) +
        ggplot2::geom_errorbar(ggplot2::aes(ymin=mean_io_out-sd_io_out, ymax=mean_io_out+sd_io_out), width=.2, position=ggplot2::position_dodge(.9)) +
        ggplot2::labs(x = NULL, y = 'Max. IO<br><sub>Writing (GB)</sub>') +
        ggplot2::geom_text(nudge_y = nudge_io, size = 2, fontface = "bold") +
        ggplot2::scale_fill_manual(values = colors, guide = 'none') +
        ggplot2::scale_y_continuous(limits = ylimits_maxio_write, expand = c(0,0)) +
        theme_job +
        ggplot2::theme(
            strip.background = ggplot2::element_blank(),
            strip.text.x = ggplot2::element_blank(),
            axis.text.x = ggtext::element_markdown(angle = 45, hjust = 1, vjust = 1)
        )
    
    plot1 + plot2 + plot3 + plot4 + plot5 + patchwork::plot_layout(ncol = 1, guides = 'collect')
}


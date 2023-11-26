
create_histogram_plot_fund_vs_benchmark <- function(data, fund_columns, title = "Comparison of Fund Returns") {
    # Reshape data from wide to long format
    long_data <- data %>%
        pivot_longer(cols = fund_columns, names_to = "Fund", values_to = "Return")


    prof_colors <- c("darkblue", "darkgreen", "darkred")

    # Calculate median for each fund
    median_data <- long_data %>%
        group_by(Fund) %>%
        summarise(median = median(Return, na.rm = TRUE))

    # Create the histogram plot
    histogram_plot <- ggplot(long_data, aes(x = Return, fill = Fund)) +
        geom_histogram(bins = 30, position = "identity", alpha = 0.5) +
        facet_wrap(~Fund, scales = "fixed") +
        scale_fill_manual(values = prof_colors) +
        geom_vline(data = median_data, aes(xintercept = median), color = "black", linetype = "dashed") +
        labs(title = title, x = "Return", y = "Count") +
        theme_minimal() +
        theme(legend.position = "bottom")

    return(histogram_plot)
}

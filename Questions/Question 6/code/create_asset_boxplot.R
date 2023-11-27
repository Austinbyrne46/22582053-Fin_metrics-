create_asset_boxplot <- function(data) {

    # Creating the boxplot
    boxplot_graph <- data %>%
        ggplot() +
        geom_boxplot(aes(x = Asset_Class, y = Monthly_Return, fill = Asset_Class), alpha = 0.7) +
        coord_flip() +
        fmxdat::theme_fmx(title.size = fmxdat::ggpts(30),
                          subtitle.size = fmxdat::ggpts(25),
                          caption.size = fmxdat::ggpts(25),
                          CustomCaption = TRUE) +
        fmxdat::fmx_cols() +
        labs(x = "", y = "%", caption = "Note:\nCalculation own",
             title = "Box Plot of Monthly Returns by Asset Class",
             subtitle = "Distribution and Outliers Analysis")

    return(boxplot_graph)
}

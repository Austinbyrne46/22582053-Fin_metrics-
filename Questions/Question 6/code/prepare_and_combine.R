prepare_and_combine <- function(df1, df2) {
    # Prepare the first data frame
    df1_prepared <- df1 %>%
        select(date, Ticker, Year, Month, Monthly_Return, Asset_Class)

    # Prepare the second data frame
    df2_prepared <- df2 %>%
        select(date, Ticker = Name, Year, Month, Monthly_Return, Asset_Class)

    # Combine the two prepared data frames
    combined_data <- rbind(df1_prepared, df2_prepared)

    return(combined_data)
}



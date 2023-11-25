calculate_cumulative_returns <- function(data) {
    # Ensure the 'date' column is in the correct format
    if (!"date" %in% names(data) || !inherits(data$date, "Date")) {
        stop("The data must contain a 'date' column in Date format.")
    }

    # Calculate the cumulative returns
    cumulative_returns <- data %>%
        arrange(date) %>%
        mutate(across(.cols = -date, .fns = ~cumprod(1 + .))) %>%
        mutate(across(.cols = -date, .fns = ~./first(.))) %>%
        pivot_longer(cols = -date, names_to = "Index", values_to = "Cumulative_port_returns")

    return(cumulative_returns)
}

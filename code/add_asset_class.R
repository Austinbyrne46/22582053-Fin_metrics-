add_asset_class <- function(df, asset_classes) {

    column_to_use <- ifelse("Ticker" %in% names(df), "Ticker", "Name")


    df <- df %>%
        mutate(Asset_Class = asset_classes[get(column_to_use)])

    return(df)
}

# Named vector for asset class mapping
asset_classes <- c(
    "M2WD Index" = "Equity",
    "M2WO Index" = "Equity",
    "M2EF Index" = "Equity",
    "M2US Index" = "Equity",
    "M8EU Index" = "Equity",
    "M8JP Index" = "Equity",
    "M1AP Index" = "Equity",
    "LGAGTRUH Index" = "Rates",
    "LUAGTRUU Index" = "Rates",
    "LEATTREU Index" = "Rates",
    "LGCPTRUH Index" = "Credit",
    "LUACTRUU Index" = "Credit",
    "LP05TREH Index" = "Credit",
    "ADXY Index" = "Asian currency",
    "DXY Index" = "USD",
    "BCOMTR Index" = "Commodity",
    "MSCI_RE" = "Real estate",
    "MSCI_USREIT" = "Real estate",
    "MSCI_ACWI" = "Equity",
    "MSCI_Jap" = "Equity",
    "MSCI_USA" = "Equity"
)



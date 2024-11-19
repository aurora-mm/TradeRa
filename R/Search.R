#' Perform a Search Query to Tradera API
#'
#' This function sends a search request to the Tradera API using the provided credentials, query, and ordering preferences.
#' The results are returned as a data frame with relevant item details.
#'
#' @param AppId Numeric. The application ID for authentication with the Tradera API.
#' @param AppKey Character. The application key for authentication with the Tradera API.
#' @param orderBy Character. Specifies the order of the search results. Can be one of `Relevance`, `PriceAscending`, or `PriceDescending`.
#' @param query Character. The search term or query to be used for retrieving items.
#'
#' @return A data frame with the following columns:
#' \item{ShortDescription}{A brief description of the item.}
#' \item{LongDescription}{A detailed description of the item.}
#' \item{Price}{The price or current bid for the item.}
#' \item{ItemUrl}{A URL to the item's listing on Tradera.}
#'
#' If no items are found, the function returns a data frame with the message `"Nothing found!"`. If the API response is empty, it returns a data frame with the message `"Internal Tradera Error"`.
#'
#' @details
#' The function constructs a SOAP request to interact with the Tradera API, performs the request using `RCurl`, and parses the XML response into a data frame using `xml2` and `tibble`.
#' The function returns only the first 50 results on Tradera for the given search query. 
#' Due to the limit on 100 calls to the Tradera API per day the results of the query are cached in the local environment.
#'
#' @importFrom RCurl basicTextGatherer curlPerform
#' @importFrom xml2 read_xml as_list
#' @importFrom tibble as_tibble enframe
#' @importFrom tidyr unnest_longer unnest_wider
#' @importFrom dplyr "%>%"
#' @importFrom dplyr filter
#' @importFrom rlang .data
#'
#' @export

Search <- function(AppId, AppKey, orderBy, query) {
  # Check if AppId is provided; if not, stop execution with an error message
  if (missing(AppId)) stop("AppId must be specified")

  # Ensure AppId is numeric; if not, stop execution with an error message
  if (!is.numeric(AppId)) stop("AppId must be numeric")

  # Check if AppKey is provided; if not, stop execution with an error message
  if (missing(AppKey)) stop("AppKey must be specified")

  # Ensure AppKey is a character string; if not, stop execution with an error message
  if (!is.character(AppKey)) stop("AppId must be character")

  # Check if query is provided; if not, stop execution with an error message
  if (missing(query)) stop("query must be specified")

  # Ensure query is a character string; if not, stop execution with an error message
  if (!is.character(query)) stop("query must be character")

  # Check if orderBy is provided; if not, stop execution with an error message
  if (missing(orderBy)) stop("orderBy must be specified")

  # Ensure orderBy is one of the allowed values; if not, stop execution with an error message
  if (!orderBy %in% c(
    "Relevance",
    "PriceAscending",
    "PriceDescending"
  )) {
    stop("orderBy can be only Relevance, PriceAscending or PriceDescending")
  }
  
  # Ensure a global cache environment exists
  cache_env <- getOption("tradera_search_cache")
  if (is.null(cache_env)) {
    cache_env <- new.env(parent = emptyenv())
    options(tradera_search_cache = cache_env)
  }
  
  # Create a unique cache key
  cache_key <- paste(AppId, AppKey, orderBy, query, Sys.Date(), sep = "_")
  
  # Check if the result is already cached
  if (exists(cache_key, envir = cache_env)) {
    return(get(cache_key, envir = cache_env))
  }
  
  # Define a nested function to perform the XML query
  xmlQuery <- function(AppId, AppKey, orderBy, query) {
    # Set up the HTTP headers for the SOAP request
    header <- c(
      Accept = "text/xml", "Content-Type" = "text/xml; charset=utf-8",
      SOAPAction = "http://api.tradera.com/Search"
    )

    # Construct the body of the SOAP request using XML format
    body <- paste0(r'[<?xml version="1.0" encoding="UTF-8"?>
<soap:Envelope xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns:xsd="http://www.w3.org/2001/XMLSchema" xmlns:soap="http://schemas.xmlsoap.org/soap/envelope/">
  <soap:Header>
    <AuthenticationHeader xmlns="http://api.tradera.com">
      <AppId>]', as.character(AppId), r'[</AppId>
      <AppKey>]', as.character(AppKey), r'[</AppKey>
    </AuthenticationHeader>
    <ConfigurationHeader xmlns="http://api.tradera.com">
      <Sandbox>0</Sandbox>
      <MaxResultAge>0</MaxResultAge>
    </ConfigurationHeader>
  </soap:Header>
  <soap:Body>
    <Search xmlns="http://api.tradera.com">
      <query>]', as.character(query), r'[</query>
      <categoryId>0</categoryId>
      <pageNumber>1</pageNumber>
      <orderBy>]', as.character(orderBy), r'[</orderBy>
    </Search>
  </soap:Body>
</soap:Envelope>
]')

    # Create a text gatherer to capture the response
    reader <- RCurl::basicTextGatherer()

    # Perform the HTTP request to the Tradera API
    RCurl::curlPerform(
      url = "http://api.tradera.com/v3/searchservice.asmx",
      httpheader = header,
      postfields = body,
      writefunction = reader$update
    )

    # Retrieve and return the response value
    return(reader$value())
  }

  # Call the xmlQuery function to execute the SOAP request
  xml <- xmlQuery(AppId, AppKey, orderBy, query)

  # Check if the response is empty; if so, return an error message
  if (xml == "") {
    df <- data.frame(
      ShortDescription = "Internal Tradera error!",
      LongDescription = "",
      Price = "",
      ItemUrl = ""
    )
    return(df)
  }

  # Convert the XML response to a list
  xml_list <- xml2::as_list(xml2::read_xml(xml))

  # Convert the list to a tibble and unnest the Envelope
  xml_df <- tibble::as_tibble(xml_list) %>% tidyr::unnest_longer("Envelope")

  # Extract the SearchResult from the tibble
  xml_df <- xml_df[[1]][[1]][["SearchResult"]] %>%
    tibble::enframe() %>%
    tidyr::unnest_wider("value", names_sep = "_", names_repair = "universal")

  # Get items
  xml_df <- xml_df %>% filter(.data$name == "Items")

  # Check if there are no items found;
  # If so, return df with same structure but with "Nothing found"
  if (nrow(xml_df) == 0) {
    df <- data.frame(
      ShortDescription = "Nothing found!",
      LongDescription = "",
      Price = "",
      ItemUrl = ""
    )
    return(df)
  }

  # Create a data frame with the relevant item details
  df <- data.frame(
    ShortDescription = unlist(xml_df$value_ShortDescription),
    LongDescription = unlist(xml_df$value_LongDescription),
    Price = unlist(xml_df$value_MaxBid),
    ItemUrl = unlist(xml_df$value_ItemUrl)
  )

  # Cache the result and return it
  assign(cache_key, df, envir = cache_env)
  return(df)
}

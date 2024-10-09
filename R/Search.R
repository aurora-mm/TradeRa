#' Perform a Search Query to Tradera API
#'
#' This function sends a search request to the Tradera API using the provided credentials, page number, query, and ordering preferences.
#' The results are returned as a list containing a data frame with relevant item details, and the total number of pages.
#'
#' @param AppId Numeric. The application ID for authentication with the Tradera API.
#' @param AppKey Character. The application key for authentication with the Tradera API.
#' @param pageNumber Numeric. The page number of the search results to retrieve. Must be a scalar.
#' @param orderBy Character. Specifies the order of the search results. Can be one of `Relevance`, `PriceAscending`, or `PriceDescending`.
#' @param query Character. The search term or query to be used for retrieving items.
#'
#' @return A list containing two elements:
#' \item{df}{A data frame with the following columns: `ShortDescription`, `LongDescription`, `Price`, and `ItemUrl`, which contain details of the items found in the search results.}
#' \item{total_pages}{Numeric. The total number of pages available for the search query.}
#'
#' If no items are found, the function returns a data frame with the message `"Nothing found!"`. If the API response is empty, it returns a data frame with the message `"Internal Tradera Error"`.
#'
#' @details
#' The function constructs a SOAP request to interact with the Tradera API, performs the request using `RCurl`, and parses the XML response into a data frame using `xml2` and `tibble`.
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

Search <- function(AppId, AppKey, pageNumber, orderBy, query) {
  # Check if AppId is provided; if not, stop execution with an error message
  if (missing(AppId)) stop("AppId must be specified")

  # Ensure AppId is numeric; if not, stop execution with an error message
  if (!is.numeric(AppId)) stop("AppId must be numeric")

  # Check if AppKey is provided; if not, stop execution with an error message
  if (missing(AppKey)) stop("AppKey must be specified")

  # Ensure AppKey is a character string; if not, stop execution with an error message
  if (!is.character(AppKey)) stop("AppId must be character")

  # Check if pageNumber is provided; if not, stop execution with an error message
  if (missing(pageNumber)) stop("pageNumber must be specified")

  # Ensure pageNumber is numeric; if not, stop execution with an error message
  if (!is.numeric(pageNumber)) stop("pageNumber must be numeric")

  # Ensure pageNumber is a scalar; if not, stop execution with an error message
  if (!length(pageNumber) == 1) stop("pageNumber must be scalar")

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

  # Define a nested function to perform the XML query
  xmlQuery <- function(AppId, AppKey, pageNumber, orderBy, query) {
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
      <pageNumber>]', as.character(pageNumber), r'[</pageNumber>
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
  xml <- xmlQuery(AppId, AppKey, pageNumber, orderBy, query)

  # Check if the response is empty; if so, return an error message
  if (xml == "") {
    df <- data.frame(
      ShortDescription = "Internal Tradera error!",
      LongDescription = "",
      Price = "",
      ItemUrl = ""
    )
    list_df <- list("df" = df, "total_pages" = 0)
    return(list_df)
  }

  # Convert the XML response to a list
  xml_list <- xml2::as_list(xml2::read_xml(xml))

  # Convert the list to a tibble and unnest the Envelope
  xml_df <- tibble::as_tibble(xml_list) %>% tidyr::unnest_longer("Envelope")

  # Extract the SearchResult from the tibble
  xml_df <- xml_df[[1]][[1]][["SearchResult"]] %>%
    tibble::enframe() %>%
    tidyr::unnest_wider("value", names_sep = "_", names_repair = "universal")

  # Get items and the total number of pages
  xml_df_pages <- xml_df %>% filter(.data$name == "TotalNumberOfPages")
  total_pages <- as.numeric(xml_df_pages$value_1)
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
    list_df <- list("df" = df, "total_pages" = 0)
    return(list_df)
  }

  # Create a data frame with the relevant item details
  df <- data.frame(
    ShortDescription = unlist(xml_df$value_ShortDescription),
    LongDescription = unlist(xml_df$value_LongDescription),
    Price = unlist(xml_df$value_MaxBid),
    ItemUrl = unlist(xml_df$value_ItemUrl)
  )

  # Return a list containing the data frame and total pages
  list_df <- list("df" = df, "total_pages" = total_pages)
  return(list_df)
}

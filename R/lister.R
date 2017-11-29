#' @export

lister <- function(number = 2, text = "Initial Shareholder"){


  shs <- rep(text, number) %>%
    enframe() %>%
    mutate(Shareholder = paste(value, name)) %>%
    select(Shareholder)

  shs

}

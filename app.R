

library(shiny)
library(tidyverse)
library(plotly)
library(shinycssloaders)
library(formattable)


devtools::install_github("rosseji/shiny.semantic@develop")
library(shiny.semantic)



devtools::install_github("trendlock/SAFEnote")
library(SAFEnote)







Threshold <- 200000
Discount <- .2
SAFE.Investment <- 50000

df_assume <- tibble(What = c("Threshold", "Discount", "SAFE.Investment"),
             `How Much` = c("$200K", "20%", "$50k"))

ui <- fluidPage(
  semanticPage(
    title = "SAFENote Simulator",


    br(),
    h1(class = "ui header", "SAFENote Simulator",
       div(class = "sub header", "By TRENDLOCK")),
    div(class = "ui divider"),
    div(class = "ui grid",
        div(class = "four wide column",
            uicard(
              div(class = "ui horizontal divider", uiicon("tag"), "Non-Adjustable Assumptions"),

              align = "center",
              formattableOutput("assume", width = "70%")
            ),
            uisegment(
              div(class = "ui horizontal divider", uiicon("tag"), "Adjustable Inputs"),
              sliderInput("Cap", "Cap", min = 0, max = 10, value = 2, pre = "$", post = "M"),
              sliderInput("Existing.no.shareholders", "Existing No of Shareholders", min = 0, max = 10, value = 3, post = " Shareholders"),
              sliderInput("Existing.no.shares.issued", "Existing No of Shares Issued", min = 0, max = 10000, value = 1000, post = " Shares"),
              sliderInput("Pre.cash.valuation", "Pre Cash Valuation", min = 0, max = 10, value = 1, step = 0.25, pre = "$", post = "M"),
              sliderInput("New.Investment", "New Investment", min = 0, max = 5, value = .25, step = 0.25, pre = "$", post = "M")

              )
            ),
        div(class = "eight wide column",

            div(class = "ui horizontal divider", uiicon("tag"), "Shares on Issue"),
            plotlyOutput("on_issue_plot") %>% withSpinner(),
            div(class = "ui horizontal divider", uiicon("tag"), "Percentage of Equity Issued"),
            plotlyOutput("main_plot") %>% withSpinner()


            ),
        div(class = "four wide column",
            uisegment(
              div(class = "ui horizontal divider", uiicon("tag"), "All Values"),
              align = "center",
              formattableOutput("inputs_tbl", width = "70%")
            )

        )

  ))

)

server <- function(input, output) {


  output$assume <- renderFormattable(
    formattable(df_assume)
  )


  output$main_plot <- renderPlotly({


    Cap <- input$Cap * 1000000
    Existing.no.shareholders <- input$Existing.no.shareholders
    Existing.no.shares.issued <- input$Existing.no.shares.issued
    Pre.cash.valuation <- input$Pre.cash.valuation * 1000000
    New.Investment <-  input$New.Investment * 1000000


    preraise_table <- lister(number = Existing.no.shareholders)
    preraise_table <- preraise_table %>%
      mutate(shares = round(Existing.no.shares.issued/Existing.no.shareholders),
             percent = shares/Existing.no.shares.issued*100)

    #### A few more calcs #########
    price.per.share <- Pre.cash.valuation/Existing.no.shares.issued
    shares.issued <- New.Investment/price.per.share
    SAFE.triggered <- if_else(New.Investment >= Threshold, TRUE, FALSE)
    Discount.Price <- price.per.share*(1-Discount)
    SAFE.Price <- Pre.cash.valuation/(Cap/Existing.no.shares.issued)
    Cap.triggered <- if_else(SAFE.Investment/SAFE.Price > SAFE.Investment/Discount.Price, TRUE, FALSE)
    Westpac.Shares <- round(max(SAFE.Investment/Discount.Price, SAFE.Investment/SAFE.Price))
    Total.Shares.Post.Raise <- sum(preraise_table$shares) + shares.issued + Westpac.Shares

    ls <- list(
      list(name = "Cap",  val = Cap),
      list(name = "Existing.no.shareholders",  val = Existing.no.shareholders),
      list(name = "Existing.no.shares.issued",  val = Existing.no.shares.issued),
      list(name = "Pre.cash.valuation",  val = Pre.cash.valuation),
      list(name = "New.Investment",  val = New.Investment),
      list(name = "price.per.share",  val = price.per.share),
      list(name = "shares.issued",  val = shares.issued),
      list(name = "SAFE.triggered",  val = SAFE.triggered),
      list(name = "Discount.Price",  val = Discount.Price),
      list(name = "SAFE.Price",  val = SAFE.Price),
      list(name = "Cap.triggered",  val = Cap.triggered),
      list(name = "Westpac.Shares",  val = Westpac.Shares),
      list(name = "Total.Shares.Post.Raise",  val = Total.Shares.Post.Raise)
    )

    output$inputs_tbl <-  renderFormattable({
      ls %>%
        map( ~ enframe(.x[["val"]]) %>%
               mutate(name = .x[["name"]],
                      value = as.character(value))) %>%
        bind_rows() %>%
        `colnames<-`(c("What", "Info")) %>%
        formattable()
    })


    ##### Building basic df for New Investor  #####
    Shareholder <- c("New Investor")
    shares <- c(shares.issued)
    New.Investor <- tibble(Shareholder, shares)


    ######B Building basic df for Westpac  #########
    Shareholder <- c("Westpac")
    shares <- c(Westpac.Shares)
    Westpac <- tibble(Shareholder, shares)

    ##########  The Post-Raise Equity Table   #####
    postraise_table <- preraise_table %>%
      select(Shareholder, shares) %>%
      bind_rows(New.Investor, Westpac) %>%
      mutate(percent = shares/Total.Shares.Post.Raise*100)

    ######  Making the data tidy #########
    preraise_table. <- preraise_table %>%
      mutate(stage = "Pre-Raise")
    postraise_table. <- postraise_table %>%
      mutate(stage = "Post-Raise")

    tidy_table <- bind_rows(preraise_table., postraise_table.)
    tidy_table$stage <- factor(tidy_table$stage, levels = c("Pre-Raise", "Post-Raise"))



    output$on_issue_plot <- renderPlotly({
      p <- ggplot(tidy_table, aes(x = stage, y = shares, fill = Shareholder))+
        geom_bar(stat = "identity")
        # labs(title = "Shares on Issue")
      ggplotly(p)
    })


    p <- ggplot(tidy_table, aes(x = stage, y = percent, fill = Shareholder))+
      geom_bar(stat = "identity")
      # labs(title = "Percentage of Equity Issued")
    ggplotly(p)

  })




}

shinyApp(ui = ui, server = server)


library(tidyverse)

##### Non-Adjustable Assumptions #########
Threshold <- 200000
Discount <- .2
SAFE.Investment <- 50000

#### Adjustable Inputs
Cap <- 2000000
Existing.no.shareholders <- 3
Existing.no.shares.issued <- 1000
Pre.cash.valuation <- 1000000
New.Investment <- 250000

#### The Pre-Raise Equity Table #####
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


####### Plots to explain things visually   #########
ggplot(tidy_table, aes(x = stage, y = shares, fill = Shareholder))+
  geom_bar(stat = "identity")+
  labs(title = "Shares on Issue")

ggplot(tidy_table, aes(x = stage, y = percent, fill = Shareholder))+
  geom_bar(stat = "identity")+
  labs(title = "Percentage of Equity Issued")



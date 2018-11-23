library(tidyverse)
library(lubridate)

convert_from_czech <- function(original) {
  case_when(
    original == "PRIJEM" ~ "Credit",
    original == "VYDAJ" ~ "Withdrawal",
    original == "VYBER KARTOU" ~ "Credit card withdrawl",
    original == "VKLAD" ~ "Credit in cash",
    original == "PREVOD Z UCTU" ~ "Collection from another bank",
    original == "VYBER" ~ "Withdrawal in cash",
    original == "PREVOD NA UCET" ~ "Remittance to another bank",
    original == "POJISTNE"  ~ "Insurrance payment",
    original == "SLUZBY" ~ "Payment for statement", 
    original == "UROK"  ~  "Interest credited",
    original == "SANKC.UROK" ~ "Sanction interest",
    original == "SIPO"  ~ "Household",
    original == "DUCHOD" ~  "Old age pension",
    original == "UVER"  ~  "Loan payment",
    original == "POPLATEK MESICNE" ~ "Monthly issuance",
    original == "POPLATEK TYDNE" ~ "Weekly issuance",
    original == "POPLATEK PO OBRATU" ~ "Issuance after transaction"
  )
}

birth_day <- function(birth_number) {
  as.integer( substr(birth_number, 5, 6) )
}

birth_month <- function(birth_number) {
  month <- as.integer( substr(birth_number, 3, 4) )
  if ( month > 12) month = month - 50
  month
}

birth_year <- function(birth_number) {
  year <- as.integer( substr(birth_number, 1, 2) )
  year + 1900
}

decode_gender <- function(birth_day) {
  ifelse( birth_day >= 50 , 
    "Female", 
    "Male"
  )
}

decode_birth_date <- function(gender, birth_number) {
  if_else(gender == "Male",
        as.Date.character(as.character(birth_number),"%y%m%d"),
        as.Date.character(as.character(birth_number-5000),"%y%m%d")
  )
}

transactions <- read.csv2("data/trans.asc", stringsAsFactors = FALSE)

transactions <- transactions %>% mutate(amount = as.double(amount)) %>%
  mutate(balance = as.double(balance)) %>%
  mutate(date = as.Date.character(date,"%y%m%d")) %>%
  mutate(type = convert_from_czech(type)) %>%
  mutate(operation = convert_from_czech(operation)) 
         
transactions %>% count(type)

transactions %>% count(operation)

accounts <- read.csv2("data/account.asc", stringsAsFactors = FALSE)

accounts <- accounts %>% mutate(date = as.Date.character(date,"%y%m%d")) %>%
    mutate(frequency = convert_from_czech(frequency))

accounts %>% count(frequency)

cards <- read.csv2("data/card.asc", stringsAsFactors = FALSE)

cards <- cards %>% mutate(issued = as.Date.character(issued,"%y%m%d"))

clients <- read.csv2("data/client.asc", stringsAsFactors = FALSE)

clients <- clients %>% mutate(
  gender = decode_gender(birth_day(birth_number)),
  birth_date = decode_birth_date(gender,birth_number)
)

disp <- read.csv2("data/disp.asc", stringsAsFactors = FALSE)

districts <- read.csv2("data/district.asc", stringsAsFactors = FALSE)
 
mmm <- districts %>% left_join(clients, by= c("A1" = "district_id"))  %>%
    left_join(disp, by = c("client_id.y" = "client_id" )) %>%
    left_join(accounts, by = c("account_id" = "account_id" )) %>%
    left_join(transactions, by.x = "account_id", by.y = "account_id"))


transactions %>% ggplot(aes(x=type, y=amount)) + geom_boxplot()

transactions %>% ggplot(aes(x=amount)) + geom_histogram()

transactions %>% ggplot(aes(x=round_date(date, unit = "month"), y=amount)) + geom_dotplot()

transactions %>% ggplot(aes(x=type)) + geom_bar(aes(y=sum(amount)))

transactions %>% ggplot(aes(x=wday(date), y=amount, group=wday(date))) + geom_boxplot()

transactions %>% ggplot(aes(x=day(date), y=amount, group=day(date))) + geom_boxplot()

transactions %>% ggplot(aes(x=type, y=amount)) + geom_bar(stat="sum")




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
  as.integer( substr(birth_number, 3, 4) )
}


decode_gender <- function(birth_day) {
  ifelse( birth_day >= 50 , 
    "Female", 
    "Male"
  )
}

decode_birth_date <- function(gender, birth_number) {
  if_else(gender == "Male",
        as.Date.character(as.character(birth_number),"%Y%m%d"),
        as.Date.character(as.character(birth_number-5000),"%Y%m%d")
  )
}

transactions <- read.csv2("data/trans.asc", stringsAsFactors = FALSE)

transactions <- transactions %>% mutate(
  amount = as.double(amount),
  balance = as.double(balance),
  date = as.Date.character(date,"%y%m%d"),
  type = convert_from_czech(type),
  operation = convert_from_czech(operation)
)

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
  birth_date = decode_birth_date(birth_number)
)

districts <- read.csv2("data/district.asc", stringsAsFactors = FALSE)
districts <- 

mmm <- districts %>% left_join(clients, by= c("A1" = "district_id"))





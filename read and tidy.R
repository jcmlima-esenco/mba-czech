
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


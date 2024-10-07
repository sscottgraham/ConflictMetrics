

# GPT JSON Parser & Cleaner  ----------------------------------------------

# This code parses JSON outputs from GPT and cleans COI types. 


# Pre-flight --------------------------------------------------------------

# load libraries 
library(tidyverse)
library(jsonlite)

# Input JSON CSV file
input_json <- ""

# load data & clean data 
data <- read_csv(paste0("output/",input_json,".csv")) %>% 
  mutate(json_cleaned = str_remove_all(coi_parsed,"```"),
         json_cleaned = str_remove_all(json_cleaned,"json"),
         json_cleaned = str_squish(json_cleaned)) 


# Create JSON Extraction Function
json_extract <- function(x){
  json_parsed = fromJSON(data$json_cleaned[x]) %>% data.frame()
  
  temp <- data.frame(pmid=data$pmid[x],json_parsed)
  
  colnames(temp) <- c("pmid","name","source","type")
  
  temp %>% select(pmid:type)
}


# Apply function to data  -------------------------------------------------
parsed_cois <- map_dfr(1:nrow(data),possibly(json_extract, otherwise = NULL)) %>% unique()


# Clean Categories --------------------------------------------------------

#Regex dictionary for COI Type matching 
coi_research = 'funding|research|grant|investigator'
coi_speaking = 'speaking|lectur|gave|honorari'
coi_consultingfees = 'consult'
coi_advisory = 'advis|panel|board|advice|committee|meeting|member'
coi_employment = 'employ|hire|associate|salar|work'
coi_feeunspec ='renumerat|money|contribut|personal fee|unspecified|remuneration|unspecified'
coi_travel = 'travel|accommodation|hospitality'
coi_stock = 'shareholder|equity|interest|stock|dividend|hold|share'
coi_ippatent = 'patent|invent|co-invent|coinvent|IP|copyright'
coi_others = 'other|own|award|edit|chair|endowed|professorship|witness|manuscript|review|non-financial|nonfinancial|unpaid|un-paid|volunteer|suppli|equipment|material|test|teach|educat|course|train|royalt|proprietary|founder|partner|co-founded|co-founder|author fees|contract|guarantor'
exclude = c("none","None","no","No", "declared","")
 

# Clean COI types in parsed JSON data
parsed_cois_clean <- parsed_cois %>% 
  filter(!(source %in% exclude)) %>%
  filter(!(type %in% exclude)) %>% 
  filter(!(is.na(type) & is.na(source))) %>% 
  mutate(
    clean_type = case_when(
      str_detect(type,coi_research) ~ "research",
      str_detect(type,coi_speaking) ~ "speaking_fees",
      str_detect(type,coi_consultingfees) ~ "consulting_fees",
      str_detect(type,coi_advisory) ~ "advisory",
      str_detect(type,coi_employment) ~ "employment",
      str_detect(type,coi_feeunspec) ~ "fee_unspecified",
      type == "fees" ~ "fee_unspecified",
      str_detect(type,coi_travel) ~ "travel_fees",
      str_detect(type,coi_stock) ~ "stock",
      str_detect(type, coi_ippatent) ~ "IP_patent",
      str_detect(type,coi_others) ~ "combined_others")
    ) %>% 
  unique()




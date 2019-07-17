library(tidyverse)
library(janitor)
library(here)

# ran python script to clean characters
# also cleaned up some length descriptions

library(data.table)
library(tidyverse)
library(janitor)
library(here)

shark_attacks <- read_csv(here("sharks", "data", "shark_attacks_cln.csv"), trim_ws = TRUE, skip_empty_rows = TRUE) %>% 
  as_tibble() %>% 
  clean_names()
  
shark_attacks_clean <- shark_attacks %>% 
  select(-href_formula) %>% 
  filter(case_number > 0,
         case_number != "xx") %>% 
  mutate_all(funs(toupper)) %>% 
  mutate_at(vars(case_number:original_order), str_squish) %>%
  mutate(year = as.numeric(year))


clean_species <- shark_attacks_clean %>% 
  separate(species, into = c("species", "species2", "species3"), sep = ", ", remove = TRUE, extra = "warn", fill = "warn") %>% 
  mutate(species = str_remove_all(species, "\\["), 
         species = str_remove_all(species, "\\]")) %>% 
  mutate(species2 = str_replace(species, "1.8 M 6' BLACKTIP", "BLACKTIP"),
        species2 = str_replace(species, "0.9 M TO 1.2 M 3' TO 4' WHITE", "WHITE"), 
        species2 = str_replace(species, "1.2 M 4' BLACKTIP", "BLACKTIP"),
        species2 = str_replace(species, "1.2 M 4' BULL", "BULL"),
        species2 = str_replace(species, "1.2 M 4' SPINNER", "SPINNER"),
        species2 = str_replace(species, "44	1.8 M 6' BLACKTIP", "BLACKTIP"),
        species2 = str_replace(species, "46	1.8 M 6' BULL", "BULL"),
        species2 = str_replace(species, "1.8 M 6' CARIBBEAN REEF", "CARIBBEAN REEF"),
        species2 = str_replace(species, "1.8 M 6' BULL", "BULL"),
        species2 = str_replace(species, "1.8 M TO 2.1 M 6' TO 7' CARIBBEAN REEF", "CARIBBEAN"),
        species2 = str_replace(species, "1.8 M TO 2.1 M 6' TO 7' HAMMERHEAD", "HAMMERHEAD"),
        species2 = str_replace(species, "1.8 M TO 2.4 M 6' TO 8' WHITE", "WHITE"),
        species2 = str_replace(species, "1.9 M 6.5' BULL", "BULL"),
        species2 = str_replace(species, "12' TO 14' WHITE", "WHITE"),
        species2 = str_replace(species, "14' TO 16' WHITE", "WHITE"),
        species2 = str_replace(species, "14' WHITE", "WHITE"),
        species2 = str_replace(species, "18' WHITE", "WHITE"),
        species2 = str_replace(species, "2 M 6.75' COPPER", "COPPER"),
        species2 = str_replace(species, "2 M 6.75' SEVENGILL", "SEVENGILL"),
        species2 = str_replace(species, "2 M 6.75' WHITE", "WHITE"),
        species2 = str_replace(species, "2 M HAMMERHEAD", "HAMMERHEAD SHARK"),
        species2 = str_replace(species, "2 SMALL BULL", "BULL"),
        species2 = str_replace(species, "2 WHITE SHARK: 13' & 9'8", "WHITE"),
        species2 = str_replace(species, "2.1 M 7' BULL", "BULL"),
        species2 = str_replace(species, "2.3 M 7.5' WHITE", "WHITE"),
        species2 = str_replace(species, "2.5 M 8.25' BULL", "BULL"),
        species2 = str_replace(species, "2.5 M 8.25' WHITE", "WHITE"),
        species2 = str_replace(species, "2.7 M 9' BULL", "BULL"),
        species2 = str_replace(species, "2' TO 3' JUVENILE", "JUVENILE"),
        species2 = str_replace(species, "2' TO 3' REEF", "REEF"),
        species2 = str_replace(species, "3 BULL SHARKS", "BULL SHARK"),
        species2 = str_replace(species, "3 M 10' BULL", "BULL"),
        species2 = str_replace(species, "3 M 10' WHITE", "WHITE"),
        species2 = str_replace(species, "3 M HAMMERHEAD", "HAMMERHEAD"),
        species2 = str_replace(species, "3 M TO 3.7 M 10' TO 12' BULL", "BULL"),
        species2 = str_replace(species, "3 M TO 4 M 10' TO 13' WHITE", "WHITE"),
        species2 = str_replace(species, "3 M TO 4 M WHITE", "WHITE"),
        species2 = str_replace(species, "3 M TO 4.5 M 10' TO 15' WHITE", "WHITE"),
        species2 = str_replace(species, "3 M TO 5 M 10' TO 16.5' WHITE", "WHITE"),
        species2 = str_replace(species, "3 TO 3.5 M 10' TO 11.5' BULL", "BULL"),
        species2 = str_replace(species, "3.5 M 11.5' FEMALE WHITE SHARK NAMED NOTCHFIN", "WHITE SHARK"),
        species2 = str_replace(species, "3.5 M 11.5' WHITE", "WHITE"),
        species2 = str_replace(species, "3.5 M WHITE", "WHITE"),
        species2 = str_replace(species, "3.6 M 11'9 WHITE", "WHITE"),
        species2 = str_replace(species, "3.7 M 12' WHITE", "WHITE"),
        species2 = str_replace(species, "3.7 M TO 4.3 M 12' TO 14' WHITE", "WHITE"),
        species2 = str_replace(species, "3.7M TO 4.2 M WHITE", "WHITE"),
        species2 = str_replace(species, "4 M 13' WHITE", "WHITE"),
        species2 = str_replace(species, "4 M TO 5 M 13' TO 16.5' WHITE", "WHITE"),
        species2 = str_replace(species, "4 M WHITE", "WHITE"),
        species2 = str_replace(species, "4 TO 5M WHITE", "WHITE"),
        species2 = str_replace(species, "4.2 M WHITE", "WHITE"),
        species2 = str_replace(species, "4.5 M 14'9 WHITE", "WHITE"),
        species2 = str_replace(species, "4.5 M & 5 M WHITE", "WHITE"),
        species2 = str_replace(species, "4.5 M TO 5.5 M 15' TO 18' WHITE", "WHITE"),
        species2 = str_replace(species, "4.5 M WHITE", "WHITE"),
        species2 = str_replace(species, "4.9 M TO 5.5 M 16' TO 18' WHITE", "WHITE"),
        species2 = str_replace(species, "4.9 M WHITE", "WHITE"),
        species2 = str_replace(species, "5 M 16.5' WHITE", "WHITE"),
        species2 = str_replace(species, "5 M TO 5.5 M 16.5' TO 18' WHITE", "WHITE"),
        species2 = str_replace(species, "5 M TO 6 M 16.5' TO 20' WHITE", "WHITE"),
        species2 = str_replace(species, "5 M WHITE", "WHITE"),
        species2 = str_replace(species, "5.5 M 18' WHITE", "WHITE"),
        species2 = str_replace(species, "5.5 M TO 6 M 18' TO 20' WHITE", "WHITE"),
        species2 = str_replace(species, "5' SPINNER", "SPINNER"),
        species2 = str_replace(species, "5' TO 6' SPINNER", "SPINNER"),
        species2 = str_replace(species, "6 M 20' WHITE", "WHITE"),
        species2 = str_replace(species, "6' TO 7' BLACKTIP", "BLACKTIP"),
        species2 = str_replace(species, "82	60 CM TO 90 CM 2' TO 3' BLACKTIP", "BLACKTIP"),
        species2 = str_replace(species, "8' BULL", "BULL"),
        species2 = str_replace(species, "8' GREAT HAMMERHEAD", "HAMMERHEAD"),
        species2 = str_replace(species, "8' WHITE", "WHITE"),
        species2 = str_replace(species, "9'2 WHITE", "WHITE"),
        species2 = str_replace(species, "A BULL", "BULL"),
        species2 = str_replace(species, "A SMALL REEF", "REEF"),
        species2 = str_replace(species, "A SMALL SPINNER", "SPINNER"),
        species2 = str_replace(species, "BLACKTIP REEF SHARK PUP", "BLACKTIP REEF SHARK"),
        species2 = str_replace(species, "BLUE SHARK 6'", "BLUE SHARK"),
        species2 = str_replace(species, "13	BRONZE WHALER 2.5M", "BRONZE WHALER SHARK"),
        species2 = str_replace(species, "BRONZE WHALER SHARKS X 3", "BRONZE WHALER SHARK"),
        species2 = str_replace(species, "BULL SHARKS", "BULL SHARK"),
        species2 = str_replace(species, "BULL SHARKS X 2", "BULL SHARK"),
        species2 = str_replace(species, "CARIBBEAN REEF SHARKS", "CARIBBEAN REEF SHARK"),
        species2 = str_replace(species, "GREY REEF SHARK. 2 M", "GREY REEF SHARK"),
        species2 = str_replace(species, "NURSE SHARK. 5'", "NURSE SHARK"),
        species2 = str_replace(species, "REEF SHARK X 2", "REEF SHARK"),
        species2 = str_replace(species, "SANDTIGER SHARK 2'", "SANDTIGER SHARK"),
        species2 = str_replace(species, "SEVEN-GILL SHARK", "SEVENGILL"),
        species2 = str_replace(species, "SMALL BLACKTIP SHARK", "BLACKTIP SHARK"),
        species2 = str_replace(species, "SMALL BROWN SHARK", "BROWN SHARK"),
        species2 = str_replace(species, "SMALL BULL SHARK", "BULL SHARK"),
        species2 = str_replace(species, "SMALL HAMMERHEAD SHARK", "HAMMERHEAD SHARK"),
        species2 = str_replace(species, "SMALL NURSE SHARK", "NURSE SHARK"),
        species2 = str_replace(species, "TIGER SHARK SAID TO BE 5 TO 7 M 16.5' TO 23'", "TIGER SHARK"),
        species2 = str_replace(species, "WHITE SHARK (TOOTH FRAGMENT RECOVERED)", "WHITE SHARK"),
        species2 = str_replace(species, "WHITE SHARK X 2", "WHITE SHARK"), 
        species2 = str_replace(species, "BULL SHARK PUP", "BULL SHARK"))

clean_species %>% 
  filter(year >= 1990 &
           year < 2019) %>% 
  filter(type == "UNPROVOKED") %>% 
  count(species2) %>% View()

# there are a few attacks missing
setdiff(2:6430, shark_attacks_clean$original_order)

united_states <- shark_attacks_clean %>% 
  filter(country == "USA")

united_states %>% 
  filter(year == 2018) %>% 
  filter(type == "UNPROVOKED") %>% 
  count(area)

# graphic
graphic2 <- united_states %>% 
  filter(year >= 1990 &
           year <= 2019) %>% 
  count(year, type) %>% 
  filter(type == "UNPROVOKED") 

write_csv(graphic2, "graphic2.csv")

graphic2 %>% 
  ggplot(aes(year, n, group = 1))+
  geom_line()

# graphic
graphic1 <- shark_attacks_clean %>%   
  filter(year >= 1990 &
         year <= 2019) %>% 
  count(year, type) %>% 
  filter(type == "UNPROVOKED") 

write_csv(graphic1, "graphic1.csv")

graphic1 %>% 
  ggplot(aes(year, n, group = 1))+
  geom_line()

shark_attacks_clean %>% 
  filter(year >= 1990 &
           year < 2019) %>% 
  filter(type == "UNPROVOKED") %>% 
  count(year) %>% 
  mutate(mean = mean(n))

shark_attacks_clean %>% 
  filter(year >= 1990 &
           year < 2019) %>% 
  filter(type == "UNPROVOKED")

# graphic
graphic3 <- united_states %>% 
  filter(year >= 1990 &
           year < 2019) %>% 
  filter(type == "UNPROVOKED") %>% 
  count(area) %>% 
  arrange(desc(n))

write_csv(graphic3, "graphic3.csv")

united_states %>% 
  filter(year == 2018) %>% 
  filter(type == "UNPROVOKED") %>% 
  filter(fatal_y_n == "Y")

united_states %>% 
  filter(year == 2017) %>% 
  filter(type == "UNPROVOKED") %>% 
  filter(fatal_y_n == "Y")
  
shark_attacks_clean %>% 
    filter(year == 2018) %>% 
    filter(type == "UNPROVOKED") %>% 
    filter(fatal_y_n == "Y") %>% View()
  
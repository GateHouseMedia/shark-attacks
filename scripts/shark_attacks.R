library(tidyverse)
library(janitor)
library(here)

# ran python script to clean characters
# re-imported data on 7/18

shark_attacks <- read_csv(here("sharks", "data", "shark_attacks_cln.csv"), trim_ws = TRUE, skip_empty_rows = TRUE) %>% 
  as_tibble() %>% 
  clean_names()
  
shark_attacks_clean <- shark_attacks %>% 
  filter(case_number > 0,
         case_number != "xx") %>% 
  mutate_all(funs(toupper)) %>% 
  mutate_at(vars(case_number:original_order), str_squish) %>%
  mutate(year = as.numeric(year))

# there are a few attacks missing
setdiff(2:6430, shark_attacks_clean$original_order)

shark_species <- "STARRY SMOOTHHOUND|SOUPFIN|LEOPARD|THRESHER|BROWN|COCKTAIL|COW|C. ALBIMARGINATUS|BANJO|CATSHARK|SPEAR-EYE|BLUE POINTER|TAWNY NURSE|WHITETIP REEF|OCEANIC WHITETIP|SANDTIGER|SAND TIGER|TIGER|BULL|NURSE|BRONZE WHALER|WHALER|WHALE|WOBBEGONG|SHORTFIN MAKO|MAKO|RAGGEDTOOTH|BLACKTIP REEF|BLACKTIP|GREY NURSE|LEMON|BLUE|HAMMERHEAD|ZAMBESI|ZAMBEZI|GREY REEF|GRAY REEF|SPINNER|CARIBBEAN REEF|BASKING|REEF|SANDBAR|SAND|ANGEL|CARPET|DUSKY|PORBEAGLE|GALAPAGOS|DOGFISH|7-GILL|BROADNOSE SEVENGILL|SEVENGILL|SEVEN-GILL|COOKIECUTTER|SALMON|COPPER|GOBLIN|SHOVELNOSE|SILKY|SILVERTIP|GRAY|GREY|WHITE"

maybes <- " OR |\\?|POSSIBLY|POSSIBILY|THOUGHT|SAID|REPORTED AS|MAY HAVE|WAS CAUGHT IN THE VICINITY|GREY-COLORED|GREY COLORED|STINGRAY|\"GRAY SHARK\"|\"GREY SHARK\"|\"WHALER SHARK\"|1.5 TO 2.5 M \\[5' TO 8.25'\\] COPPER SHARK|BULL SHARK CAUGHT, LEG RECOVERED & BURIED BESIDE THE MAN'S BODY|SMALL BROWN SHARK"

# just cleaning on 1990 - 2018 results
species_cleaned <- shark_attacks_clean %>%
  mutate(species_clean = str_extract(species, shark_species)) %>%
  mutate(species_clean = case_when(str_detect(species, maybes) ~ "MAYBE",
                                   TRUE ~ species_clean)) %>% 
  mutate(species_clean = str_replace(species_clean, "ZAMBESI", "ZAMBEZI")) %>% 
  mutate(species_clean = str_replace(species_clean, "GREY", "GRAY")) %>% 
  mutate(species_clean = str_replace(species_clean, "SANDTIGER", "SAND TIGER")) %>% 
  mutate(species_clean = str_replace(species_clean, "7-GILL|SEVENGILL", "SEVEN-GILL")) %>% 
  mutate(species_clean = str_replace(species_clean, "COPPER", "BRONZE WHALER")) # there are only two described as copper after 1990. after checking the reports, one is for sure a bronze whaler, the other should be classified as a maybe/unverified. that one is now in the maybe script 

top_species <- species_cleaned %>% 
  filter(year >= 1990 &
           year < 2019) %>% 
  filter(type == "UNPROVOKED") %>% 
  count(species_clean, sort = T)

write_csv(top_species, "top_species.csv")

united_states <- shark_attacks_clean %>% 
  filter(country == "USA")

united_states %>% 
  filter(year == 2018) %>% 
  filter(type == "UNPROVOKED") %>% 
  count(area, sort = T)

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
           year <= 2019) %>% 
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
    filter(type == "UNPROVOKED") 

shark_attacks_clean %>% 
  filter(year == 2017) %>% 
  filter(type == "UNPROVOKED") 
  
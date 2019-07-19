library(tidyverse)
library(janitor)
library(here)

# ran python script to clean characters
# re-imported data on 7/18


shark_attacks <- read_csv(here("sharks", "data", "shark_attacks_cln.csv"), trim_ws = TRUE, skip_empty_rows = TRUE) %>% 
  as_tibble() %>% 
  clean_names()
  
# clean out empty rows, remove white space
shark_attacks_clean <- shark_attacks %>% 
  filter(case_number > 0,
         case_number != "xx") %>% 
  mutate_all(funs(toupper)) %>% 
  mutate_at(vars(case_number:original_order), str_squish) %>%
  mutate(year = as.numeric(year))

# there are a few attacks missing
setdiff(2:6430, shark_attacks_clean$original_order)

# df of all the shark species
shark_species <- "STARRY SMOOTHHOUND|SOUPFIN|LEOPARD|THRESHER|BROWN|COCKTAIL|COW|C. ALBIMARGINATUS|BANJO|CATSHARK|SPEAR-EYE|BLUE POINTER|TAWNY NURSE|WHITETIP REEF|OCEANIC WHITETIP|SANDTIGER|SAND TIGER|TIGER|BULL|NURSE|BRONZE WHALER|WHALER|WHALE|WOBBEGONG|SHORTFIN MAKO|MAKO|RAGGEDTOOTH|BLACKTIP REEF|BLACKTIP|GREY NURSE|LEMON|BLUE|HAMMERHEAD|ZAMBESI|ZAMBEZI|GREY REEF|GRAY REEF|SPINNER|CARIBBEAN REEF|BASKING|REEF|SANDBAR|SAND|ANGEL|CARPET|DUSKY|PORBEAGLE|GALAPAGOS|DOGFISH|7-GILL|BROADNOSE SEVENGILL|SEVENGILL|SEVEN-GILL|COOKIECUTTER|SALMON|COPPER|GOBLIN|SHOVELNOSE|SILKY|SILVERTIP|GRAY|GREY|WHITE"

# df of words/phrases that are used for unconfirmed species
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

# df of all the shark species
activity_types <- "BODY BOARDING|BODY SURFING|BOOGIE BOARDING|ABALONE|SCUBA DIVING|HOOKAH|FREE DIVING|FREEDIVING|DIVING|FISHING|FLOATING|KAYAKING|KITE SURFING|PADDLE BOARDING|PADDLING|PLAYING|SPEARFISHING|SITTING|STANDING|SURFING|SWIMMING|TREADING WATER|WADE FISHING|WADING|WADING / FISHING|WALKING|WINDSURFING|SNORKELING|PADDLEBOARDING|SURF SKIING|SURF-SKIING|FOILBOARDING|KAYAKING / FISHING|CANOEING|SUP|SKIMBOARDING|KAKAYING|LOBSTERING|LOBSTER FISHING|IN WATER WITH DIVING SEABIRDS|ROWING|SAILING|KITEBOARDING|KITE BOARDING|BODYBOARDING|PADDLE-BOARDING|SURFNG|STAMDING|KITE-BOARDING|BODY-BOARDING|ATTEMPTING TO REMOVE FISHING NET FROM SUBMERGED OBJECT|CAPSIZED FISHING BOAT|FIVE MEN ON MAKESHIFT RAFT|WADE-FISHING|FREE DIVING / SPEARFISHING|FREE DIVING & SPEARFISHING"

# df of words/phrases that are used for unconfirmed species
activity_maybe <- "\\?"

activities_cleaned <- shark_attacks_clean %>% 
  mutate(activities_clean = str_extract(activity, activity_types)) %>%
  mutate(activities_clean = case_when(str_detect(activity, activity_maybe) ~ "MAYBE",
                                   TRUE ~ activities_clean)) %>% 
  mutate(activities_clean = str_replace(activities_clean, "STAND-UP PADDLEBOARDING|SUP|SUP FOIL BOARDING|PADDLEBOARDING|PADDLE-BOARDING", "PADDLE BOARDING")) %>% 
  mutate(activities_clean = str_replace(activities_clean, "WADING / FISHING|WADE-FISHING", "WADE FISHING")) %>% 
  mutate(activities_clean = str_replace(activities_clean, "SURF-SKIING", "SURF SKIING")) %>% 
  mutate(activities_clean = str_replace(activities_clean, "LOBSTER FISHING", "LOBSTERING")) %>% 
  mutate(activities_clean = str_replace(activities_clean, "IN WATER WITH DIVING SEABIRDS", "OTHER")) %>% 
  mutate(activities_clean = str_replace(activities_clean, "FREEDIVING", "FREE DIVING")) %>% 
  mutate(activities_clean = str_replace(activities_clean, "HOOKAH", "HOOKAH DIVING")) %>% 
  mutate(activities_clean = str_replace(activities_clean, "ABALONE", "SWIMMING/DIVING FOR ABALONE")) %>% 
  mutate(activities_clean = str_replace(activities_clean, "KITEBOARDING|KITE-BOARDING", "KITE BOARDING")) %>% 
  mutate(activities_clean = str_replace(activities_clean, "BODYBOARDING", "BODY BOARDING")) %>% 
  mutate(activities_clean = str_replace(activities_clean, "SURFNG", "SURFING")) %>% 
  mutate(activities_clean = str_replace(activities_clean, "STAMDING", "STANDING")) %>% 
  mutate(activities_clean = str_replace(activities_clean, "BODY-BOARDING", "BODY BOARDING")) %>% 
  mutate(activities_clean = str_replace(activities_clean, "BODY BOARDING|BODY SURFING|BOOGIE BOARDING", "BODY BOARDING, BODY SURFING OR BOOGIE BOARDING")) %>%
  mutate(activities_clean = str_replace(activities_clean, "ATTEMPTING TO REMOVE FISHING NET FROM SUBMERGED OBJECT", "OTHER")) %>% 
  mutate(activities_clean = str_replace(activities_clean, "CAPSIZED FISHING BOAT", "OTHER")) %>% 
  mutate(activities_clean = str_replace(activities_clean, "FIVE MEN ON MAKESHIFT RAFT", "OTHER")) %>% 
  mutate(activities_clean = str_replace(activities_clean, "FREE DIVING / SPEARFISHING|FREE DIVING & SPEARFISHING", "FREE DIVING AND SPEAR FISHING")) %>% 
  mutate(activities_clean = replace_na(activities_clean, "OTHER"))

activities <- activities_cleaned %>% 
  filter(year >= 1990 &
           year < 2019) %>% 
  filter(type == "UNPROVOKED") %>% 
  count(activities_clean, sort = T) %>% 
  filter(activities_clean != "MAYBE")

write_csv(activities, "activities.csv")

unclean_activities <- activities_cleaned %>% 
  filter(year >= 1990 &
           year < 2019) %>% 
  filter(type == "UNPROVOKED") %>% 
  count(activities_clean, activity)

write_csv(unclean_activities, "unclean_activities.csv")

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
graphic3 <- united_states %>% 
  filter(year >= 1990 &
           year <= 2019) %>% 
  filter(type == "UNPROVOKED") %>% 
  count(area) %>% 
  arrange(desc(n))

write_csv(graphic3, "graphic3.csv")

### numbers for story ### 

# average attacks every year between 1990 and 2018
shark_attacks_clean %>% 
  filter(year >= 1990 &
           year < 2019) %>% 
  filter(type == "UNPROVOKED") %>% 
  count(year) %>% 
  mutate(mean = mean(n))

# total number of unprovoked attacks between 1990 and 2018
shark_attacks_clean %>% 
  filter(year >= 1990 &
           year < 2019) %>% 
  filter(type == "UNPROVOKED")

# all attacks in the same time frame
shark_attacks_clean %>% 
  filter(year >= 1990 &
           year < 2019)

# how many of those were fatal?
shark_attacks_clean %>% 
  filter(year >= 1990 &
           year < 2019) %>% 
  filter(type == "UNPROVOKED") %>% 
  filter(fatal_y_n == "Y")

# 1990 numbers
shark_attacks_clean %>% 
  filter(year == 1990) %>% 
  filter(type == "UNPROVOKED") %>% 
  filter(fatal_y_n == "Y")

# 2015 numbers
shark_attacks_clean %>% 
  filter(year == 2015) %>% 
  filter(type == "UNPROVOKED") %>% 
  filter(fatal_y_n == "Y")


# just us attacks
united_states <- shark_attacks_clean %>% 
  filter(country == "USA")

# us attacks in 2018
united_states %>% 
  filter(year == 2018) %>% 
  filter(type == "UNPROVOKED") %>% 
  count(area, sort = T)

# deadly attacks in the us in 2017
united_states %>% 
  filter(year == 2017) %>% 
  filter(fatal_y_n == "Y")

# deadly attacks in the us in 2018
united_states %>% 
  filter(year == 2018) %>% 
  filter(fatal_y_n == "Y")

# deadly attacks in the us in 2019
united_states %>% 
  filter(year == 2019) %>% 
  filter(fatal_y_n == "Y")

# deadly attacks globally in 2019
shark_attacks_clean %>% 
  filter(year == 2019) %>% 
  filter(fatal_y_n == "Y")

# deadly attacks globally in 2019
shark_attacks_clean %>% 
  filter(year > 2013, 
         year <= 2019) %>% 
  filter(fatal_y_n == "Y") %>% View()

# number of unprovoked attacks globally in 2018
shark_attacks_clean %>% 
    filter(year == 2018) %>% 
    filter(type == "UNPROVOKED") 

# number of unprovoked attacks globally in 2017
shark_attacks_clean %>% 
  filter(year == 2017) %>% 
  filter(type == "UNPROVOKED") 

# how many occured in volusia county?
united_states %>% 
  filter(year >= 1990) %>% 
  filter(type == "UNPROVOKED") %>% 
  filter(str_detect(location, "VOLUSIA COUNTY")) %>% View()

# breakdown of other counties, but this isn't very clean
united_states %>% 
  filter(year >= 1990) %>% 
  filter(type == "UNPROVOKED") %>% 
  separate(location, c("location1", "location2", "location3", "location4"), sep = ", ") %>% 
  count(year, location2, sort = T) %>% View()

united_states %>% 
  filter(year >= 1990) %>% 
  filter(type == "UNPROVOKED") %>% 
  count(time, sort = T) %>% View()

united_states %>% 
  filter(year >= 1990 &
           year < 2019)  %>% 
  filter(type == "UNPROVOKED") %>% 
  count(activity, sort = T) %>% View()

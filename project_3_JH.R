# Jane Huang
# Project 3

library(purrr)
library(plotly)
library(dplyr)
library(tidyverse)
library(jsonlite)
library(data.tree)
library(sunburstR)

#setwd("/Users/janehuang/Desktop/Advanced DV/project_3")

bill_spot <- read_csv("billspot_music_data (1).csv")

genre_strings <- unlist(bill_spot$spotify_genre)
extracted_genres <- str_extract_all(genre_strings, "(?<=')[^']+(?=')")
all_genres <- unlist(extracted_genres)
unique_genres <- unique(all_genres)

genres_tibble <- as_tibble(unique_genres) %>%
  rename(genre = value) %>%
  filter(genre != ", ") %>%
  mutate(genre_cat_1 = case_when(
    str_detect(genre, "country") ~ "country",
    TRUE ~ "other")) %>%
  mutate(genre_cat_2 = case_when(
    str_detect(genre, "rock") ~ "rock",
    TRUE ~ "other")) %>%
  mutate(genre_cat_3 = case_when(
    str_detect(genre, "folk") ~ "folk",
    TRUE ~ "other")) %>%
  mutate(genre_cat_4 = case_when(
    str_detect(genre, "metal") ~ "metal",
    TRUE ~ "other")) %>%
  mutate(genre_cat_5 = case_when(
    str_detect(genre, "blues") ~ "blues",
    TRUE ~ "other")) %>%
  mutate(genre_cat_6 = case_when(
    str_detect(genre, "soul") ~ "soul",
    TRUE ~ "other"))

genres_tibble_2 <- genres_tibble %>%
  mutate(genre_cat_all = str_c(genre_cat_1, genre_cat_2, genre_cat_3, 
                               genre_cat_4, genre_cat_5, genre_cat_6, 
                               sep = " ")) %>%
  mutate(genre_cat_all = str_replace_all(genre_cat_all, "other", "")) %>%
  mutate(genre_cat_all = str_squish(genre_cat_all)) %>%
  mutate(genre_cat_all = if_else(genre_cat_all == "", "other", genre_cat_all)) %>%
  mutate(genre_cat_all = case_when(genre_cat_all == "blues soul" ~ "blues",
                                   genre_cat_all == "country rock" ~ "country",
                                   genre_cat_all == "rock blues" ~ "blues",
                                   genre_cat_all == "rock folk" ~ "folk",
                                   TRUE ~ genre_cat_all))

genre_tibble_final <- genres_tibble_2 %>%
  select(genre, genre_cat_all) %>%
  rename(genre_detailed = genre,
         genre_broad = genre_cat_all)

# Function to split genres in bill_spot
split_genres <- function(df, genre_col) {
  df %>%
    mutate(spotify_genre = str_remove_all(!!sym(genre_col), "\\[|\\]|'")) %>%
    separate(spotify_genre, into = paste0("genre_detailed_", 1:10), sep = ",\\s*", fill = "right")
}

bill_spot_split <- split_genres(bill_spot, "spotify_genre")

genres_long <- bill_spot_split %>%
  pivot_longer(cols = starts_with("genre_detailed_"), names_to = "genre_col", values_to = "genre") %>%
  left_join(genre_tibble_final, by = c("genre" = "genre_detailed")) %>%
  rename(genre_detailed = genre)

genres_wide <- genres_long %>%
  pivot_wider(names_from = genre_col, values_from = c(genre_detailed, genre_broad), 
              names_glue = "{genre_col}_{.value}")

bill_spot_split_cleaned <- genres_wide %>%
  mutate(genre_broad_conc = pmap_chr(list(
    genre_detailed_1_genre_broad, genre_detailed_2_genre_broad, genre_detailed_3_genre_broad,
    genre_detailed_4_genre_broad, genre_detailed_5_genre_broad, genre_detailed_6_genre_broad,
    genre_detailed_7_genre_broad, genre_detailed_8_genre_broad, genre_detailed_9_genre_broad,
    genre_detailed_10_genre_broad), ~ str_c(na.omit(c(...)), collapse = " "))) %>%
  mutate(genre_broad_conc = str_replace_all(genre_broad_conc, "other", "")) %>%
  mutate(genre_broad_conc = str_squish(genre_broad_conc)) %>%
  mutate(genre_broad_conc = str_split(genre_broad_conc, " ")) %>%
  mutate(genre_broad_conc = map(genre_broad_conc, ~ str_c(sort(unique(.)), collapse = ", "))) %>%
  unnest(cols = c(genre_broad_conc))

bill_spot_split_cleaned_2 <- bill_spot_split_cleaned %>%
  mutate(genre_most_broad = case_when(genre_broad_conc == "blues, metal, rock" ~ "blues rock",
                                      genre_broad_conc == "blues, country, folk, rock" ~ "blues rock",
                                      genre_broad_conc == "blues, country, rock" ~ "blues rock",
                                      genre_broad_conc == "blues, folk, rock" ~ "blues rock",
                                      genre_broad_conc == "blues, rock, soul" ~ "soul",
                                      genre_broad_conc == "country, folk" ~ "country rock",
                                      genre_broad_conc == "country, folk, metal, rock" ~ "rock",
                                      genre_broad_conc == "country, folk, rock" ~ "rock",
                                      genre_broad_conc == "country, folk, rock" ~ "rock",
                                      genre_broad_conc == "rock, soul" ~ "soul",
                                      genre_broad_conc == "folk, rock" ~ "folk rock",
                                      genre_broad_conc == "blues, rock" ~ "blues rock",
                                      genre_broad_conc == "metal, rock" ~ "blues rock",
                                      TRUE ~ genre_broad_conc)) %>%
  mutate(genre_most_broad = case_when(song %in% c("Street Corner Serenade", "If Looks Could Kill", 
                                                  "Made To Love You", "Green-Eyed Lady") ~ "rock",
                                      song %in% c("Love Ain't", "Sal's Got A Sugar Lip", 
                                                  "Even If It Breaks Your Heart") ~ "country",
                                      TRUE ~ genre_most_broad))


bill_spot_split_cleaned_4 <- bill_spot_split_cleaned_2 %>%
  mutate(genre_most_broad = str_to_title(genre_most_broad),
         genre_detailed_1_genre_detailed = str_to_title(genre_detailed_1_genre_detailed)) %>%
  mutate(song = case_when(song == "Stay/The Load-Out" ~ "Stay - The Load-Out",
                          song == "Something Happened On The Way To Heaven" ~ "Something Happened...",
                          song == "A Whole New World (Aladdin's Theme)" ~ "A Whole New World",
                          song == "(She's A) Very Lovely Woman/The Long Way Around" ~ "Very Lovely Woman",
                          song == "Bells, Bells, Bells (The Bell Song)" ~ "Bells, Bells, Bells",
                          song == "99.9% Sure (I've Never Been Here Before)" ~ "99.9% Sure",
                          song == "I'd Do Anything For Love (But I Won't Do That)" ~ "I'd Do Anything For Love",
                          song == 'Hold Me, Thrill Me, Kiss Me, Kill Me (From "Batman Forever")' ~ "Hold Me, Thrill Me...",
                          song == "New York Mining Disaster 1941 (Have You Seen My Wife, Mr. Jones)" ~ "New York Mining Disaster 1941",
                          song == 'I Finally Found Someone (From "The Mirror Has Two Faces")' ~ "I Finally Found Someone",
                          song == "Don't Know What You Got (Till It's Gone)" ~ "I Finally Found Someone",
                          song == "Here I Am (Just When I Thought I Was Over You)" ~ "Here I Am",
                          song == "Me And Julio Down By The Schoolyard" ~ "Me And Julio...",
                          song == 'Shakedown (From "Beverly Hills Cop II")' ~ "Shakedown",
                          song == 'Streets Of Philadelphia (From "Philadelphia")' ~ "Streets Of Philadelphia",
                          song == "Where The Stars And Stripes And The Eagle Fly" ~ "Where The Stars And Stripes",
                          song == "Objects In The Rear View Mirror May Appear Closer Than..." ~ "Objects In The Rear View",
                          song == "Another Day/Oh Woman Oh Why" ~ "Another Day",
                          song == "The Main Event/Fight" ~ "The Main Event",
                          song == "You Can't Roller Skate In A Buffalo Herd" ~ "You Can't Roller Skate...",
                          song == "Theme From The Dukes Of Hazzard (Good Ol' Boys)" ~ "Theme From The Dukes...",
                          song == "You've Never Been This Far Before" ~ "You've Never Been This...",
                          song == "The Wreck Of The Edmund Fitzgerald" ~ "The Wreck Of The Edmund...",
                          TRUE ~ song)) %>%
  mutate(pathString = paste("Root", genre_most_broad, genre_detailed_1_genre_detailed, song, sep = "/")) %>%
  mutate(value = Weeks.on.Chart) %>%
  select(song, value, genre_most_broad, 
         genre_detailed_1_genre_detailed, pathString) 

  
tree <- as.Node(bill_spot_split_cleaned_4, pathDelimiter = "/")

list_structure <- ToListExplicit(tree, unname = TRUE)

json_data <- toJSON(list_structure, pretty = TRUE)

write(json_data, file = "sunburst_data_upper.json")


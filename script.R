### LOAD LIBRARIES ----
library(tidyverse) 
library(SetListR)
library(spotifyr)

### SETUP ----
# Get and API key for setlist.fm API https://www.setlist.fm/settings/apps
api_key <- 'Paste your setlist.fm API key'

# Get a Client ID and and Client Secret from Spotify API https://developer.spotify.com/dashboard/applications
Sys.setenv(SPOTIFY_CLIENT_ID = 'Paste your Client ID')
Sys.setenv(SPOTIFY_CLIENT_SECRET = 'Paste you Client Secret')
access_token <- get_spotify_access_token()

# artist mbid from musicbrainz.org. 
# Search for the artist and copy the id from the url. E.g https://musicbrainz.org/artist/0383dadf-2a4e-4d10-a46a-e9e041da8eb3
artist_id = '69ee3720-a7cb-4402-b48d-a02c366f2bcf'
artist_name = 'The Cure'
page_number = 154

### LOAD FUNCTIONS ----
source("functions.R")

### GET DATA ----
# Create three dataframes using the functions
setlist_data <- get_setlists(api_key, artist_id, artist_name, page_number)
song_data <- get_songs(artist_name)
album_data <- get_albums(artist_name)

# Join setlist data and song data to get some more data about the songs played live 
setlist_spotify <- setlist_data %>%
        left_join(song_data, by= c('code'))

### WRITE DATA ----
write_csv(setlist_spotify, "setlist_spotify.csv")
write_csv(album_data, "album_data.csv")

# Creates a dataframe of all songs on all concerts, from the specified number of pages on Setlist FM
# If too many pages are specified, the function through an error
get_setlists <- function(api_key, id, artist_name, pages) {
        
        
        # Initialise an empty data frame
        all_pages <- NULL
        # Divide number of pages by 2, because www.setlist.fm has 10 concets per page, while the API has 20.
        api_pages <- ceiling(pages/2)
        
        # This for-loop goes through all pages on Setlist FM, from 1 to n specified among the function arguments
        for (x in 1:api_pages) {
                
                # Create one list per Setlist FM page
                setlists <- searchSetLists(
                        API_KEY = api_key, 
                        mbid = id, 
                        artistName = artist_name, 
                        page = x)
                
                # Initiate an empty data frame
                one_page <- NULL
                
                # The 5th element of the list contains a list with all concerts from that page. 
                # Calculate the length of the lists (i.e. how many concerts are on this page)
                len <- length(setlists[[5]])
                
                # This for-loop goes through every concert on the page
                for (i in 1:len) {
                        
                        # take the i element of the the list - that would be one concert list
                        concert <- setlists[[5]][[i]]
                        
                        # create a data frame for venues
                        venue <- as.data.frame(concert$venue)%>%
                                mutate(id = concert$id) %>%
                                select(id, name, city.name, city.coords.lat, city.coords.long, city.country.name) %>%
                                rename(venue_name = name)
                        
                        # create a data frame for the tour
                        tour <- as.data.frame(concert$tour)%>%
                                mutate(id = concert$id, date = concert$eventDate)
                        
                        # if a concert does not have any songs registered on Setlist FM (can happen with old ones)
                        if (dim(as.data.frame(concert$sets))[1] == 0) {
                                
                                # create an empty data frame with three columns: concert id, song name, song order
                                songs <- data.frame(matrix(ncol = 3, nrow = 1))
                                x <- c("song_name", "id", "order")
                                colnames(songs) <- x
                                
                                # populate the data frame with values 
                                songs <- songs %>% 
                                        mutate(id = concert$id, song_name = NULL, order = 0)    
                        } 
                        
                        # otherwise, if concert has songs...
                        else {
                                # create a data frame with all songs on the concert
                                songs <-as.data.frame(concert$sets) %>%
                                        mutate_at(vars(starts_with("set.")),funs(as.character)) %>%
                                        select(starts_with("set.song.name")) %>%
                                        # pivot the data frame to get the long structure - two columns, multiple rows
                                        gather(key = "song_number", value = "song_name") %>%
                                        # add concert id
                                        mutate(id = concert$id) %>%
                                        # assign row number as the song order number
                                        mutate(order = row_number()) 
                                # %>% select(-song_number) 
                                
                        }
                        
                        # combine songs, tour and venue data frames
                        concert_df <- songs %>%
                                left_join(tour) %>%
                                left_join(venue, by = 'id')
                        
                        # add every new concert from the page to one_page data frame
                        one_page <- bind_rows(one_page, concert_df)
                        
                }
                
                # add every new page to all_pages data frame
                all_pages <- bind_rows(all_pages, one_page) 
                
        } 
        all_pages <- all_pages %>%
                mutate(code =  str_replace_all(str_replace_all(tolower(song_name), " ", ""), "[[:punct:]]", "")) %>%
                select(-song_number)
        
        return(all_pages)
}

# Creates a dataframe with every unique song of the artist.
# Each song has a set of attributes - the first album it was released on, max valence, mean duration, release date, all albums it appeared on, etc. 
get_songs <- function(artist_name){
        
        # Get all artst albums and compilations (and singles)
        albums <- get_artist_albums(artist_name) %>%
                rbind(get_artist_albums(artist_name, album_types = "compilation")) %>%
                # rbind(get_artist_albums(name, album_types = "single")) %>%
                # Filter only albums in your list
                # filter(album_name %in% disc) %>%
                # Create an album_code without spaces and punctuation. It will be used for a join with 'track' dataframe
                mutate(album_code =  str_replace_all(str_replace_all(tolower(album_name), " ", ""), "[[:punct:]]", ""))
        
        # Get popularity score per album_uri
        alb_pop <- get_album_popularity(albums)
        
        # Get all tracks per album
        tracks <- get_album_tracks(albums) %>%
                # Create and album_code without spaces and punctuation. It will be used for a join with 'albums' dataframe
                mutate(album_code =  str_replace_all(str_replace_all(tolower(album_name), " ", ""), "[[:punct:]]", "")) %>%
                select(-album_name)
        
        # Get song attributes for every track_uri
        audio <- get_track_audio_features(tracks)
        
        # Get track popularity for every track_uri
        track_pop <- get_track_popularity(tracks)
        
        # Join it all together into a nice dataframe
        full<- albums %>%
                left_join(alb_pop) %>%
                left_join(tracks, by = 'album_code') %>%
                left_join(audio) %>%
                left_join(track_pop) %>%
                # Clean up song names by removing characters that follow after - and (
                separate(track_name, into = c("song_name", "detail_a"), sep = " - ") %>%
                separate(song_name, into = c("song_name", "detail_b"), sep = '\\(') %>%
                # If the code is empty, use detail_a as the song_name
                mutate(song_name = if_else(song_name == "", detail_b, song_name)) %>%
                select(-detail_a, -detail_b) %>%
                # Add a song code without spaces and punctuation - unique for every song
                mutate(code =  str_replace_all(str_replace_all(tolower(song_name), " ", ""), "[[:punct:]]", "")) %>%
                # Temporary group_by to calculate one value of each attributes 
                # for songs that have multiple appearances on different albums
                group_by(code) %>%
                mutate(release_date = min(album_release_date), # the earliest release date
                       # pick the earliest album that contained the song
                       album = (if_else(release_date == album_release_date, album_name, NULL)), 
                       # pick the track_uri of the earliest track appearance
                       track_uri = if_else(release_date == album_release_date, track_uri, NULL),
                       # create a string of all albums that contain the song
                       all_albums = paste0(album_name, collapse = "; "), 
                       duration_ms = mean(duration_ms), 
                       # take max popularity
                       popularity = max(track_popularity), 
                       # arguably, take the highest value for valence
                       valence = max(valence),
                       song_name = min(song_name),
                       album = min(album, na.rm = TRUE),
                       track_uri = min(id, na.rm = TRUE)) %>%
                distinct(track_uri, code, album, all_albums, duration_ms, popularity, valence, release_date, song_name) 
}

# Creates a datafrane with all songs on all albums, compilations and singles. 
# The dataframe contains album attriutes (release date, popularity, umage url) and song attriutes (popularity, beats per minute, valence, etc)
get_albums <- function(artist_name) {
        
        # Get all artst albums and compilations (and singles)
        albums <- get_artist_albums(artist_name) %>%
                rbind(get_artist_albums(artist_name, album_types = "compilation")) %>%
                rbind(get_artist_albums(artist_name, album_types = "single")) %>%
                # Filter only albums in your list
                # filter(album_name %in% disc) %>%
                # Create an album_code without spaces and punctuation. It will be used for a join with 'track' dataframe
                mutate(album_code =  str_replace_all(str_replace_all(tolower(album_name), " ", ""), "[[:punct:]]", ""))
        
        # Get popularity score per album_uri
        alb_pop <- get_album_popularity(albums)
        
        # Get all tracks per album
        tracks <- get_album_tracks(albums) %>%
                # Create and album_code without spaces and punctuation. It will be used for a join with 'albums' dataframe
                mutate(album_code =  str_replace_all(str_replace_all(tolower(album_name), " ", ""), "[[:punct:]]", "")) %>%
                select(-album_name)
        
        # Get song attributes for every track_uri
        audio <- get_track_audio_features(tracks)
        
        discography <- albums %>%
                left_join(tracks) %>%
                left_join(alb_pop) %>%
                left_join(audio) %>%
                # Clean up song names by removing characters that follow after - and (
                separate(track_name, into = c("song_name", "detail_a"), sep = " - ") %>%
                separate(song_name, into = c("song_name", "detail_b"), sep = '\\(') %>%
                # Add a song code without spaces and punctuation - unique for every song
                mutate(code =  str_replace_all(str_replace_all(tolower(song_name), " ", ""), "[[:punct:]]", "")) %>%
                # If the code is empty, use detail_a as the song_name
                mutate(song_name = if_else(is.na(song_name), detail_b, song_name)) %>%
                select(-detail_a, -detail_b)
        
}

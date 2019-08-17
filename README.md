# music-data
Functions to extract data from setlist.fm API and Spotify API

1. Make sure you have the following packages installed:
- spotifyr
- SetListR
- tidyverse
If they are not installed, run install.packages("spotifyr"), etc.

2. Open script.r
Follow the instructions on how to get API keys from setlist.fm and Spotify.
Fill in the keys in placeholders. 

3. Pick an artist and specify the following parameters
- artist_id (can be found on musicbrainz.org)
- artist_name
- page_number (number of pages you want to extract from setlist.fm. There are 10 setlist per page in reverse chronological order)

4. Run the rest of the code to get two csv files
- all songs played on all concerts
- all songs on all albums, singles and compilations

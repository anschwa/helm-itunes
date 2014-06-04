Helm iTunes
===========

Search and play your music with iTunes or Spotify from inside Emacs.

![helm-itunes screenshot](https://raw.githubusercontent.com/daschwa/helm-itunes/master/screenshot.png)

# Usage

`M-x helm-itunes` and search for either a song, artist, or album in your iTunes 
library.

(The search begins after you’ve typed at least 2 characters. You can use space-separated terms for psuedo-fuzzy matching.)

`M-x helm-itunes-player` to play songs through Spotify instead of iTunes.

# Status
Ready to use.

Currently only supports OS X.

### Bugs:

- Song tittles with parenthesis or non-alphanumeric characters will sometimes not play.

- Each search is somehow being repeated, causing iTunes to open and close various times.

# Credits
Inspired by Kris Jenkins' [helm-spotify](https://github.com/krisajenkins/helm-spotify).
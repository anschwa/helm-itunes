;;; helm-itunes.el --- Play local iTunes and Spotify tracks
;; Copyright 2014 Adam Schwartz
;;
;; Author: Adam Schwartz <adam@adamschwartz.io>
;; URL: https://github.com/daschwa/helm-itunes
;;
;; Created: 2014-06-02
;; Version: 0.0.1
;; Package-Requires: ((helm "1.6.1"))

;;; Commentary:
;;
;; A search & play interface for iTunes and Spotify.
;;
;; You can search for either a song, artist, or album.
;;
;; Bugs:
;; Symbols in song titles will sometimes prevent the song from playing.
;; iTunes will open and close various times upon a search.
;;
;; Currently only supports OS X.
;;
;; Inspired by helm-spotify: https://github.com/krisajenkins/helm-spotify
;;

;;; Code:

(require 'helm)

;; Change the music player to Spotify
(defvar music-player nil "initialize spotify-player variable")
(setq-default music-player "itunes")

(defun helm-itunes-player (player)
  "Choose Spotify or iTunes as the music player"

  (interactive "sPlay music through Spotify? (y/n): ")
  (if (or (equal (downcase player) "y")
          (equal (downcase player) "yes"))
      (progn      (message "Player set to Spotify")
                  (setq music-player "spotify"))
    (progn (message "Player set to iTunes"))
    (setq music-player "itunes")))


;; AppleScript that searches iTunes for songs.
(defun search-script (pattern)
  (format "-- toggle a variable if iTunes was running before this script was run.
if application \"iTunes\" is running then
        set irun to true
else
        set irun to false
end if

set pattern to %S

set matches to {}
tell application \"iTunes\"
        repeat with t in (file tracks whose artist contains pattern ¬
                or album contains pattern or name contains pattern)
                set matches to matches & ¬
                        {{artist of t as string, album of t as string, ¬
                                (name of t as string) & \"pattern-match-end\"}}
        end repeat

end tell

-- If you never had iTunes open, then quit after performing the search, otherwise leave it open.
if not irun then
        tell application \"iTunes\" to quit
end if

-- Return the list of songs that matched the search
return matches" pattern))


;; Return a list of matches from the results of running the AppleScript.
;; Take the string of matches separated by commas and "pattern-match-end".
;; Split the string into a list of songs, then split each song into
;; a list of artist, album, and name.
;; Finally, concatenate the list items into a single string.

(defun get-song-list (pattern)
  "Return a list of matching songs in your iTunes library"
  (mapcar (lambda (song-list)
            (split-string song-list "\\,\s")) (split-string
                                               (shell-command-to-string
                                                (format "osascript -e %S" (search-script pattern)))
                                               "pattern-match-end,\s\\|pattern-match-end")))


(defun itunes-format-track (track)
  "Given a track, return a formatted string to display"
  (let ((song (nth 2 track))
        (artist (nth 0 track))
        (album (nth 1 track)))
    (format "%S\n %S - %S" song artist album)))


(defun spotify-format-track (track)
  "Return a Spotify compatible string to play"
  (let ((song (nth 2 track))
        (artist (nth 0 track))
        (album (nth 1 track)))
    (format "spotify:local:%s:%s:%s:123" artist album song)))


;;---------- Helm Functions ----------;;


(defun itunes-seach-formatted (pattern)
  "Create the helm search results candidates"
  (mapcar (lambda (track)
            (cons (itunes-format-track track)
                  (if (equal music-player "spotify")
                      (spotify-format-track track)
                    (nth 2 track))))
          (get-song-list pattern)))


(defun helm-itunes-search ()
  "Initiate the search"
  (itunes-seach-formatted helm-pattern))


(defun helm-itunes-play-track (track)
  (shell-command (format "osascript -e 'tell application %S to play track %S' " music-player track)))


;;;###autoload
(defvar helm-source-itunes-search
  '((name . "iTunes Search")
    (volatile)
    (delayed . 1)    ; Change the delay to prevent Emacs from crashing.
    (multiline)
    (requires-pattern . 2)
    (candidates-process . helm-itunes-search)
    (action . (("Play Track" . helm-itunes-play-track)))))


;;;###autoload
(defun helm-itunes ()
  "Bring up a Spotify search interface in helm."
  (interactive)
  (helm :sources '(helm-source-itunes-search)
        :buffer "*helm-itunes*"))


(provide 'helm-itunes)
;;; helm-itunes.el ends here

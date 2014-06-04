;;;  helm-itunes.el Play local Spotify Tracks
;; Copyright 2014 Adam Schwartz
;;
;; Author: Adam Schwartz <adam@adamschwartz.io>
;; URL: https://github.com/daschwa/helm-itunes
;; Created: 2014-06-02 19:32:16
;; Version: 0.0.1

;;; Commentary:
;;
;; A search & play interface for iTunes.
;;
;; There should be a simple way to get the songs to play
;; with Spotify instead of iTunes.
;;
;; Currently only supports OS X.
;;

;;; Code:

(require 'helm)

;; (defvar track-uri nil)
;; (setq "spotify:local:Beastie+Boys:Ill Communication:Sure+Shot:123")
;; (shell-command (format "osascript -e 'tell application %S to play track %S'" "Spotify" track-uri))

;; Either an artist, album, or song name to search for.
(defvar search-pattern nil)
(setq search-pattern "Sure Shot")


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
  (mapcar (lambda (song-list)
            (split-string song-list "\\,\s")) (split-string
                                               (shell-command-to-string
                                                (format "osascript -e %S" (search-script pattern)))
                                               "pattern-match-end,\s\\|pattern-match-end")))

(get-song-list search-pattern)


(defun itunes-format-track (track)
  "Given a track, return a formatted string to display"
  (let ((song (nth 2 track))
        (artist (nth 0 track))
        (album (nth 1 track)))
    (format "%S\n %S\n %S" song artist album)))


(itunes-format-track '("Artist" "Album" "Song"))

(defun spotify-format-track (track)
  "Return a Spotify compatible string to play"
  (let ((song (nth 2 track))
        (artist (nth 0 track))
        (album (nth 1 track)))
    (format "spotify:local:%s:%s:%s:123" artist album song)))

(spotify-format-track '("Artist" "Album" "Song"))

;; Helm Functions

(defun helm-itunes-search ()
  (mapcar (lambda (track)
            (cons (itunes-format-track track)
                  (if (equal music-player "spotify")
                      (spotify-format-track track)
                    (nth 2 track))))
          (get-song-list search-pattern)))


(defun helm-itunes-play-track (track)
  (shell-command (format "osascript -e 'tell application %S to play track %S'" music-player track)))


(defvar helm-source-itunes-track-search
  '((name . "iTunes Search")
    (volatile)
    (delayed)
    (multiline)
                                        ;(requires-pattern . 2)
    (candidates-process . helm-itunes-search)
    (action . (("Play Track" . helm-itunes-play-track)))))


(defun helm-itunes ()
  "Bring up a Spotify search interface in helm."
  (interactive)
  (helm :sources '(helm-source-itunes-track-search)
        :buffer "*helm-itunes*"))

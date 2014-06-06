set local_track to "spotify:local:Beastie+Boys:Ill Communication:Sure+Shot:123"

tell application "Spotify"
	play track local_track
end tell

-- local Spotify URI format: "spotify:local:Artist:Album:Song:123"

-- "123" looks like a song ID, but it seems to work with any non-null value, so I am just putting "123" for all songs.
-- toggle a variable if iTunes was running before this script was run.
if application "iTunes" is running then
	set irun to true
else
	set irun to false
end if

set pattern to "Sure Shot"

set matches to {}
tell application "iTunes"
	repeat with t in (file tracks whose artist contains pattern Â
		or album contains pattern or name contains pattern)
		set matches to matches & Â
			{{artist of t as string, album of t as string, Â
				(name of t as string) & "pattern-match-end"}}
	end repeat
	
end tell

-- If you never had iTunes open, then quit after performing the search, otherwise leave it open.
if not irun then
	tell application "iTunes" to quit
end if

-- Return the list of songs that matched the search
return matches
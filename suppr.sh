MUSIC_PATH=`mpc -f %file% current`
FULLPATH=/home/mpd/music/$MUSIC_PATH
mpc del 0
mpc play
rm -f "$FULLPATH"


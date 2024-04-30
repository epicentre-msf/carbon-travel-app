rsync -zavh ~/epicentre/meeting_place_planner/data episerv:/home/epicentre/carbon-travel-app/

docker run --rm \
-p 5858:3838 \
-v /home/epicentre/carbon-travel-app:/root/app epicentremsf/epishiny \
R -e "shiny::runApp('/root/app', port = 3838, host = '0.0.0.0')"

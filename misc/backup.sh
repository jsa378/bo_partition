#!/bin/zsh -e

commit_message="$1"

cd /Users/jesse/Downloads/bo_partition/
git add .
git commit -m "${commit_message}"
git push origin master

cd /Users/jesse/Downloads/

tar cJf $(date +%F).tar.xz meetings/
scp $(date +%F).tar.xz jsa378@alliancecan.ca:/home/jsa378/meetings_backups/
rm $(date +%F).tar.xz
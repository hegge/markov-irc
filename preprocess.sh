#!/bin/bash


log_directory="sample/"
channel="#dev"

ignore_users="<.gitbot|<.nonsense"
ignore_content=" -!- |^---"


cat ${log_directory}/*/${channel}.log |
grep -vE "$ignore_content" |
grep -vE "$ignore_users" |
grep -v "scripting" |
cut -f2- -d'>' | cut -f2- -d' ' >logfile

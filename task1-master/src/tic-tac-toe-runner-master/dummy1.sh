#!/bin/sh
read prev

if [ ${#prev} -gt 100 ]
then
    exit 4
else
    echo "{\"last\":{\"I\":{\"am\":\"json\"}},\"prev\":$prev}"
fi
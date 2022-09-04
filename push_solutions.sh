#!/bin/bash
if [ -z $APIKEY ]; then
    echo "Please set your APIKEY environment variable."
    exit 1
fi
for i in {1..35}; do
    if [ -d tasks/$i.png.solutions ]; then
        if [ -n "$(ls tasks/$i.png.solutions)" ]; then
            SMALLEST=$(cd tasks/$i.png.solutions/; ls * | sort -n | head -1)
            if [ ! -f tasks/$i.png.solutions/$SMALLEST.submitted ]; then
                echo "Submitting $i.png.solutions/$SMALLEST"
                curl -sS -H "Authorization: Bearer $APIKEY" -F file=@tasks/$i.png.solutions/$SMALLEST https://robovinci.xyz/api/submissions/$i/create | tee /dev/stderr | jq -e '.submission_id'
                if [ $? = 0 ]; then
                    touch tasks/$i.png.solutions/$SMALLEST.submitted
                    git add tasks/$i.png.solutions/$SMALLEST
                    git add tasks/$i.png.solutions/$SMALLEST.submitted
                fi
            fi
        fi
    fi
done

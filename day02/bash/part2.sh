#!/bin/bash

#set -x

function is_invalid() {
    size="${#1}"
    mid=$(($size / 2))
    for digit_len in $(seq 1 "$mid")
    do
	init="${1:0:$digit_len}"
        local i
        for i in $(seq "$digit_len" "$digit_len" "$size")
        do
            this_segment="${1:$i:$digit_len}"
            this_segment="${this_segment:-$init}"
            if [ "$init" != "$this_segment" ]
            then
                continue 2
            fi
        done
        return 0
    done
    return 1
}

input=$(cat)
sum=$((0))

for elem in $(echo $input | tr , "\n")
do
    range=($(echo $elem | tr - " "))
    start=$((${range[0]}))
    for i in $(seq $start ${range[1]})
    do
        is_invalid "$i"
        if [ "$?" -eq 0 ]
        then
            ((sum+="$i"))
        fi
    done
done

echo "$sum"

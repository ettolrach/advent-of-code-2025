#!/bin/bash

function is_doubled() {
    size="${#1}"
    if [ $(($size % 2)) -eq 1 ]
    then
	return 1
    fi
    mid=$(($size / 2))
    init="${1:0:$mid}"
    latter="${1:$mid:$size}"
    if [ $init = $latter ]
    then
	return 0
    else
	return 1
    fi
}

input=$(cat)
sum=$((0))

for elem in $(echo $input | tr , "\n")
do
    range=($(echo $elem | tr - " "))
    start=$((${range[0]}))
    for i in $(seq $start ${range[1]})
    do
	is_doubled $i
	if [ $? -eq 0 ]
	then
	    ((sum+=$(($i))))
	fi
    done
done

echo $sum

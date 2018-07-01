#!/usr/bin/env bash
draw(){

gnuplot << PLOT
set terminal svg size 1920,600 enhanced font 'Verdana,10'
unset key
set output "$1.svg"  
set object 1 rectangle from screen 0,0 to screen 1,1 fc rgb "white10" behind
set xlabel "X"
set ylabel "Y"
set size ratio -1
set grid
plot "$1","$1" w l
PLOT

}

main(){
    for each in slicedata*.dat
    do
        echo "drawing ${each}"
        draw "${each}"
    done
}

main

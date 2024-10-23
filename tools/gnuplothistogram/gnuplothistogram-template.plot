set output '@BASENAME@.@FORMAT@'
set terminal @FORMAT@ font "Verdana,12"

set title '@TITLE@'
set ylabel "@YLABEL@" offset 0,0

set auto x

set style data histogram
set style histogram gap 1 @ERRORBARS@
set errorbars lc rgb '#444444'
set xtics rotate by 45 right

set xtics font "Verdana,@XTICS@"
set ytics font "Verdana,@YTICS@"

set boxwidth 0.9
set style fill solid
set style line 1 linecolor rgb '#3264c8' linetype 1 linewidth 1
set style line 2 linecolor rgb '#d83812' linetype 1 linewidth 1
set style line 3 linecolor rgb '#fa9600' linetype 1 linewidth 1
set style line 4 linecolor rgb '#109318' linetype 1 linewidth 1
set style line 5 linecolor rgb '#93ade2' linetype 1 linewidth 1
set style line 6 linecolor rgb '#edd20b' linetype 1 linewidth 1
set style line 7 linecolor rgb '#00a0bf' linetype 1 linewidth 1
set style line 8 linecolor rgb '#72bf00' linetype 1 linewidth 1
set style line 9 linecolor rgb '#969996' linetype 1 linewidth 1
set style line 100 linecolor rgb '#000000' linetype 1 linewidth 1

set grid ytics
set xtics scale 0
set datafile separator ","

set yrange [0:*]

set lmargin 6
set rmargin 1
set key under nobox

set output '@BASENAME@.@FORMAT@'
set terminal @FORMAT@ font "Verdana,12" @SIZE@

set title '@TITLE@'
set ylabel "@YLABEL@" offset @YLABEL_OFFSET@ font "@YLABEL_FONT@"
set tmargin @TMARGIN

set auto x

set style data histogram
set style histogram gap 1 @ERRORBARS@
set errorbars lc rgb '#444444'
set xtics rotate by 45 right

set xtics font "Verdana,@XTICS@"
set ytics font "Verdana,@YTICS@"

set boxwidth 0.9
set style fill solid
set style line 1 linecolor rgb '@COLOR0@' linetype 1 linewidth 1
set style line 2 linecolor rgb '@COLOR1@' linetype 1 linewidth 1
set style line 3 linecolor rgb '@COLOR2@' linetype 1 linewidth 1
set style line 4 linecolor rgb '@COLOR3@' linetype 1 linewidth 1
set style line 5 linecolor rgb '@COLOR4@' linetype 1 linewidth 1
set style line 6 linecolor rgb '@COLOR5@' linetype 1 linewidth 1
set style line 7 linecolor rgb '@COLOR6@' linetype 1 linewidth 1
set style line 8 linecolor rgb '@COLOR7@' linetype 1 linewidth 1
set style line 9 linecolor rgb '@COLOR8@' linetype 1 linewidth 1
set style line 10 linecolor rgb '@COLOR9@' linetype 1 linewidth 1
set style line 100 linecolor rgb '#000000' linetype 1 linewidth 1
set style line 1000 linecolor rgb '#555555 linewidth 20

set grid ytics
set xtics scale 0
set datafile separator ","

set yrange @RANGE@

set lmargin @LMARGIN@
set rmargin @RMARGIN@
set bmargin @BMARGIN@

set key @KEY@

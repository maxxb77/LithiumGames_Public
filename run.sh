gams main.gms s=main
#exit 17

# solve various market setups and calibration routines
gams solve_fig1.gms r=main s=fig1
gdxdump fig1.gdx format=csv symb=rep > prod_ratios.csv
#exit 17

# using the last solve from that file, start tinkering with which variables to free up
gams solve_fig2.gms r=fig1 s=fig2

gdxdump fig2.gdx format=csv symb=rep2 > rep2.csv
gdxdump fig2.gdx format=csv symb=rep3 > rep3.csv
#exit 17

gams solve_fig3.gms r=fig1 s=fig3
gdxdump fig3.gdx format=csv symb=rep4 > rep4.csv

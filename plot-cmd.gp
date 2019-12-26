set yrange [:] reverse
set linestyle 1 lc "blue" lw 1.5
set linestyle 3 lc "dark-violet" lw 1.5
plot \
 "SALIDA.DAT" u ($2-$3):3 w l t "Primaria",\
  "" u ($7-$8):8 w l ls 1 t "Secundaria",\
  "" u ($12-$13):13 w l ls 3 t "Combinada"

# u-b vs b
# "SALIDA.DAT" u ($1-$2):2 w l t "Primaria",\
# "" u ($6-$7):7 w l ls 1 t "Secundaria",\
# "" u ($11-$12):12 w l ls 3 t "Combinada"

# b-v vs v
# "SALIDA.DAT" u ($2-$3):3 w l t "Primaria",\
# "" u ($7-$8):8 w l ls 1 t "Secundaria",\
# "" u ($12-$13):13 w l ls 3 t "Combinada"

# v-i vs i
# "SALIDA.DAT" u ($3-$5):5 w l t "Primaria",\
# "" u ($8-$10):10 w l ls 1 t "Secundaria",\
# "" u ($13-$15):15 w l ls 3 t "Combinada"

# r-i vs i
#"SALIDA.DAT" u ($4-$5):5 w l t "Primaria",\
# "" u ($9-$10):10 w l ls 1 t "Secundaria",\
# "" u ($14-$15):15 w l ls 3 t "Combinada"



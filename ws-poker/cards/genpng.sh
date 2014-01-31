for S in C D H S
do
    for C in A 2 3 4 5 6 7 8 9 10 J Q K
    do
        export N=${C}${S}
        echo "${N}..."
        convert +antialias -trim -background transparent -density 200 -resize 512 src/${N}.svg ${N}.png &
    done
    wait
done

for C in Blue Red
do
    echo "${C}_Back..."
    convert +antialias -trim -background transparent -density 200 -resize 512 src/${C}_Back.svg ${C}_Back.png &
    done
wait

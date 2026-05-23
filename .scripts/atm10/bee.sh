#/usr/bin/env sh

for bee_type in $@; do
    echo "
    else if ${bee_type}_gene_drawer has le 1024 productivebees:gene then
        output
            retain 64 bee_cage
        to each ${bee_type}_bee_breeding_chamber slot 0

        output
            retain 64 rose_bush
        to each ${bee_type}_bee_breeding_chamber slot 3

        output
            retain 64 rose_bush
        to each ${bee_type}_bee_breeding_chamber slot 4
    "
done

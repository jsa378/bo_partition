#!/bin/zsh

for i in {1..1}
do
    Rscript /Users/jesse/Downloads/bo_partition/implementation_testing/dice.R ${i} "rastr" 2 20 5 10 "/Users/jesse/Downloads/cedar_test_output/26jul24meeting/2dim_reg_dice/"
done

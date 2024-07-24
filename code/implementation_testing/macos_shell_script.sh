#!/bin/zsh

for i in {1..10}
do
    # echo "BEGINNING RUN ${i} OF REGULAR DICE"
    # mkdir /Users/jesse/Downloads/cedar_test_output/26jul24meeting/10runs/2dim_reg_dice/seed_${i}    
    # Rscript /Users/jesse/Downloads/bo_partition/code/implementation_testing/dice.R ${i} "rastr" 2 20 100 10 "/Users/jesse/Downloads/cedar_test_output/26jul24meeting/10runs/2dim_reg_dice/seed_${i}/"

    # echo "BEGINNING RUN ${i} OF PARTITION-BO V2 (MINOR CHANGES)"
    # mkdir /Users/jesse/Downloads/cedar_test_output/26jul24meeting/10runs/2dim_v2/seed_${i}    
    # Rscript /Users/jesse/Downloads/bo_partition/code/research/v2/bo_partition.R ${i} "rastr" 2 20 100 10 25 0.1 1000 10 "/Users/jesse/Downloads/cedar_test_output/26jul24meeting/10runs/2dim_v2/seed_${i}/" ${i}

    # echo "BEGINNING RUN ${i} OF PARTITION-BO V3 (MINOR CHANGES PLUS SWITCHING)"
    # mkdir /Users/jesse/Downloads/cedar_test_output/26jul24meeting/10runs/2dim_v3/seed_${i}    
    # Rscript /Users/jesse/Downloads/bo_partition/code/research/v3/bo_partition.R ${i} "rastr" 2 20 100 10 25 0.1 1000 10 "/Users/jesse/Downloads/cedar_test_output/26jul24meeting/10runs/2dim_v3/seed_${i}/" ${i}

    echo "BEGINNING RUN ${i} OF PARTITION-BO V4 (MINOR CHANGES PLUS SWITCHING, PLUS NO EARLY REJECTIONS)"
    mkdir /Users/jesse/Downloads/cedar_test_output/26jul24meeting/10runs/2dim_v4/seed_${i}    
    Rscript /Users/jesse/Downloads/bo_partition/code/research/v4/bo_partition.R ${i} "rastr" 2 20 100 10 25 0.1 1000 10 "/Users/jesse/Downloads/cedar_test_output/26jul24meeting/10runs/2dim_v4/seed_${i}/" ${i}

    echo "BEGINNING RUN ${i} OF PARTITION-BO V5 (LIKE V4 BUT WITH POINT SHARING)"
    mkdir /Users/jesse/Downloads/cedar_test_output/26jul24meeting/10runs/2dim_v5/seed_${i}    
    Rscript /Users/jesse/Downloads/bo_partition/code/research/v5/bo_partition.R ${i} "rastr" 2 20 100 10 25 0.1 1000 10 0 "/Users/jesse/Downloads/cedar_test_output/26jul24meeting/10runs/2dim_v5/seed_${i}/" ${i}
done

# conda activate 54805
# python /Users/jesse/Downloads/cedar_test_output/26jul24meeting/10runs/plots.py "rastr" 2 20 100 10 25 0.1 1000 10 "/Users/jesse/Downloads/cedar_test_output/26jul24meeting/10runs_crash/"
# conda deactivate
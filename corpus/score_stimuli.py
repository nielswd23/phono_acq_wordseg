import csv
from ngram_calculator import run

# Run gold tokenized models
run("gold_token_final.csv", "infant_2a_testing.csv", "scored_stimuli/gold_token_2a.csv")

run("gold_token_final.csv", "infant_2b_testing.csv", "scored_stimuli/gold_token_2b.csv")

run("gold_token_final.csv", "infant_3_testing.csv", "scored_stimuli/gold_token_3.csv")

# Run gold typized models
run("gold_type_final.csv", "infant_2a_testing.csv", "scored_stimuli/gold_type_2a.csv")

run("gold_type_final.csv", "infant_2b_testing.csv", "scored_stimuli/gold_type_2b.csv")

run("gold_type_final.csv", "infant_3_testing.csv", "scored_stimuli/gold_type_3.csv")

# Run unseg tokenized models
run("unseg_token_final.csv", "infant_2a_testing.csv", "scored_stimuli/unseg_token_2a.csv")

run("unseg_token_final.csv", "infant_2b_testing.csv", "scored_stimuli/unseg_token_2b.csv")

run("unseg_token_final.csv", "infant_3_testing.csv", "scored_stimuli/unseg_token_3.csv")

# Run unseg typized models
run("unseg_type_final.csv", "infant_2a_testing.csv", "scored_stimuli/unseg_type_2a.csv")

run("unseg_type_final.csv", "infant_2b_testing.csv", "scored_stimuli/unseg_type_2b.csv")

run("unseg_type_final.csv", "infant_3_testing.csv", "scored_stimuli/unseg_type_3.csv")
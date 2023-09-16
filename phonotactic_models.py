import ngram_calculator

path_test = "infant_stim.txt"
path_out = "out.txt" 

dir = "./Pearl_segmented_files/"
path_training_files = [dir+"formatted_seg_puddle.txt", 
                       dir+"segmented.baseline.txt",
                       dir+"segmented.dpseg.txt",
                       dir+"segmented.tp.txt"]

for file in path_training_files:
    ngram_calculator.run(file, path_test, path_out)
# run(path to training, path to test, path to out)
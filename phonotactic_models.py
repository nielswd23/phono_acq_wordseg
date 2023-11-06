import ngram_calculator

path_test = "infant_stim.txt"

dir_t = "./Formatted_Pearl_seg_files/segmented."
dir_o = "./Pearl_ngram_out/"
path_training_out_files = [[dir_t+"ag.txt", dir_o+"ag.txt"],
                       [dir_t+"puddle.txt", dir_o+"puddle.txt"], 
                       [dir_t+"baseline.txt", dir_o+"baseline.txt"],
                       [dir_t+"dpseg.txt", dir_o+"dpseg.txt"],
                       [dir_t+"tp.txt", dir_o+"tp.txt"],
                       [dir_t+"dibs.txt", dir_o+"dibs.txt"]]

for l in path_training_out_files:
    ngram_calculator.run(l[0], path_test, l[1])
# run(path to training, path to test, path to out)

# # additional run on unsegmented Pearl corpus and gold seg
ngram_calculator.run("./Formatted_Pearl_seg_files/unseg.txt", path_test, 
                     dir_o + "unseg.txt")

ngram_calculator.run("./Formatted_Pearl_seg_files/gold_seg.txt", path_test, 
                     dir_o + "gold_seg.txt")
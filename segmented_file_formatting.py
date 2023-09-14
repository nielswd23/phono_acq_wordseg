from prep_txt_wordseg import conv_str

with open(r"./PearlCorpus_test/segmented.puddle.txt") as file:
    seg_utt = file.readlines()

rev_seg_utt = [utt[:-1] for utt in seg_utt] # remove newline character

l_words = [" ".join(list(word)) for utt in rev_seg_utt 
           for word in utt.split(" ")]

# return to special char format
conv_dict = {"A":"^", "K": "@"}

final_form = [conv_str(word, conv_dict) for word in l_words]

# with open("formatted_seg_puddle.txt", "w") as f: 
#     for item in final_form:
#         f.write("%s\n" % item)

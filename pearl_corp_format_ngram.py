from prep_txt_wordseg import *

## formatting Pearl corpus to run the ngram calc on base utterances:
## unsegmented utterances and gold standard 

## using conv_pearl_utts from other script which standardizes Klattbet chars
# creating a list of the chars in utt that are not spaces and then joining 
# this with spaces to get the right format 
unseg_output = [" ".join([char for char in utt if char != " "]) 
        for utt in conv_pearl_utts]

unseg_types = list(set(unseg_output))


# # writing the revised corpus to a txt file 
# with open("./formatted_Pearl_seg_files/unseg.txt", "w") as f: 
#     for item in unseg_output:
#         f.write("%s\n" % item)

# with open("./formatted_Pearl_seg_files_types/unseg.txt", "w") as f: 
#     for item in unseg_types:
#         f.write("%s\n" % item)



### gold standard 
gold_output = [" ".join(list(word)) for utt in conv_pearl_utts 
               for word in utt.split()]

gold_types = list(set(gold_output))

# # writing the revised corpus to a txt file 
# with open("./formatted_Pearl_seg_files/gold_seg.txt", "w") as f: 
#     for item in gold_output:
#         f.write("%s\n" % item)

# with open("./formatted_Pearl_seg_files_types/gold_seg.txt", "w") as f: 
#     for item in gold_types:
#         f.write("%s\n" % item)
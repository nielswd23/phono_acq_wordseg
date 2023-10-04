from prep_txt_wordseg import *

## formatting Pearl corpus to run the ngram calc on base utterances

## using conv_pearl_utts from other script which standardizes Klattbet chars
# creating a list of the chars in utt that are not spaces and then joining 
# this with spaces to get the right format 
output = [" ".join([char for char in utt if char != " "]) 
        for utt in conv_pearl_utts]


# writing the revised corpus to a txt file 
with open("./formatted_Pearl_seg_files/unseg.txt", "w") as f: 
    for item in output:
        f.write("%s\n" % item)
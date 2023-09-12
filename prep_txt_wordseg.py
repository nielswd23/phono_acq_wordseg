# preparing Pearl Corpus for the wordseg format

with open(r"PearlCorpusPhonetic.txt") as file:
    Pearl_l_of_utt = file.readlines()

def add_eword(utt): 
    utt_w_spaces = " ".join(utt[:-1]) # index to remove newline character \n
    rev_utt = " ;eword ".join(utt_w_spaces.split("   ")) + " ;eword"
    return rev_utt

# def add_eword(l_utt): 
#     rev_l_utt = [" ;eword ".join(utt.split(" ")) for utt in l_utt]
#     return rev_l_utt

rev_Pearl = [add_eword(utt) for utt in Pearl_l_of_utt]
# save as file to feed wordseg model
# remove txt file and add this file to github

# preparing Pearl Corpus for the wordseg format
with open(r"PearlCorpusPhonetic.txt") as file:
    Pearl_l_of_utt = file.readlines()

# want to get rid of some of the special chars 
# and standardize klattbet system 
conv_dict = {"^":"x", "@": "A", "j":"J", "R":"X", "a":"c"}

def conv_str(string, conv_d):
    new_string = ""
    for char in string:
        if char not in conv_d:
            new_string += char
        else:
            new_string += conv_d[char]
    return new_string

conv_pearl_utts = [conv_str(utt[:-1], conv_dict) for utt in Pearl_l_of_utt] # [:-1] to remove newline character \n


def add_eword(utt): 
    utt_w_spaces = " ".join(utt) 
    rev_utt = " ;eword ".join(utt_w_spaces.split("   ")) + " ;eword"
    return rev_utt

rev_Pearl = [add_eword(utt) for utt in conv_pearl_utts]

symbols = []
for utt in conv_pearl_utts:
    for char in utt:
        if char not in symbols:
            symbols.append(char)

# # writing the revised corpus to a txt file 
# with open("prepared_Pearl.txt", "w") as f: 
#     for item in rev_Pearl:
#         f.write("%s\n" % item)
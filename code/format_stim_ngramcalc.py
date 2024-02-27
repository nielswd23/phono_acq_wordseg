with open(r"../connor/stim_txt_Klattbet/stim2a.txt") as file:
    stim_2a = file.readlines()

with open(r"../connor/stim_txt_Klattbet/stim2b.txt") as file:
    stim_2b = file.readlines()

with open(r"../connor/stim_txt_Klattbet/stim3.txt") as file:
    stim_3 = file.readlines()

def ngram_format(word):
    return " ".join(list(word)[:-1]) # adding spaces between each character. also lopping off the last special newline character

format_stim_2a = [ngram_format(w) for w in stim_2a]
format_stim_2b = [ngram_format(w) for w in stim_2b]
format_stim_3 = [ngram_format(w) for w in stim_3]

# writing the formatted stimuli lists to txt files
with open("../connor/stim_txt_Klattbet/ngram_format_stim2a.txt", "w") as f: 
    for item in format_stim_2a:
        f.write("%s\n" % item)

with open("../connor/stim_txt_Klattbet/ngram_format_stim2b.txt", "w") as f: 
    for item in format_stim_2b:
        f.write("%s\n" % item)

with open("../connor/stim_txt_Klattbet/ngram_format_stim3.txt", "w") as f: 
    for item in format_stim_3:
        f.write("%s\n" % item)
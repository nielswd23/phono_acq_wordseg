from prep_txt_wordseg import conv_str, symbols

# now formatting for stimuli
with open(r"./infant_stim_klattbet/infant_2b.txt") as file:
    infant_stim_2b = file.readlines()

with open(r"./infant_stim_klattbet/infant_2a.txt") as file:
    infant_stim_2a = file.readlines()

with open(r"./infant_stim_klattbet/infant_3.txt") as file:
    infant_stim_3 = file.readlines()

infant_stim = infant_stim_2a + infant_stim_2b + infant_stim_3

infant_conv_dict = {"@":"A"}
conv_infant_stim = [conv_str(" ".join(list(word[:-1])), infant_conv_dict) 
                    for word in infant_stim]

stim_symbols = []
for word in conv_infant_stim:
    for char in word:
        if char not in stim_symbols:
            stim_symbols.append(char)

# # checking the difference between Corpus and stimuli symbols
# print(set(stim_symbols) - set(symbols))
# print(set(symbols) - set(stim_symbols))

# Writing the revised stimuli to a txt file
with open("infant_stim.txt", "w") as f: 
    for item in conv_infant_stim:
        f.write("%s\n" % item)
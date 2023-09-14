from prep_txt_wordseg import conv_str

with open(r"./infant_stim_klattbet/infant_2a.txt") as file:
    infant_stim_2a = file.readlines() 

conv_dict = {"X":"R"}
final = [conv_str(" ".join(list(word[:-1])), conv_dict) 
         for word in infant_stim_2a]

# with open("stim.txt", "w") as f: 
#     for item in final:
#         f.write("%s\n" % item)


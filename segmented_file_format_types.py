import os

# mkdir for output files
os.mkdir("./formatted_Pearl_seg_files_types/")

# Folder Path
path = "./Pearl_segmented_files/"
  
# Change the directory
os.chdir(path)

# main function
def format_seg_file(file_name):
    out_file = "../formatted_Pearl_seg_files_types/"+file_name
    with open(file_name, 'r') as file:
        seg_utt = file.readlines()

    rev_seg_utt = [utt[:-1] for utt in seg_utt] # remove newline character
    
    l_words_types = []
    for utt in rev_seg_utt:
        for word in utt.split(" "):
            format_word = " ".join(list(word)) # accommodating the format restrictions of ngram calculator. each sound separated by a space 
            if format_word not in l_words_types and format_word != '': # only adding unique words. types list. removing empty words because baseline had some strange spacing and produced empty words
                l_words_types.append(format_word)
    
    with open(out_file, "w") as f: 
        for item in l_words_types:
            f.write("%s\n" % item)

    return l_words_types

# iterate through files
for f in os.listdir():
    format_seg_file(f)


### just manually add gold and unseg by taking formatted version 
### and doing list(set(l)). Kind of a lot to do a seperate types extraction









# def format_seg_file1(file_name):
#     out_file = "../formatted_Pearl_seg_files/"+file_name
#     with open(file_name, 'r') as file:
#         seg_utt = file.readlines()

#     rev_seg_utt = [utt[:-1] for utt in seg_utt] # remove newline character

#     l_words = [" ".join(list(word)) for utt in rev_seg_utt 
#             for word in utt.split(" ")]
    
#     # removing empty words. baseline had some strange spacing and produced empty words
#     if '' in l_words:
#         l_words = [word for word in l_words if word != '']
    
#     # with open(out_file, "w") as f: 
#     #     for item in l_words:
#     #         f.write("%s\n" % item)

#     return l_words

# # # iterate through files
# # for f in os.listdir():
# #     format_seg_file(f)

# l_tokens = format_seg_file1("segmented.baseline.txt")

# test = list(set(l_tokens))

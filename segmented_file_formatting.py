import os

# mkdir for output files
os.mkdir("./formatted_Pearl_seg_files/")

# Folder Path
path = "./Pearl_segmented_files/"
  
# Change the directory
os.chdir(path)

# main function
def format_seg_file(file_name):
    out_file = "../formatted_Pearl_seg_files/"+file_name
    with open(file_name, 'r') as file:
        seg_utt = file.readlines()

    rev_seg_utt = [utt[:-1] for utt in seg_utt] # remove newline character

    l_words = [" ".join(list(word)) for utt in rev_seg_utt 
            for word in utt.split(" ")]
    
    # removing empty words. baseline had some strange spacing and produced empty words
    if '' in l_words:
        l_words = [word for word in l_words if word != '']
    
    with open(out_file, "w") as f: 
        for item in l_words:
            f.write("%s\n" % item)

# iterate through files
for f in os.listdir():
    format_seg_file(f)
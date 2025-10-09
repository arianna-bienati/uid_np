# -*- coding: utf-8 -*-
"""
Created on Fri Nov  8 15:36:55 2024

@author: isabell

split corpus file into separate files, one per text
consider only rsta and rstb texts

"""

import os

# function to split the file by text
def split_corpus_file(input_file, output_folder):
    # create output folder if it doesn't exist
    if not os.path.exists(output_folder):
        os.makedirs(output_folder)
    
    with open(input_file, 'r', encoding='utf-8') as file:
        content = file.read()
        
    # split content by <text> tags
    texts = content.split("<text>")
    
    for text in texts[1:]: # skip first split since it is before first <text> tag
        # get ID attribute and its value
        id_start = text.find('<text_id ') + 9
        id_end = text.find('>', id_start)
        text_id = text[id_start:id_end]
        
        # take only rsta and rstb texts
        if any(x in text_id for x in ["rsta", "rstb"]):
            
            # check if we already have this file
            if not os.path.exists(os.path.join(output_folder, text_id, ".vrt")):
        
                # get content within <text> tag
                text_content = "<text>\n" + text.strip() # add tag back to output
        
                # create new file with ID as file name
                output_file = os.path.join(output_folder, f"{text_id}.vrt")
                with open(output_file, 'w', encoding='utf-8') as output:
                    output.write(text_content)
            
                print(f"Created file: {output_file}")
        
        
# path to input file
input_file = 'C:/Users/isabell/Documents/UdS/Corpus_Analysis/RSC/data/rsc_dep_gs_603_202412.vrt/rsc_dep_gs_603_202412.vrt'

# path to output folder
output_folder = 'C:/Users/isabell/Documents/UdS/Corpus_Analysis/RSC/data/rsc_dep_gs_603_202412.vrt/files'

# split corpus file
split_corpus_file(input_file, output_folder)
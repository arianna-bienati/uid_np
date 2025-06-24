# -*- coding: utf-8 -*-
"""
Created on Fri Nov  8 16:45:23 2024

@author: isabell

script to get relevant NPs from corpus files and write info to csv
consider only object NPs that are compounds
take syntactic and surprisal information
extract metadata: text ID, author, year, journal, primary topic


"""

import os
import re
import csv


# regex for special characters, numbers
special = r'(\W+\t)'


# function to extract compound NPs with annotation and metadata from corpus file
def extract_compound_NPs(file_content):
    # initialize variables
    NP_data = []
    lines = file_content.splitlines()
    
    # initialize variables for metadata
    text_author = None
    text_year = None
    text_jrnl = None
    text_primaryTopic = None
    
    for i in range(len(lines)):
        # initalize variables for NP info
        compound_const_1 = None
        const_1_srp = None
        compound_const_2 = None
        const_2_srp = None
        compound_head = None
        head_deprel = None
        head_srp = None
        
        # extract metadata
        if lines[i].startswith('<text_id '): # text ID
            text_id = re.search(r'<text_id\s(.*?)>', lines[i]).group(1)
        elif lines[i].startswith('<text_author '): # author
            text_author = re.search(r'<text_author\s(.*)>', lines[i]).group(1)
        elif lines[i].startswith('<text_year '): # author
            text_year = re.search(r'<text_year\s(.*?)>', lines[i]).group(1)
        elif lines[i].startswith('<text_jrnl '): # author
            text_jrnl = re.search(r'<text_jrnl\s(.*?)>', lines[i]).group(1)
        elif lines[i].startswith('<text_primaryTopic '): # author
            text_primaryTopic = re.search(r'<text_primaryTopic\s(.*?)>', lines[i]).group(1)
            
        # check if current line is a word of a sentence (and not annotation)
        if len(lines[i].strip()) > 0 and not lines[i].startswith('<') and not re.search(special, lines[i]):
            columns = lines[i].split('\t') # get columns (word with annotation)
            # check if word is compound constituent
            if columns[6] == 'compound':
                # check if second line (i.e. next word) exists
                if len(lines[i+1].strip()) and not lines[i+1].startswith('<'):
                    next_columns = lines[i+1].split('\t')
                    # check if second word is compound constituent
                    if next_columns[6] == 'compound':
                        # check if third line exists
                        if len(lines[i+2].strip()) and not lines[i+2].startswith('<'):
                            next_columns_2 = lines[i+2].split('\t')
                            # check if third word is head noun and object
                            if next_columns_2[2] == 'NOUN' and next_columns_2[6] == 'obj':
                                compound_const_1 = columns[0]
                                const_1_srp = columns[8]
                                compound_const_2 = next_columns[0]
                                const_2_srp = next_columns[8]
                                compound_head = next_columns_2[0]
                                head_deprel = next_columns_2[6]
                                head_srp = next_columns_2[8]
                                NP_data.append({'text_id': text_id,
                                                'author': text_author,
                                                'year': text_year,
                                                'journal': text_jrnl,
                                                'topic': text_primaryTopic,
                                                'const1': compound_const_1,
                                                'const1_srp': const_1_srp,
                                                'const2': compound_const_2,
                                                'const2_srp': const_2_srp,
                                                'head': compound_head,
                                                'head_deprel': head_deprel,
                                                'head_srp': head_srp})
                                i += 2 # jump to next word after compound
                    # check if second word is head noun
                    elif next_columns[2] == 'NOUN' and next_columns[6] == 'obj':
                        compound_const_1 = columns[0]
                        const_1_srp = columns[8]
                        compound_head = next_columns[0]
                        head_deprel = next_columns[6]
                        head_srp = next_columns[8]
                        NP_data.append({'text_id': text_id,
                                        'author': text_author,
                                        'year': text_year,
                                        'journal': text_jrnl,
                                        'topic': text_primaryTopic,
                                        'const1': compound_const_1,
                                        'const1_srp': const_1_srp,
                                        'const2': compound_const_2,
                                        'const2_srp': const_2_srp,
                                        'head': compound_head,
                                        'head_deprel': head_deprel,
                                        'head_srp': head_srp})
                        i += 3 # jump to next word after compound
                       
    return NP_data


# function to add NP data to csv file
def save_to_csv(data, output_file):   
    # open output file
    with open(output_file, 'a', newline = '', encoding = 'utf-8') as csv_file:
        # define csv header
        header = ['text_id', 'author', 'year', 'journal', 'topic',
                  'const1', 'const1_srp', 'const2', 'const2_srp',
                  'head', 'head_deprel', 'head_srp']
        writer = csv.DictWriter(csv_file, fieldnames = header)
        
        # add header if output file is empty
        if os.path.getsize(output_file) == 0:
            writer.writeheader()
        
        # write NP data to file only if there is a compound
        for row in data:
            if row.get('head'):
                writer.writerow(row)
        
    
# function to process corpus files
def process_corpus_files(data_folder, output_file):    
    # go through each file in corpus data folder
    for file in os.listdir(data_folder):
        
        # get path of corpus file
        file_path = os.path.join(data_folder, file)
        
        # open and read corpus file
        corpus_file = open(file_path, 'r', encoding = 'utf-8').read()
        
        # get NP data from corpus file
        NP_data = extract_compound_NPs(corpus_file)
        
        # add NP data to output csv file
        save_to_csv(NP_data, output_file)
        
        print(f'Processing file {file}...')
        
        
# main function
if __name__ == "__main__":
   
    # data folder
    data_folder = 'C:/Users/isabell/Documents/UdS/Corpus_Analysis/RSC/LMM/analysis_20241018/data/rsc_v604_udpipe_srp_202410'
    #data_folder = 'C:/Users/isabell/Documents/UdS/Corpus_Analysis/RSC/LMM/analysis_20241018/data/test'
    
    # output file
    output_file = 'C:/Users/isabell/Documents/UdS/Corpus_Analysis/RSC/LMM/analysis_20241018/data/NP_data.csv'
    #output_file = 'C:/Users/isabell/Documents/UdS/Corpus_Analysis/RSC/LMM/analysis_20241018/preprocess/test.csv'
    
    # process corpus files
    process_corpus_files(data_folder, output_file)
        
                                
        
            
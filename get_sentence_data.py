# -*- coding: utf-8 -*-
"""
Created on Thu Sep 18 14:01:15 2025

@author: isabell

script to get relevant sentences from corpus files and write info to csv
- extract sentences from each document
- calculate Information Fluctuation Complexity based on surprisal annotation
- extract metadata: text ID, author, year, journal, primary topic
- version which includes the sentence content in the output file

"""

import os
import re
import csv
import sys

import numpy as np


# function to extract sentences from corpus file
def parse_sentences(file_path):
    
    # initialize variables for metadata
    text_id = None
    text_author = None
    text_year = None
    text_jrnl = None
    
    current_sentence = [] # current sentence: list of tokens
    in_sentence = False
    
    sents_in_file = [] # list for all sentences found in current file
    

    with open(file_path, 'r', encoding='utf-8') as f:
        for line in f:
            line = line.strip()
            
            # extract metadata
            if line.startswith('<text_id '): # text ID
                text_id = re.search(r'<text_id\s(.*?)>', line).group(1)
            elif line.startswith('<text_author '): # author
                text_author = re.search(r'<text_author\s(.*)>', line).group(1)
            elif line.startswith('<text_year '): # author
                text_year = re.search(r'<text_year\s(.*?)>', line).group(1)
            elif line.startswith('<text_jrnl '): # author
                text_jrnl = re.search(r'<text_jrnl\s(.*?)>', line).group(1)
            elif line.startswith('<s_sid '): # sentence ID
                sent_id = re.search(r'<s_sid\s(.*?)>', line).group(1)
            
            if re.match(r'<s_s10local\b.*>', line): # sentence starts
                in_sentence = True
                current_sentence = [] # initialize list for current sentence
                
            elif line == '</s_s10local>': # sentence ends
                in_sentence = False
                if current_sentence:
 
                    sent = [] # initialize list for current sentence
                     
                    # go through all the tokens in the current sentence
                    for idx, word in enumerate(current_sentence):

                        # get sentence tokens and following attributes:
                        # word, s50
                        token = [current_sentence[idx][0],  # word
                                     current_sentence[idx][-4]] # s50
                        sent.append(token) # add to list of current sentence
                    #print(sent)
                            
                    if sent:
                        # get surprisal values of all tokens
                        srp_values = [float(tok[-1]) for tok in sent]
                        avg_srp = sum(srp_values) / len(srp_values)
                        sum_srp = sum(srp_values)

                        if len(srp_values) < 3:
                            uid_dev = np.nan
                            sigma_gamma = np.nan
                        else:
                            diffs = np.diff(srp_values)
    
                            # this implementation matches conceptually line 369-378 of postprocess_eval_results.py in https://github.com/thomashikaru/word-order-uid/tree/tacl-share/evaluation
                            # this implementation matches conceptually also the function in revisiting-uid.ipynb at https://github.com/rycolab/revisiting-uid/tree/main/src
                            # and should be faithful to Collins' (2014) UIDev proposal
                            uid_dev = np.mean(np.abs(diffs))

                            # this implementation should be faithful to information fluctuation complexity applied to texts, as it appeared in Brasolin, Bienati (2025)
                            sigma_gamma = np.sqrt(np.mean((diffs - np.mean(diffs))**2))
                            
                
                        # add NP data to list of all NPs in file
                        sents_in_file.append({
                            "text_id": text_id,
                            "author": text_author,
                            "year": text_year,
                            "journal": text_jrnl,
                            "sent_id": sent_id,
                            #"sentence": sent, 
                            "sent_len": len(sent),
                            "sent_str": ' '.join(token[0] for token in sent),
                            "avg_srp": avg_srp,
                            "sum_srp": sum_srp,
                            "uid_dev": uid_dev,
                            "sigma_gamma": sigma_gamma
                            })
         
            # while in the sentence
            elif in_sentence: 
                if line:  # skip empty lines
                    token = line.split()
                    current_sentence.append(token) # add tokens to current sentence

    return sents_in_file


# function to add sentence data to csv file
def save_to_csv(sents_in_file, output_file):   
    # open output file
    with open(output_file, 'a', newline = '', encoding = 'utf-8') as csv_file:
        # define csv header
        header = ['text_id', 'author', 'year', 'journal', 
                  'sent_id', 'sent_len', 'sent_str',
                  'avg_srp', 'sum_srp', 'uid_dev', 'sigma_gamma']
        writer = csv.DictWriter(csv_file, fieldnames = header)
        
        # add header if output file is empty
        if os.path.getsize(output_file) == 0:
            writer.writeheader()
        
        # write sentence data to file
        for row in sents_in_file:
            if row['sent_len']: # only if there is sentence data
                writer.writerow(row)
        
    
# function to process corpus files
def process_corpus_files(data_folder, output_file):    
    # go through each file in corpus data folder
    for file in os.listdir(data_folder):
        
        # only consider .vrt files
        if file.endswith('.vrt'):
        
            print(f'Processing file {file}...')
            
            # get path of corpus file
            file_path = os.path.join(data_folder, file)
            
            # open corpus file, extract sentences
            sents_in_file = parse_sentences(file_path)
            
            # add NP data to output csv file
            save_to_csv(sents_in_file, output_file)
            print(f'Added sentences to output file: {output_file}')
        
        
        
# main function
if __name__ == "__main__":
   

    #data_folder = 'C:/Users/isabell/Documents/UdS/Corpus_Analysis/RSC/fluctuation_complexity/test'
    #data_folder = 'C:/Users/isabell/Documents/UdS/Corpus_Analysis/RSC/data/rsc_dep_gs_603_202412.vrt/files'
    data_folder = sys.argv[1]

    #output_file = 'C:/Users/isabell/Documents/UdS/Corpus_Analysis/RSC/fluctuation_complexity/test/test_sentence_data.csv'
    #output_file = 'C:/Users/isabell/Documents/UdS/Corpus_Analysis/RSC/fluctuation_complexity/data/sentence_data.csv'
    output_file = sys.argv[2]

    # process corpus files
    process_corpus_files(data_folder, output_file)
        
                                
        
            
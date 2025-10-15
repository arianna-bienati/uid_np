# -*- coding: utf-8 -*-
"""
Created on Fri Nov  8 16:45:23 2024

@author: isabell

script to get relevant NPs from corpus files and write info to csv
- consider only NPs tagged as 'NOUN'
- consider only subjects and direct objects
- extract entire NP: head with all dependents
- extract relevant annotation for each token in NP (word, lemma, upos, head/parent, urel, s50local)
- calculate Information Fluctuation Complexity based on surprisal annotation
- extract metadata: text ID, author, year, journal, primary topic

"""

import os
import re
import csv
import sys

from collections import deque
import numpy as np


# function to extract sentences from corpus file
def parse_sentences(file_path):
    
    # initialize variables for metadata
    text_id = None
    text_author = None
    text_year = None
    text_jrnl = None
    
    sentences = [] # list of sentences
    current_sentence = [] # current sentence: list of tokens
    in_sentence = False
    
    NPs_in_file = [] # list for all NPs found in current file
    

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
            
            if re.match(r'<s_s10local\b.*>', line): # sentence starts
                in_sentence = True
                current_sentence = [] # initialize list for current sentence
                
            elif line == '</s_s10local>': # sentence ends
                in_sentence = False
                if current_sentence:
                    sentences.append(current_sentence) # add current sentence
                    
                    children = {} # dictionary for heads with children
                    # create dependency graph of heads and their children
                    for idx, word in enumerate(current_sentence, start=1):
                        head = int(word[5]) # get the heads
                        children.setdefault(head, []).append(idx) # save children
 
                    # go through all the tokens in the current sentence
                    for idx, word in enumerate(current_sentence, start=1):
                        # if you encounter a noun which is (passive) subject or direct object
                        if word[2] == 'NOUN' and (word[6] == 'nsubj' or word[6] == 'nsubj:pass' or word[6] == 'obj'):
                            
                            NP = [] # initialize list for current NP
                           
                            visited = set([idx]) # tokens that have been visited
                            queue = deque([idx]) # create double-ended queue

                            # go through queue
                            while queue:
                                current = queue.popleft() # take left element in queue
                                
                                for child in children.get(current, []):
                                    if child not in visited:
                                        visited.add(child)
                                        queue.append(child)
                                        
                            sorted_indices = sorted(visited)

                            # get NP tokens and following attributes:
                            # word, lemma, upos, head/parent, urel, s50
                            NP = [[current_sentence[i-1][0], # word
                                   current_sentence[i-1][1], # lemma
                                   current_sentence[i-1][2], # upos
                                   current_sentence[i-1][5], # parent
                                   current_sentence[i-1][6], # urel
                                   current_sentence[i-1][-4]] # s50
                                  for i in sorted_indices]
                            
                            head_synt_role = word[6]
                            head_lemma = word[1]
                            
                            if NP:
                                #for tok in NP:
                                    #print(tok)
                                # get surprisal values of all tokens
                                srp_values = [float(tok[-1]) for tok in NP]
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
                                
                                # add current NP to list of NPs for current sentence
                    
                                # add NP data to list of all NPs in file
                                NPs_in_file.append({
                                    "text_id": text_id,
                                    "author": text_author,
                                    "year": text_year,
                                    "journal": text_jrnl,
                                    "NP": NP, 
                                    "NP_len": len(NP),
                                    "NP_str": ' '.join(token[0] for token in NP),
                                    "NP_pos": '_'.join(token[2] for token in NP),
                                    "head_lemma": head_lemma,
                                    "head_synt_role": head_synt_role,
                                    "avg_srp": avg_srp,
                                    "sum_srp": sum_srp,
                                    "uid_dev": uid_dev,
                                    "sigma_gamma": sigma_gamma
                                    })
                    #print(f"[{text_id}] -> {NPs}")
             
            # while in the sentence
            elif in_sentence: 
                if line:  # skip empty lines
                    token = line.split()
                    current_sentence.append(token) # add tokens to current sentence

    return NPs_in_file


# function to add NP data to csv file
def save_to_csv(NPs_in_file, output_file):   
    # open output file
    with open(output_file, 'a', newline = '', encoding = 'utf-8') as csv_file:
        # define csv header
        header = ['text_id', 'author', 'year', 'journal', 
                  'NP', 'NP_len', 'NP_str', 'NP_pos', 'head_lemma', 'head_synt_role',
                  'avg_srp', 'sum_srp', 'uid_dev', 'sigma_gamma']
        writer = csv.DictWriter(csv_file, fieldnames = header)
        
        # add header if output file is empty
        if os.path.getsize(output_file) == 0:
            writer.writeheader()
        
        # write NP data to file
        for row in NPs_in_file:
            if row['NP']: # only if there is NP data
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
            
            # open corpus file, extract sentences and NPs
            NPs_in_file = parse_sentences(file_path)
            
            # add NP data to output csv file
            save_to_csv(NPs_in_file, output_file)
            print(f'Added NPs to output file: {output_file}')
        
        
        
# main function
if __name__ == "__main__":
   
    # data folder
    # data_folder = 'C:/Users/isabell/Documents/UdS/Corpus_Analysis/RSC/LMM/analysis_20241018/data/rsc_v604_udpipe_srp_202410'
    #data_folder = sys.argv[1]
    # data_folder = 'C:/Users/isabell/Documents/UdS/Corpus_Analysis/RSC/fluctuation_complexity/test'
    data_folder = 'C:/Users/isabell/Documents/UdS/Corpus_Analysis/RSC/data/rsc_dep_gs_603_202412.vrt/files'
    
    # output file
    # output_file = 'C:/Users/isabell/Documents/UdS/Corpus_Analysis/RSC/LMM/analysis_20241018/data/NP_data.csv'
    #output_file = sys.argv[2]
    # output_file = 'C:/Users/isabell/Documents/UdS/Corpus_Analysis/RSC/fluctuation_complexity/test/test_v3.csv'
    output_file = 'C:/Users/isabell/Documents/UdS/Corpus_Analysis/RSC/fluctuation_complexity/data/NP_data_v1.csv'
    
    # process corpus files
    process_corpus_files(data_folder, output_file)
        
                                
        
            
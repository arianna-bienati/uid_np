# -*- coding: utf-8 -*-
"""
Created on Fri Jun  20 12:48 2025

@author: sonnet4 + ari (adapted from isabell)

Refactored script to extract all NPs from corpus files and write info to csv
Takes syntactic and surprisal information for all NPs (not just compounds)
Extracts metadata: text ID, author, year, journal, primary topic
Computes surprisal-based metrics for each NP
"""

import re
from collections import defaultdict

from np_class import NounPhrase
from utils import extract_metadata

# regex for special characters, numbers
special = r'(\W+\t)'

def extract_all_NPs(file_content):
    """Extract all NPs with annotation and metadata from corpus file."""
    lines = file_content.splitlines()
    metadata = extract_metadata(lines)
    text_id = metadata.get('text_id', 'unknown')
    
    NP_data = []
    sentence_tokens = []  # Store tokens for current sentence
    token_counter = 1  # Generate token IDs since they're missing
    
    for i, line in enumerate(lines):
        # Skip metadata lines and empty lines
        if line.startswith('<') or len(line.strip()) == 0 or re.search(special, line):
            # If we hit sentence boundary and have tokens, process NPs
            if sentence_tokens and (line.strip() == '' or line.startswith('<')):
                nps = identify_NPs_in_sentence(sentence_tokens, text_id, metadata)
                NP_data.extend(nps)
                sentence_tokens = []
                token_counter = 1  # Reset counter for new sentence
            continue
        
        # Parse token line
        columns = line.split('\t')
        if len(columns) >= 9:  # Ensure we have all required columns
            token_info = {
                'id': str(token_counter),  # Generate sequential ID
                'word': columns[0],
                'lemma': columns[1],
                'pos': columns[2],
                'head_id': columns[5],
                'deprel': columns[6],
                'surprisal': columns[8]
            }
            sentence_tokens.append(token_info)
            token_counter += 1
    
    # Process last sentence if exists
    if sentence_tokens:
        nps = identify_NPs_in_sentence(sentence_tokens, text_id, metadata)
        NP_data.extend(nps)
    
    return NP_data

def identify_NPs_in_sentence(tokens, text_id, metadata):
    """Identify noun phrases in a sentence based on dependency relations."""
    nps = []
    
    # Group dependencies to find NP structures
    heads_to_dependents = defaultdict(list)
    token_by_id = {}
    
    for token in tokens:
        token_by_id[token['id']] = token
        if token['head_id'] != '0':  # Not root
            heads_to_dependents[token['head_id']].append(token)
    
    # Find potential NP heads (nouns that are objects or subjects)
    for token in tokens:
        if (token['pos'] == 'NOUN' and
            token['head_id'] in [t['id'] for t in tokens if t['deprel'] == 'root'] and
            token['deprel'] not in ['obl', 'vocative', 'expl', 'dislocated']):
            
            # Create NP with this head
            np = NounPhrase(text_id, metadata)
            
            # Add the head token
            np.add_token(
                token['word'], token['lemma'], token['pos'], token['deprel'],
                token['head_id'], token['surprisal'], token['id']
            )
            
            # Add dependents that are part of the NP
            if token['id'] in heads_to_dependents:
                for dependent in heads_to_dependents[token['id']]:
                    if dependent['deprel'] in ['det', 'amod', 'compound', 'nmod', 'nummod', 'acl:relcl', 'acl']:
                        np.add_token(
                            dependent['word'], dependent['lemma'], dependent['pos'], dependent['deprel'],
                            dependent['head_id'], dependent['surprisal'], dependent['id']
                        )
            
            # Sort tokens by their position in sentence
            np.tokens.sort(key=lambda x: int(x['token_id']))
            
            if np.is_valid_np() and len(np.tokens) >= 1:  # At least head token
                nps.append(np)
    
    return nps
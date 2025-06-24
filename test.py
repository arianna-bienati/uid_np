from collections import defaultdict
from np_class import NounPhrase

def identify_NPs_in_sentence(tokens, text_id, metadata):
    """Identify noun phrases in a sentence based on dependency relations, including relative clauses."""
    nps = []
    
    # Group dependencies to find NP structures
    heads_to_dependents = defaultdict(list)
    token_by_id = {}
    
    for token in tokens:
        token_by_id[token['id']] = token
        if token['head_id'] != '0':  # Not root
            heads_to_dependents[token['head_id']].append(token)
    
    def collect_relative_clause_tokens(rel_clause_head, collected_tokens=None):
        """Recursively collect all tokens in a relative clause."""
        if collected_tokens is None:
            collected_tokens = set()
        
        # Add the current token
        collected_tokens.add(rel_clause_head['id'])
        
        # Recursively add all dependents of this token
        if rel_clause_head['id'] in heads_to_dependents:
            for dependent in heads_to_dependents[rel_clause_head['id']]:
                if dependent['id'] not in collected_tokens:  # Avoid cycles
                    collect_relative_clause_tokens(dependent, collected_tokens)
        
        return collected_tokens
    
    # Find potential NP heads (nouns that are core dependents of root)
    for token in tokens:
        if (token['pos'] == 'NOUN' and 
            token['head_id'] in [t['id'] for t in tokens if t['deprel'] == 'root'] and
            token['deprel'] not in ['obl', 'vocative', 'expl', 'dislocated']):
            
            # Create NP with this head
            np = NounPhrase(text_id, metadata)
            
            # Add the head token
            np.add_token(
                token['word'], token['pos'], token['deprel'],
                token['head_id'], token['surprisal'], token['id']
            )
            
            # Add dependents that are part of the NP
            if token['id'] in heads_to_dependents:
                for dependent in heads_to_dependents[token['id']]:
                    # Standard NP dependents
                    if dependent['deprel'] in ['det', 'amod', 'compound', 'nmod', 'nummod']:
                        np.add_token(
                            dependent['word'], dependent['pos'], dependent['deprel'],
                            dependent['head_id'], dependent['surprisal'], dependent['id']
                        )
                    
                    # Relative clauses
                    elif dependent['deprel'] in ['acl:relcl', 'acl', 'relcl']:
                        # Collect all tokens in the relative clause
                        rel_clause_token_ids = collect_relative_clause_tokens(dependent)
                        
                        # Add all tokens from the relative clause to the NP
                        for token_id in rel_clause_token_ids:
                            if token_id in token_by_id:
                                rel_token = token_by_id[token_id]
                                np.add_token(
                                    rel_token['word'], rel_token['pos'], rel_token['deprel'],
                                    rel_token['head_id'], rel_token['surprisal'], rel_token['id']
                                )
                    
                    # Prepositional phrases modifying the noun
                    elif dependent['deprel'] in ['nmod:prep', 'prep']:
                        # For "the book on the table", include the whole PP
                        pp_token_ids = collect_relative_clause_tokens(dependent)
                        for token_id in pp_token_ids:
                            if token_id in token_by_id:
                                pp_token = token_by_id[token_id]
                                np.add_token(
                                    pp_token['word'], pp_token['pos'], pp_token['deprel'],
                                    pp_token['head_id'], pp_token['surprisal'], pp_token['id']
                                )
            
            # Sort tokens by their position in sentence
            np.tokens.sort(key=lambda x: int(x['token_id']))
            
            if np.is_valid_np() and len(np.tokens) >= 1:
                nps.append(np)
    
    return nps

test6_tokens = [
        {'id': '1', 'word': 'The', 'pos': 'DET', 'deprel': 'det', 'head_id': '2', 'surprisal': '1.8'},
        {'id': '2', 'word': 'researcher', 'pos': 'NOUN', 'deprel': 'nsubj', 'head_id': '3', 'surprisal': '3.2'},
        {'id': '3', 'word': 'gave', 'pos': 'VERB', 'deprel': 'root', 'head_id': '0', 'surprisal': '2.1'},
        {'id': '4', 'word': 'the', 'pos': 'DET', 'deprel': 'det', 'head_id': '5', 'surprisal': '1.9'},
        {'id': '5', 'word': 'student', 'pos': 'NOUN', 'deprel': 'iobj', 'head_id': '3', 'surprisal': '2.8'},
        {'id': '6', 'word': 'a', 'pos': 'DET', 'deprel': 'det', 'head_id': '8', 'surprisal': '2.0'},
        {'id': '7', 'word': 'complex', 'pos': 'ADJ', 'deprel': 'amod', 'head_id': '8', 'surprisal': '4.5'},
        {'id': '8', 'word': 'assignment', 'pos': 'NOUN', 'deprel': 'obj', 'head_id': '3', 'surprisal': '3.7'},
        {'id': '9', 'word': 'that', 'pos': 'PRON', 'deprel': 'nsubj', 'head_id': '13', 'surprisal': '2.4'},
        {'id': '10', 'word': 'they', 'pos': 'PRON', 'deprel': 'nsubj', 'head_id': '13', 'surprisal': '3.1'},
        {'id': '11', 'word': 'could', 'pos': 'AUX', 'deprel': 'aux', 'head_id': '13', 'surprisal': '2.7'},
        {'id': '12', 'word': 'not', 'pos': 'PART', 'deprel': 'advmod', 'head_id': '13', 'surprisal': '3.8'},
        {'id': '13', 'word': 'solve', 'pos': 'VERB', 'deprel': 'acl:relcl', 'head_id': '8', 'surprisal': '4.2'}
    ]

nps = identify_NPs_in_sentence(test6_tokens, "test", {"year": 2025})

for np in nps:
    np.compute_surprisal_metrics()
    print(f"  Found NP: {np}")
    print(f"    Tokens: {[t['word'] for t in np.tokens]}")
    print(f"    Surprisals: {[t['surprisal'] for t in np.tokens]}")
    print(f"    Mean abs diff: {np.mean_abs_diff:.3f}")
    print(f"    Quadratic mean: {np.quadratic_mean_transitions:.3f}")
    print()
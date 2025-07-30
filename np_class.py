import numpy as np

class NounPhrase:
    """Class to represent a noun phrase with its tokens and features."""
    
    def __init__(self, text_id, metadata=None):
        self.text_id = text_id
        self.metadata = metadata or {}
        self.tokens = []
        self.head_token = None
        self.head_lemma = None
        self.head_deprel = None
        
        # Computed features
        self.uid_dev = None
        self.sigma_gamma = None
        
    def add_token(self, word, lemma, pos, deprel, head_id, surprisal, token_id):
        """Add a token to the NP."""
        token_info = {
            'word': word,
            'lemma': lemma,
            'pos': pos,
            'deprel': deprel,
            'head_id': head_id,
            'surprisal': float(surprisal) if surprisal else 0.0,
            'token_id': int(token_id)
        }
        self.tokens.append(token_info)
        
        # Check if this token is the head (NOUN with obj, nsubj, etc.)
        if pos == 'NOUN' and deprel in ['obj', 'nsubj', 'nsubj:pass', 'iobj']:
            self.head_token = token_info
            self.head_lemma = lemma
            self.head_deprel = deprel
    
    def compute_surprisal_metrics(self):
        """Compute surprisal-based transition metrics."""
        if len(self.tokens) < 2:
            self.uid_dev = 0.0
            self.sigma_gamma = 0.0
            return
        
        surprisals = [token['surprisal'] for token in self.tokens]
        diffs = np.diff(surprisals)
        
        # this implementation matches conceptually line 369-378 of postprocess_eval_results.py in https://github.com/thomashikaru/word-order-uid/tree/tacl-share/evaluation
        # this implementation matches conceptually also the function in revisiting-uid.ipynb at https://github.com/rycolab/revisiting-uid/tree/main/src
        # and should be faithful to Collins' (2014) UIDev proposal
        self.uid_dev = np.mean(np.abs(diffs)) if diffs.size > 0 else 0.0

        # this implementation should be faithful to information fluctuation complexity applied to texts, as it appeared in Brasolin, Bienati (2025)
        self.sigma_gamma = np.sqrt(np.mean((diffs - np.mean(diffs))**2))
    
    def is_valid_np(self):
        """Check if this is a valid NP (has head and at least one token)."""
        return self.head_token is not None and len(self.tokens) > 0
    
    def get_tokens_string(self):
        """Get space-separated string of all tokens."""
        return ' '.join([token['word'] for token in self.tokens])
    
    def to_dict(self):
        """Convert NP to dictionary for CSV writing."""
        self.compute_surprisal_metrics()
        
        surprisals = [token['surprisal'] for token in self.tokens]
        
        return {
            'text_id': self.text_id,
            'author': self.metadata.get('author'),
            'year': self.metadata.get('year'),
            'journal': self.metadata.get('journal'),
            'topic': self.metadata.get('topic'),
            'np_tokens': self.get_tokens_string(),
            'np_length': len(self.tokens),
            'head_word': self.head_token['word'] if self.head_token else None,
            'head_lemma': self.head_lemma,
            'head_deprel': self.head_deprel,
            'mean_surprisal': np.mean(surprisals) if surprisals else 0.0,
            'uid_dev': self.uid_dev,
            'sigma_gamma': self.sigma_gamma
        }

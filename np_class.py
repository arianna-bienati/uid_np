import numpy as np

class NounPhrase:
    """Class to represent a noun phrase with its tokens and features."""
    
    def __init__(self, text_id, metadata=None):
        self.text_id = text_id
        self.metadata = metadata or {}
        self.tokens = []  # List of (word, pos, deprel, head_id, surprisal, token_id)
        self.head_token = None
        self.head_lemma = None
        self.head_deprel = None
        
        # Computed features
        self.mean_abs_diff = None
        self.quadratic_mean_transitions = None
        
    def add_token(self, word, pos, deprel, head_id, surprisal, token_id):
        """Add a token to the NP."""
        token_info = {
            'word': word,
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
            self.head_lemma = word  # You might want to add actual lemma extraction
            self.head_deprel = deprel
    
    def compute_surprisal_metrics(self):
        """Compute surprisal-based transition metrics."""
        if len(self.tokens) < 2:
            self.mean_abs_diff = 0.0
            self.quadratic_mean_transitions = 0.0
            return
        
        surprisals = [token['surprisal'] for token in self.tokens]
        diffs = np.diff(surprisals)
        
        self.mean_abs_diff = np.mean(np.abs(diffs)) if diffs.size > 0 else 0.0
        self.quadratic_mean_transitions = np.sqrt(np.mean(diffs**2)) if diffs.size > 0 else 0.0
    
    def is_valid_np(self):
        """Check if this is a valid NP (has head and at least one token)."""
        return self.head_token is not None and len(self.tokens) > 0
    
    def get_tokens_string(self):
        """Get space-separated string of all tokens."""
        return ' '.join([token['word'] for token in self.tokens])
    
    def get_surprisals_string(self):
        """Get comma-separated string of all surprisal values."""
        return ','.join([str(token['surprisal']) for token in self.tokens])
    
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
            'head_surprisal': self.head_token['surprisal'] if self.head_token else None,
            'surprisals': self.get_surprisals_string(),
            'mean_surprisal': np.mean(surprisals) if surprisals else 0.0,
            'min_surprisal': min(surprisals) if surprisals else 0.0,
            'max_surprisal': max(surprisals) if surprisals else 0.0,
            'std_surprisal': np.std(surprisals) if surprisals else 0.0,
            'mean_abs_diff': self.mean_abs_diff,
            'quadratic_mean_transitions': self.quadratic_mean_transitions
        }

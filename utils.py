import csv
import re
from pathlib import Path

def extract_metadata(lines):
    """Extract metadata from corpus file lines."""
    metadata = {}
    
    for line in lines:
        if line.startswith('<text_id '):
            metadata['text_id'] = re.search(r'<text_id\s(.*?)>', line).group(1)
        elif line.startswith('<text_author '):
            metadata['author'] = re.search(r'<text_author\s(.*)>', line).group(1)
        elif line.startswith('<text_year '):
            metadata['year'] = re.search(r'<text_year\s(.*?)>', line).group(1)
        elif line.startswith('<text_jrnl '):
            metadata['journal'] = re.search(r'<text_jrnl\s(.*?)>', line).group(1)
        elif line.startswith('<text_primaryTopic '):
            metadata['topic'] = re.search(r'<text_primaryTopic\s(.*?)>', line).group(1)
    
    return metadata

def save_to_csv(data, output_file):
    """Save NP data to CSV file."""
    if not data:
        return
    
    # Convert NP objects to dictionaries
    rows = [np.to_dict() for np in data if np.is_valid_np()]
    
    if not rows:
        return
    
    # Define CSV header
    header = [
        'text_id', 'author', 'year', 'journal', 'topic',
        'np_tokens', 'np_length', 'head_word', 'head_lemma', 'head_deprel',
        'mean_surprisal', 'uid_dev', 'sigma_gamma'
    ]
    
    # Write to CSV
    file_exists = Path(output_file).exists() and Path(output_file).stat().st_size > 0
    
    with open(output_file, 'a', newline='', encoding='utf-8') as csv_file:
        writer = csv.DictWriter(csv_file, fieldnames=header)
        
        # Add header if file is empty
        if not file_exists:
            writer.writeheader()
        
        # Write NP data
        writer.writerows(rows)
import argparse
from pathlib import Path

import tqdm

from np_pipeline import extract_all_NPs
from utils import save_to_csv


def process(input_paths, output_file):
    """Process input files and extract NPs to output file."""
    processed_count = 0
    total_nps = 0

    # Use tqdm for progress tracking if there are multiple files
    for input_path in tqdm.tqdm(input_paths, desc="Extracting NPs"):
        if not input_path.suffix == '.vrt':
            continue
        
        with open(input_path, 'r', encoding='utf-8') as f:
            file_content = f.read()
            
        # Extract NP data
        np_data = extract_all_NPs(file_content)
            
        # Save to CSV
        save_to_csv(np_data, output_file)
            
        processed_count += 1
        total_nps += len([np for np in np_data if np.is_valid_np()])
            
        print(f'Processing file {input_path}... Found {len(np_data)} NPs')
    
    print(f'\nProcessing complete:')
    print(f'Files processed: {processed_count}')
    print(f'Total NPs extracted: {total_nps}')


def main():
    parser = argparse.ArgumentParser(description="Extract NPs from input files")
    parser.add_argument(
        "inputs", 
        nargs="+", 
        type=Path,
        help="Input files to process"
    )
    parser.add_argument(
        "-o", "--output",
        type=Path,
        default="./out.csv",
        help="Output location. Default: out.csv in current directory"
    )
    
    args = parser.parse_args()
    
    # Process the files
    process(args.inputs, args.output)


if __name__ == "__main__":
    main()
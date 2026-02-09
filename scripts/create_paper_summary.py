#!/usr/bin/env python3
"""
Create a comprehensive summary of downloaded papers
"""

import json
import os
from pathlib import Path
import re

def extract_year_from_arxiv_id(arxiv_id):
    """Extract year from arXiv ID (format: YY.MM.NNNNN)"""
    try:
        year_part = arxiv_id.split('.')[0]
        if year_part.isdigit():
            if int(year_part) >= 50:  # Assume 19xx for years 50-99
                return f"19{year_part}"
            else:  # Assume 20xx for years 00-49
                return f"20{year_part}"
    except:
        pass
    return "unknown"

def main():
    papers_dir = Path("/Users/sac/cre/docs/papers")
    
    # Read papers info
    with open(papers_dir / "papers_info.json", 'r') as f:
        papers = json.load(f)
    
    # Update years from arXiv IDs
    for paper in papers:
        if paper['year'] == 'unknown':
            paper['year'] = extract_year_from_arxiv_id(paper['arxiv_id'])
    
    # Count PDF files
    pdf_files = [f for f in os.listdir(papers_dir) if f.endswith('.pdf')]
    
    print("Van der Aalst Papers Download Summary")
    print("=" * 50)
    print(f"\nTotal papers found: {len(papers)}")
    print(f"Total PDF files: {len(pdf_files)}")
    
    # Year distribution
    years = [paper['year'] for paper in papers]
    from collections import Counter
    year_counts = Counter(years)
    
    print(f"\nYears distribution:")
    for year in sorted(year_counts.keys(), reverse=True):
        print(f"  {year}: {year_counts[year]} papers")
    
    # Show some recent papers
    print(f"\nMost recent papers:")
    recent_papers = sorted(papers, key=lambda x: x['arxiv_id'], reverse=True)[:10]
    for paper in recent_papers:
        print(f"  {paper['year']} - {paper['title']} ({paper['arxiv_id']})")
    
    # Save updated JSON
    with open(papers_dir / "papers_info.json", 'w') as f:
        json.dump(papers, f, indent=2, ensure_ascii=False)
    
    # Create markdown summary
    with open(papers_dir / "PAPER_SUMMARIES.md", 'w') as f:
        f.write("# Wil M. P. van der Aalst Papers Collection\n\n")
        f.write(f"This collection contains {len(papers)} papers downloaded from arXiv by Wil M. P. van der Aalst.\n\n")
        f.write("## Papers by Year\n\n")
        
        for year in sorted(year_counts.keys(), reverse=True):
            f.write(f"### {year} ({year_counts[year]} papers)\n\n")
            
            year_papers = [p for p in papers if p['year'] == year]
            for paper in year_papers:
                f.write(f"- **{paper['title']}** ({paper['arxiv_id']})\n")
            f.write("\n")
    
    print(f"\nUpdated papers_info.json and created PAPER_SUMMARIES.md")

if __name__ == "__main__":
    main()

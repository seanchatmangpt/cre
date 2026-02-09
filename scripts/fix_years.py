import json
import re

# Read papers info
with open('/Users/sac/cre/docs/papers/papers_info.json', 'r') as f:
    papers = json.load(f)

# Extract years from titles
for paper in papers:
    title = paper['title']
    match = re.search(r'\b(19|20)\d{2}\b', title)
    if match:
        paper['year'] = match.group(0)
    else:
        # Look for 2-digit year pattern
        match_2digit = re.search(r'\b(19|20)?(\d{2})\b(?![\d])', title)
        if match_2digit:
            if match_2digit.group(1):
                paper['year'] = match_2digit.group(1) + match_2digit.group(2)
            else:
                # Default to 20xx for 2-digit years after 2000
                year = int(match_2digit.group(2))
                if year >= 50:
                    paper['year'] = '19' + str(year)
                else:
                    paper['year'] = '20' + str(year)
        else:
            # Look for any 2-digit number as year
            match_any_2digit = re.search(r'\b\d{2}\b', title)
            if match_any_2digit:
                year = int(match_any_2digit.group(0))
                if year >= 50:
                    paper['year'] = '19' + str(year)
                else:
                    paper['year'] = '20' + str(year)
            else:
                # Keep as unknown if no year found
                paper['year'] = 'unknown'

# Save updated info
with open('/Users/sac/cre/docs/papers/papers_info.json', 'w') as f:
    json.dump(papers, f, indent=2, ensure_ascii=False)

# Show year distribution
years = [paper['year'] for paper in papers]
from collections import Counter
year_counts = Counter(years)

print('Papers by year:')
for year in sorted(year_counts.keys()):
    print(f'  {year}: {year_counts[year]}')

print(f'\nTotal papers: {len(papers)}')
print(f'Papers with valid years: {sum(1 for y in years if y != "unknown")}')

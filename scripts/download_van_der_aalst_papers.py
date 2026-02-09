#!/usr/bin/env python3
"""
Systematic downloader for all 121 papers by Wil M. P. van der Aalst from arXiv.
Handles pagination, extraction, and downloading of all papers.
"""

import requests
from bs4 import BeautifulSoup
import re
import os
import time
import concurrent.futures
from urllib.parse import urljoin, urlparse
import logging
from pathlib import Path
import json

# Set up logging
logging.basicConfig(level=logging.INFO, format='%(asctime)s - %(levelname)s - %(message)s')
logger = logging.getLogger(__name__)

class VanDerAalstPaperDownloader:
    def __init__(self, base_dir="/Users/sac/cre/docs/papers"):
        self.base_dir = Path(base_dir)
        self.base_dir.mkdir(parents=True, exist_ok=True)
        self.session = requests.Session()
        self.session.headers.update({
            'User-Agent': 'Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36'
        })
        
        # Track downloaded papers to avoid duplicates
        self.downloaded_papers = set()
        self.papers_info = []
        
    def sanitize_filename(self, title):
        """Clean title for safe filename"""
        # Remove special characters and limit length
        title = re.sub(r'[^\w\s-]', '', title)
        title = re.sub(r'[-\s]+', '_', title)
        return title[:100]  # Limit length
    
    def get_search_page(self, start_index=0):
        """Fetch a single search page"""
        url = f"https://arxiv.org/search/?searchtype=author&query=van+der+Aalst%2C+W+M+P&start={start_index}"
        logger.info(f"Fetching: {url}")
        
        try:
            response = self.session.get(url, timeout=30)
            response.raise_for_status()
            return response.text
        except Exception as e:
            logger.error(f"Error fetching {url}: {e}")
            return None
    
    def extract_papers_from_page(self, html_content, page_num):
        """Extract paper information from search page"""
        soup = BeautifulSoup(html_content, 'html.parser')
        papers = []
        
        # Find all paper entries
        paper_entries = soup.find_all('li', class_='arxiv-result')
        
        if not paper_entries:
            logger.warning(f"No papers found on page {page_num}")
            return papers
        
        for entry in paper_entries:
            # Extract arXiv ID
            arxiv_link = entry.find('a', href=True)
            if not arxiv_link:
                continue
                
            href = arxiv_link['href']
            arxiv_id_match = re.search(r'arxiv\.org/abs/(\d+\.\d+)', href)
            if not arxiv_id_match:
                continue
                
            arxiv_id = arxiv_id_match.group(1)
            
            # Extract title
            title_elem = entry.find('p', class_='title')
            title = title_elem.get_text(strip=True) if title_elem else f"Paper_{arxiv_id}"
            
            # Extract authors
            authors_elem = entry.find('p', class_='authors')
            authors = authors_elem.get_text(strip=True) if authors_elem else ""
            
            # Extract year
            year_match = re.search(r'\b(19|20)\d{2}\b', title)
            year = year_match.group(1) if year_match else "unknown"
            
            # Extract abstract
            abstract_elem = entry.find('p', class_='abstract')
            abstract = abstract_elem.get_text(strip=True) if abstract_elem else ""
            
            papers.append({
                'arxiv_id': arxiv_id,
                'title': title,
                'authors': authors,
                'year': year,
                'abstract': abstract,
                'pdf_url': f"https://arxiv.org/pdf/{arxiv_id}.pdf",
                'page_num': page_num
            })
            
            logger.info(f"Found paper: {title} ({arxiv_id})")
        
        return papers
    
    def get_all_papers(self):
        """Get all papers by handling pagination"""
        all_papers = []
        start_index = 0
        page_num = 1
        total_papers = 0
        
        while True:
            html_content = self.get_search_page(start_index)
            if not html_content:
                break
            
            page_papers = self.extract_papers_from_page(html_content, page_num)
            if not page_papers:
                break
                
            all_papers.extend(page_papers)
            total_papers = len(all_papers)
            
            logger.info(f"Page {page_num}: Found {len(page_papers)} papers, Total: {total_papers}")
            
            # Stop if we have fewer than 50 papers (last page)
            if len(page_papers) < 50:
                break
                
            start_index += 50
            page_num += 1
            
            # Small delay to be respectful
            time.sleep(1)
        
        logger.info(f"Total papers found: {len(all_papers)}")
        return all_papers
    
    def download_pdf(self, paper):
        """Download a single paper PDF"""
        try:
            response = self.session.get(paper['pdf_url'], timeout=60, stream=True)
            response.raise_for_status()
            
            # Create filename
            safe_title = self.sanitize_filename(paper['title'])
            filename = f"van_der_aalst_{paper['year']}_{safe_title}_{paper['arxiv_id']}.pdf"
            filepath = self.base_dir / filename
            
            # Check if already downloaded
            if filepath.exists():
                logger.info(f"Already exists: {filename}")
                self.downloaded_papers.add(paper['arxiv_id'])
                return True
            
            # Download
            with open(filepath, 'wb') as f:
                for chunk in response.iter_content(chunk_size=8192):
                    f.write(chunk)
            
            logger.info(f"Downloaded: {filename}")
            self.downloaded_papers.add(paper['arxiv_id'])
            return True
            
        except Exception as e:
            logger.error(f"Failed to download {paper['pdf_url']}: {e}")
            return False
    
    def download_all_papers(self):
        """Download all papers concurrently"""
        papers = self.get_all_papers()
        self.papers_info = papers
        
        logger.info(f"Starting download of {len(papers)} papers...")
        
        # Create info file
        info_file = self.base_dir / "papers_info.json"
        with open(info_file, 'w', encoding='utf-8') as f:
            json.dump(papers, f, indent=2, ensure_ascii=False)
        
        # Download concurrently
        success_count = 0
        with concurrent.futures.ThreadPoolExecutor(max_workers=5) as executor:
            futures = [executor.submit(self.download_pdf, paper) for paper in papers]
            
            for i, future in enumerate(concurrent.futures.as_completed(futures)):
                if future.result():
                    success_count += 1
                logger.info(f"Progress: {i+1}/{len(papers)}")
        
        # Generate report
        self.generate_report(papers, success_count)
        
        logger.info(f"Download complete: {success_count}/{len(papers)} papers downloaded")
        
        return papers
    
    def generate_report(self, papers, success_count):
        """Generate download report"""
        report = f"""
Van der Aalst Papers Download Report
===================================

Total papers found: {len(papers)}
Successfully downloaded: {success_count}
Failed: {len(papers) - success_count}

Papers downloaded:
"""
        
        for paper in papers:
            status = "✓" if paper['arxiv_id'] in self.downloaded_papers else "✗"
            report += f"{status} {paper['year']} - {paper['title']} ({paper['arxiv_id']})\n"
        
        # Write report
        report_file = self.base_dir / "download_report.txt"
        with open(report_file, 'w', encoding='utf-8') as f:
            f.write(report)
        
        logger.info(f"Report saved to: {report_file}")

def main():
    downloader = VanDerAalstPaperDownloader()
    papers = downloader.download_all_papers()
    
    print(f"\nDownloaded {len(papers)} papers to {downloader.base_dir}")
    print(f"Downloaded {len(downloader.downloaded_papers)} unique papers")

if __name__ == "__main__":
    main()

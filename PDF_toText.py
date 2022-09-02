# -*- coding: utf-8 -*-
"""
Created on Thu Jul  7 17:17:40 2022

@author: Juan Camilo Betancur Jaramillo
"""

import fitz


def extract_text(path, all_pages=True, first_page=0, last_page=1) -> 'Tuple':
 
    '''
    This function takes a PDF file and by default returns the text from 
    all its pages and the page count as a tuple. However, it can retrieve
    the text from a range of pages. When all_pages == False, by default 
    it retrieves the text of the first page, but can be modified to any range.

    path: Path to the PDF file

    all_pages: Retrieve the text from all the pages. Default == True

    first_page: First page of the rank. By default it is the first page

    last_page: Last page of the rank. By default it is the second page
    '''
    ### Loads the PDF document
    doc = fitz.open(path)
    text = ""
    
    ### Loads the PDF's pages
    if all_pages == True:
        pages = [doc.load_page(i) for i in range(doc.page_count)]
    else: 
        pages = [doc.load_page(i) for i in range(first_page, last_page)]
    
    ### Loops through the PDF's pages and extracts its text
    for page in pages:
        text += page.get_text()
    
    ### Returns a tuple containing both the text as a string and 
    ### the number of pages of the document. For retrieving only the
    ### text, you can index the result as follows:
    ### extract_text('example_path.pdf', all_pages=True)[0]
    return text, doc.page_count
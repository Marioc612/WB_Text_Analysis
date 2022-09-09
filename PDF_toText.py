# -*- coding: utf-8 -*-
"""
Created on: 2022-Jul-7-17:17
Edited on:  2022-Sep-9-14:46

@author: Juan Camilo Betancur Jaramillo
"""

import fitz
import pandas as pd
import os
import glob


def extract(paths, all_pages=True, first_page=0, last_page=1) -> 'Tuple':
    '''
    This function takes a PDF file and by default returns the text from 
    all its pages and the page count as a tuple. However, it can retrieve
    the text from a range of pages. When all_pages == False, by default 
    it retrieves the text of the first page, but can be modified to any range.

    paths: Path to the PDF file or list of paths

    all_pages: Retrieve the text from all the pages. Default == True

    first_page: First page of the rank. By default it is the first page

    last_page: Last page of the rank. By default it is the second page
    '''

    # Loads the PDF document
    doc = fitz.open(paths)
    text = ""

    # Loads the PDF's pages
    if all_pages == True:
        pages = [doc.load_page(i) for i in range(doc.page_count)]
    else:
        pages = [doc.load_page(i) for i in range(first_page, last_page)]

    # Loops through the PDF's pages and extracts its text
    for page in pages:
        text += page.get_text()

    # Returns a tuple containing both the text as a string and
    # the number of pages of the document. For retrieving only the
    # text, you can index the result as follows:
    ### extract_text('example_path.pdf', all_pages=True)[0]
    return text, doc.page_count


def extract_many(folder: 'str',
                 rel_path: 'str' = True,
                 export_json: 'bool' = False,
                 json_path: 'str' = os.getcwd(),
                 json_name: 'str' = 'PDFtext_extract') -> 'DataFrame':
    '''
    Gets a folder name or path and returns the texts of the PDF
    files inside of it as a Pandas DataFrame. The DataFrame's
    columns are: 'texts' and 'numPages'.

    folder: relative or absolute path to the folder. If you 
    introduce an absolute path, the parameter rel_path should be
    passed as False (rel_path = False).

    rel_path: set as True by default. It should be set to False
    if you want to introduce an absolute path to the folder.

    json_path: path to the exported json file. It defaults to the
    current working directory (cwd)

    json_name_ the name of the exported json file. It defaults to
    'PDFtext_extract'. You should not introduce the file extension.
    '''

    if rel_path:
        # Gets current working directory
        cwd = os.getcwd().replace('\\', '/')

        # Gets the paths of the files inside a folder
        paths = glob.glob(cwd+f'/{folder}/*')

    if not rel_path:
        # Gets the paths of the files inside a folder
        paths = glob.glob(f'{folder}/*')

    # Gets only the PDF file paths
    paths = [path.replace('\\', '/') for path in paths
             if path[-4:] == '.pdf']

    # Gets the tuples with the full texts and the number of pages
    all = [extract(path, all_pages=True)[0] for path in paths]

    # Unpacks the tuples into two separate lists
    texts = [item[0] for item in all]
    pages = [item[1] for item in all]

    # Creates the Pandas DataFrame to be returned
    df = pd.DataFrame({'texts': texts,
                       'numPages': pages})

    if export_json:
        df.to_json(f'{json_path}/{json_name}.json')

    return df


if __name__ == '__main__':
    print("This file is not intended to be executed, but imported.")

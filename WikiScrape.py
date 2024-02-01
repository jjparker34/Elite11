import requests
from bs4 import BeautifulSoup
import pandas as pd
from io import StringIO

url = 'https://en.wikipedia.org/wiki/Elite_11'
response = requests.get(url)
soup = BeautifulSoup(response.text, 'html.parser')

tables = soup.find_all('table')
data_list = []


for table in tables:
    # Check if the table has the class 'wikitable'
    if "wikitable" in table.get('class', []):
        rows = table.find_all('tr')
        for row in rows:
            columns = row.find_all('td')
            # Initialize an empty list to store cell data in a row
            row_data = []
            for column in columns:
                cell_data = column.get_text(strip=True)  # Get the text content of the cell
                row_data.append(cell_data)  # Append cell data to the row list
            if row_data:  # Check if the row data is not empty
                data_list.append(row_data)  # Append the row data to the data_list

df = pd.DataFrame(data_list)

df.to_csv('Elite11.csv', index=False)




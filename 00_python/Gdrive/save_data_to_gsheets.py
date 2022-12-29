import gspread
from google.colab import auth
from google.auth import default
import random

auth.authenticate_user()

creds, _ = default()
gc = gspread.authorize(creds)
sh = gc.create('A new spreadsheet')

# Open our new sheet and add some data.
worksheet = gc.open('A new spreadsheet').sheet1

cell_list = worksheet.range('A1:C2')

for cell in cell_list:
  cell.value = random.randint(1, 10)

worksheet.update_cells(cell_list)
# Go to https://sheets.google.com to see your new spreadsheet.

import requests
from bs4 import BeautifulSoup as bs

# Collect data from GitHub using web scraping techniques.

# - Profile picture
# - The name of the user
# - A short description of the user

github_profile = "https://github.com/leandro-raposo"
req = requests.get(github_profile)
scraper = bs(req.content, "html.parser")
profile_picture = scraper.find("img", {"alt": "Avatar"})["src"]

print(profile_picture)

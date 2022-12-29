from pydrive.auth import GoogleAuth
from pydrive.drive import GoogleDrive
from google.colab import auth # fix this
from oauth2client.client import GoogleCredentials

# Authenticate and create the PyDrive client.
auth.authenticate_user()
gauth = GoogleAuth()
gauth.credentials = GoogleCredentials.get_application_default()
drive = GoogleDrive(gauth)

# Download a file based on its file ID
file_id = '1QUZKz3eVpoGMvoVEAZ_YzSpVjs1yIDElvBzkNTQvSDs'
downloaded = drive.CreateFile({'id': file_id})
print('Downloaded content "{}"'.format(downloaded.GetContentString()))

########################### step 01 #############################
from google.colab import auth
auth.authenticate_user()

# https://cloud.google.com/resource-manager/docs/creating-managing-projects
project_id = '[your Cloud Platform project ID]'

########################### step 02 #############################

from googleapiclient.discovery import build
gcs_service = build('storage', 'v1')

# Generate a random bucket name to which we'll upload the file.
import uuid
bucket_name = 'colab-sample-bucket' + str(uuid.uuid1())

body = {
  'name': bucket_name,
  # For a full list of locations, see:
  # https://cloud.google.com/storage/docs/bucket-locations
  'location': 'us',
}
gcs_service.buckets().insert(project=project_id, body=body).execute()
print('Done')

########################### step 03 #############################

from googleapiclient.http import MediaFileUpload

media = MediaFileUpload('/tmp/to_upload.txt', 
                        mimetype='text/plain',
                        resumable=True)

request = gcs_service.objects().insert(bucket=bucket_name, 
                                       name='to_upload.txt',
                                       media_body=media)

response = None
while response is None:
  # _ is a placeholder for a progress object that we ignore.
  # (Our file is small, so we skip reporting progress.)
  _, response = request.next_chunk()

print('Upload complete')
print('https://console.cloud.google.com/storage/browser?project={}'.format(project_id))


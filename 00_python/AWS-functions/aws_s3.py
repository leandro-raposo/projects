
import awsLogin.awsLogin as al

# s3uploader
client = al.login()

client.create_bucket(
    ACL='public',
    Bucket='lr-bucket-wordpress', # altere para um nome qualquer
    CreateBucketConfiguration={
        'LocationConstraint': 'us-east-1'
    },
)
# upload no S3
client.upload_file("hello-s3.txt", "meu-exemplo-buteco-opensource", "hello-s3")

# obter arquivo do s3
client.download_file("meu-exemplo-buteco-opensource", "hello-s3", "downloaded-hello-s3.txt")
d = open("downloaded-hello-s3.txt")
print(d.readlines())

# listar buckets
response = client.list_buckets()

print('Existing buckets:')
for bucket in response['Buckets']:
    print(f'  {bucket["Name"]}')


# listar objetos dentro do bucket
def list_objects_bucket(bucket_name):
  response = client.list_objects(Bucket=bucket_name)

  for object in response['Contents']:
    print(object['Key'])
    
    
list_objects_bucket('cjmm-primeirobucket')

# obter objeto do bucket
def download_object(bucket_name, object_name, file_name):
  client.download_file(bucket_name, object_name, file_name)

client.download_file('cjmm-primeirobucket', 'sample_data/mnist_test.csv', 'mnist_test.csv')

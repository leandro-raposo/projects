from google.cloud import storage

# List buckets.
# The client library will look for credentials using ADC.

def authenticate_implicit_with_adc(project_id="your-google-cloud-project-id"):
    storage_client = storage.Client(project=project_id)
    buckets = storage_client.list_buckets()
    print("Buckets:")
    for bucket in buckets:
        print(bucket.name)
    print("Listed all storage buckets.")


auto = authenticate_implicit_with_adc('solar-dialect-373018')

print(auto)

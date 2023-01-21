
import openai

token = open("chave-chatgpt.txt").readlines()
openai.api_key = token[0]

prompt = "Quais foram os presidentes do Brasil?"
completions = openai.Completion.create(engine="text-davinci-002", prompt=prompt, max_tokens=1024)
print(completions.choices[0].text)

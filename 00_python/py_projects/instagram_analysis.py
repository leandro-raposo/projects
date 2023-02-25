import instaloader
import pandas as pd

# Inicializar o objeto instaloader
L = instaloader.Instaloader()

# Carregar a publicação
post = instaloader.Post.from_shortcode(L.context, 'XXXXXX')
# Substitua XXXXXX pelo shortcode da publicação

# Criar um dataframe para armazenar os comentários
comments_df = pd.DataFrame(columns=['username', 'text', 'timestamp'])

# Iterar sobre os comentários e adicionar ao dataframe
for comment in post.get_comments():
    comments_df = comments_df.append({'username': comment.owner_username,
                                      'text': comment.text,
                                      'timestamp': comment.created_at_utc},
                                     ignore_index=True)

# Salvar o dataframe em um arquivo CSV
comments_df.to_csv('comments.csv', index=False)

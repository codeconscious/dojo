from urllib.parse import urlunparse
from urllib.request import urlopen
with urlopen("http://codeconscio.us") as url:
    html = url.read()
    print(html)
```
docker run --name elm-spa -v "$(pwd)":/usr/share/nginx/html:ro -d  -p 8000:80 --rm nginx
```
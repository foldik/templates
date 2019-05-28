__GET PAGE__

```
curl --request GET "http://localhost:8000/api/resources?page=1&limit=10"
```

__GET__

```
curl --request GET "http://localhost:8000/api/resources/1"
```

__POST__

```
curl --request POST --header 'Content-Type: application/json' --data '{ "name": "Wow" }' http://localhost:8000/api/resources
```

__DELETE__

```
curl --request DELETE http://localhost:8000/api/resources/1
```
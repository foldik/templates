# Development

For development this project uses [elm-live](https://github.com/wking-io/elm-live) for hot code reloading and [json-server](https://github.com/typicode/json-server) to provide a mock API that the frontend can use.

```
npm run mock-api & npm run dev
```

Now you should see the frontend under http://localhost:9000. Additionally you can check the mock API under http://localhost:9001.

# Build

```
npm run build
```

# Build docker image

The `Dockerfile` contains the following steps:

1. Copy package.json and elm.json,
1. pull npm and elm dependencies (initial build can take 1-2 minutes but after that docker caches this layer, so later builds will be much faster),
1. copy the actual project,
1. build frontend,
1. copy the frontend into an nginx based docker image.

__Build the image__

```
docker build -t elm-spa .
```

__Start the application__

```
docker run --name elm-spa -d -p 8000:80 --rm elm-spa
```

Now you should see the frontend under http://localhost:8000.
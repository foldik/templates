######################## Build
FROM node:11-alpine as build

## Cache
COPY package.json ./
COPY elm.json .
RUN npm install
COPY src/placeholder src/Main.elm
RUN npm run build

## Build artifact
COPY . ./

RUN npm run build
RUN npm run uglifyjs
RUN npm run build-style

######################## Final
FROM nginx:1.16-alpine

COPY --from=build index.html /usr/share/nginx/html/
COPY --from=build dist/ /usr/share/nginx/html/dist/
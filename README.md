To run dev docker compose: 
```bash
docker compose -f docker-compose.dev.yml up --build
```
and then access the app at `http://localhost:8080` in your web browser.
It will live reload on code changes to the frontend or backend.

to run prod dockerfile:
```bash
docker build -t bf-compiler .
docker run -p 8080:[host-port] bf-compiler
```
and then access the app at `http://localhost:[host-port]` in your web browser.
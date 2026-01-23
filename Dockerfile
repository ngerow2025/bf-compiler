# -------- Stage 1: Build WASM with Rust --------
FROM rust:1.93-alpine AS wasm-builder

# Install build dependencies
RUN apk add --no-cache musl-dev

# Install wasm-pack
RUN cargo install wasm-pack

WORKDIR /app

# Copy Rust project files
COPY Cargo.toml Cargo.lock* ./
COPY src ./src

# Build WASM
RUN wasm-pack build --release --target bundler --out-dir web-frontend/src/wasm --out-name compiler_bf_target

# -------- Stage 2: Build React app --------
FROM node:20-alpine AS builder

# Install pnpm
RUN corepack enable && corepack prepare pnpm@latest --activate

WORKDIR /app

# Copy package files first (cache-friendly)
COPY web-frontend/package.json web-frontend/pnpm-lock.yaml web-frontend/pnpm-workspace.yaml* ./

# Install dependencies
RUN pnpm install --frozen-lockfile

# Copy the rest of the frontend source
COPY web-frontend/ ./

# Copy WASM output from previous stage
COPY --from=wasm-builder /app/web-frontend/src/wasm ./src/wasm

# Build the React app
RUN pnpm run build

# -------- Stage 3: NGINX runtime --------
FROM nginxinc/nginx-unprivileged:1.25-alpine

# Copy custom nginx config
COPY nginx-config/nginx.conf /etc/nginx/conf.d/default.conf 


# Copy static output
COPY --from=builder /app/dist /usr/share/nginx/html

EXPOSE 8080

CMD ["nginx", "-g", "daemon off;"]

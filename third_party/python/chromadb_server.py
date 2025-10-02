#!/usr/bin/env python3
# File path: third_party/python/chromadb_server.py
"""Lightweight Flask wrapper that exposes a ChromaDB-compatible REST API."""

import os
from typing import List

from flask import Flask, jsonify, request
import chromadb
from chromadb.config import Settings


DEFAULT_PERSIST_DIR = os.getenv("CHROMADB_PERSIST_DIR", "./chroma_data")
DEFAULT_COLLECTION = os.getenv("CHROMADB_COLLECTION", "cwa_docs")


def create_app() -> Flask:
    app = Flask(__name__)

    persist_dir = os.getenv("CHROMADB_PERSIST_DIR", DEFAULT_PERSIST_DIR)
    allow_reset = os.getenv("CHROMADB_ALLOW_RESET", "true").lower() != "false"

    settings = Settings(
        persist_directory=persist_dir,
        allow_reset=allow_reset,
    )
    client = chromadb.Client(settings)

    @app.get("/api/v1/heartbeat")
    def heartbeat():
        """Simple readiness probe."""
        return jsonify({"nanosecond heartbeat": 1})

    @app.get("/api/v1/collections")
    def list_collections():
        collections = client.list_collections()
        return jsonify(
            {
                "collections": [
                    {"id": collection.name, "name": collection.name}
                    for collection in collections
                ]
            }
        )

    @app.post("/api/v1/collections")
    def create_collection():
        data = request.get_json(force=True, silent=True) or {}
        name = data.get("name", DEFAULT_COLLECTION)
        try:
            collection = client.get_or_create_collection(name=name)
        except Exception as exc:  # noqa: BLE001
            return jsonify({"error": str(exc)}), 400
        return jsonify({"id": collection.name, "name": collection.name})

    @app.post("/api/v1/collections/<collection_id>/add")
    def add_to_collection(collection_id: str):
        data = request.get_json(force=True, silent=True) or {}
        try:
            collection = client.get_collection(name=collection_id)
        except Exception as exc:  # noqa: BLE001
            return jsonify({"error": str(exc)}), 400

        ids: List[str] = data.get("ids", [])
        documents: List[str] = data.get("documents", [])
        metadatas = data.get("metadatas") or None
        embeddings = data.get("embeddings") or None

        try:
            collection.add(
                ids=ids,
                documents=documents,
                metadatas=metadatas,
                embeddings=embeddings,
            )
        except Exception as exc:  # noqa: BLE001
            return jsonify({"error": str(exc)}), 400
        return jsonify({"status": "success"})

    @app.post("/api/v1/collections/<collection_id>/upsert")
    def upsert_into_collection(collection_id: str):
        data = request.get_json(force=True, silent=True) or {}
        try:
            collection = client.get_collection(name=collection_id)
        except Exception as exc:  # noqa: BLE001
            return jsonify({"error": str(exc)}), 400

        ids: List[str] = data.get("ids", [])
        documents: List[str] = data.get("documents", [])
        metadatas = data.get("metadatas") or None
        embeddings = data.get("embeddings") or None

        try:
            collection.upsert(
                ids=ids,
                documents=documents,
                metadatas=metadatas,
                embeddings=embeddings,
            )
        except Exception as exc:  # noqa: BLE001
            return jsonify({"error": str(exc)}), 400
        return jsonify({"status": "success"})

    @app.post("/api/v1/collections/<collection_id>/query")
    def query_collection(collection_id: str):
        data = request.get_json(force=True, silent=True) or {}
        try:
            collection = client.get_collection(name=collection_id)
        except Exception as exc:  # noqa: BLE001
            return jsonify({"error": str(exc)}), 400

        query_embeddings = data.get("query_embeddings", [])
        n_results = data.get("n_results", 10)

        try:
            results = collection.query(
                query_embeddings=query_embeddings,
                n_results=n_results,
            )
        except Exception as exc:  # noqa: BLE001
            return jsonify({"error": str(exc)}), 400
        return jsonify(results)

    return app


def main() -> None:
    app = create_app()

    host = os.getenv("CHROMADB_SERVER_HOST", "127.0.0.1")
    port = int(os.getenv("CHROMADB_SERVER_PORT", "8000"))
    persist_dir = os.getenv("CHROMADB_PERSIST_DIR", DEFAULT_PERSIST_DIR)

    os.makedirs(persist_dir, exist_ok=True)

    print(
        "Starting ChromaDB HTTP server on http://%s:%s" % (host, port),
        flush=True,
    )
    print(f"Data will be persisted to: {persist_dir}", flush=True)

    app.run(host=host, port=port, debug=False)


if __name__ == "__main__":
    main()

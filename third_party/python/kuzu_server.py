#!/usr/bin/env python3
# File path: third_party/python/kuzu_server.py
"""Flask proxy that exposes minimal REST endpoints for Kuzu queries."""

import os
from typing import Iterable, List

from flask import Flask, jsonify, request
import kuzu


DEFAULT_DB_PATH = os.getenv("KUZU_DB_PATH", "./kuzu_data")


def _materialize(result: kuzu.QueryResult) -> List[List[object]]:
    rows: List[List[object]] = []
    while result.has_next():
        rows.append(list(result.get_next()))
    return rows


def create_app() -> Flask:
    app = Flask(__name__)

    db_path = os.getenv("KUZU_DB_PATH", DEFAULT_DB_PATH)
    os.makedirs(db_path, exist_ok=True)

    database = kuzu.Database(db_path)
    connection = kuzu.Connection(database)

    @app.get("/health")
    def health():
        return jsonify({"status": "ok"})

    @app.post("/query")
    def query():
        data = request.get_json(force=True, silent=True) or {}
        query_str = data.get("query", "")
        try:
            result = connection.execute(query_str)
        except Exception as exc:  # noqa: BLE001
            return jsonify({"success": False, "error": str(exc)}), 400
        rows = _materialize(result)
        return jsonify({"success": True, "data": rows})

    @app.post("/execute")
    def execute():
        data = request.get_json(force=True, silent=True) or {}
        statements: Iterable[str] = data.get("statements", [])

        responses = []
        for statement in statements:
            try:
                connection.execute(statement)
                responses.append({"success": True})
            except Exception as exc:  # noqa: BLE001
                responses.append({"success": False, "error": str(exc)})
        return jsonify({"results": responses})

    return app


def main() -> None:
    app = create_app()

    host = os.getenv("KUZU_SERVER_HOST", "127.0.0.1")
    port = int(os.getenv("KUZU_SERVER_PORT", "9000"))

    print(
        "Starting Kuzu HTTP proxy on http://%s:%s" % (host, port),
        flush=True,
    )
    print(f"Database path: {os.getenv('KUZU_DB_PATH', DEFAULT_DB_PATH)}", flush=True)

    app.run(host=host, port=port, debug=False)


if __name__ == "__main__":
    main()

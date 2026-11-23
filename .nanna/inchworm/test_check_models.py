import json
import os
import pathlib
import subprocess
import threading
import unittest
from http.server import BaseHTTPRequestHandler, HTTPServer

SCRIPT = pathlib.Path(__file__).resolve().parent / "check_models.sh"


class Fake:
    def __init__(self):
        self.chats = []
        fake = self

        class Handler(BaseHTTPRequestHandler):
            def log_message(self, *args):
                pass

            def do_POST(self):
                body = json.loads(self.rfile.read(int(self.headers["Content-Length"])))
                if self.path == "/api/chat":
                    fake.chats.append(body)
                    out = b'{"message":{"content":"ok"}}'
                else:
                    out = b"{}"
                self.send_response(200)
                self.send_header("Content-Length", str(len(out)))
                self.end_headers()
                self.wfile.write(out)

        self.server = HTTPServer(("127.0.0.1", 0), Handler)
        self.url = f"http://127.0.0.1:{self.server.server_port}"
        threading.Thread(target=self.server.serve_forever, daemon=True).start()

    def close(self):
        self.server.shutdown()
        self.server.server_close()


class CheckModelsTests(unittest.TestCase):
    def test_only_leanstral_probe_sets_the_modelfile_context(self):
        fake = Fake()
        self.addCleanup(fake.close)
        p = subprocess.run(
            ["bash", str(SCRIPT)],
            env={"PATH": os.environ["PATH"], "OLLAMA_HOST_URL": fake.url},
            capture_output=True,
            text=True,
            timeout=60,
        )
        self.assertEqual(p.returncode, 0, p.stderr)
        by_model = {c["model"]: c["options"] for c in fake.chats}
        self.assertEqual(by_model["leanstral-whitedwarf"]["num_ctx"], 8192)
        self.assertNotIn("num_ctx", by_model["gemma4:e4b"])

    def test_unserved_model_fails(self):
        p = subprocess.run(
            ["bash", str(SCRIPT)],
            env={"PATH": os.environ["PATH"], "OLLAMA_HOST_URL": "http://127.0.0.1:1", "PROBE": "0"},
            capture_output=True,
            text=True,
            timeout=60,
        )
        self.assertEqual(p.returncode, 1)
        self.assertIn("not served", p.stderr)


if __name__ == "__main__":
    unittest.main()

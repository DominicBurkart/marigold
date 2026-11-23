import hashlib
import json
import os
import pathlib
import shutil
import subprocess
import tempfile
import threading
import unittest
from http.server import BaseHTTPRequestHandler, HTTPServer

SRC = pathlib.Path(__file__).resolve().parent / "leanstral"


class Fake:
    def __init__(self, blob_present, create_reply):
        self.blob_present = blob_present
        self.create_reply = create_reply
        self.requests = []
        fake = self

        class Handler(BaseHTTPRequestHandler):
            def log_message(self, *args):
                pass

            def record(self, body=b""):
                fake.requests.append(
                    {
                        "method": self.command,
                        "path": self.path,
                        "headers": {k.lower(): v for k, v in self.headers.items()},
                        "body": body,
                    }
                )

            def reply(self, status, body=b""):
                self.send_response(status)
                self.send_header("Content-Length", str(len(body)))
                self.end_headers()
                if self.command != "HEAD":
                    self.wfile.write(body)

            def do_HEAD(self):
                self.record()
                self.reply(200 if fake.blob_present else 404)

            def do_POST(self):
                length = int(self.headers.get("Content-Length", 0))
                body = self.rfile.read(length)
                self.record(body)
                if self.path.startswith("/api/blobs/"):
                    self.reply(201)
                else:
                    self.reply(200, fake.create_reply.encode())

        self.server = HTTPServer(("127.0.0.1", 0), Handler)
        self.url = f"http://127.0.0.1:{self.server.server_port}"
        self.thread = threading.Thread(target=self.server.serve_forever, daemon=True)
        self.thread.start()

    def close(self):
        self.server.shutdown()
        self.server.server_close()


class ImportCase(unittest.TestCase):
    def setUp(self):
        self.tmp = tempfile.TemporaryDirectory()
        self.dir = pathlib.Path(self.tmp.name)
        self.script_dir = self.dir / "leanstral"
        self.script_dir.mkdir()
        for f in ("import.sh", "Modelfile"):
            shutil.copy(SRC / f, self.script_dir / f)
        self.gguf = self.dir / "model.gguf"
        self.gguf.write_bytes(b"not a real model" * 1000)
        self.sum = hashlib.sha256(self.gguf.read_bytes()).hexdigest()
        self.fakes = []

    def tearDown(self):
        for f in self.fakes:
            f.close()
        self.tmp.cleanup()

    def run_import(self, blob_present=False, reply='{"status":"success"}\n', sha=None, gguf=None):
        fake = Fake(blob_present, reply)
        self.fakes.append(fake)
        env = {
            "PATH": os.environ["PATH"],
            "OLLAMA_HOST_URL": fake.url,
            "LEANSTRAL_SHA256": sha or self.sum,
        }
        p = subprocess.run(
            ["bash", str(self.script_dir / "import.sh"), str(gguf or self.gguf)],
            env=env,
            capture_output=True,
            text=True,
            timeout=60,
        )
        return p, fake

    def blob_posts(self, fake):
        return [r for r in fake.requests if r["path"].startswith("/api/blobs/") and r["method"] != "HEAD"]

    def create_payload(self, fake):
        creates = [r for r in fake.requests if r["path"] == "/api/create"]
        self.assertEqual(len(creates), 1)
        return json.loads(creates[0]["body"])


class UploadTests(ImportCase):
    def test_uploads_when_blob_absent_as_raw_post(self):
        p, fake = self.run_import(blob_present=False)
        self.assertEqual(p.returncode, 0, p.stderr)
        posts = self.blob_posts(fake)
        self.assertEqual(len(posts), 1)
        self.assertEqual(posts[0]["method"], "POST")
        self.assertEqual(posts[0]["path"], f"/api/blobs/sha256:{self.sum}")
        self.assertEqual(posts[0]["body"], self.gguf.read_bytes())
        self.assertNotIn("application/x-www-form-urlencoded", posts[0]["headers"].get("content-type", ""))

    def test_head_checked_before_upload(self):
        p, fake = self.run_import(blob_present=False)
        self.assertEqual(fake.requests[0]["method"], "HEAD")
        self.assertEqual(fake.requests[0]["path"], f"/api/blobs/sha256:{self.sum}")

    def test_skips_upload_when_blob_present(self):
        p, fake = self.run_import(blob_present=True)
        self.assertEqual(p.returncode, 0, p.stderr)
        self.assertEqual(self.blob_posts(fake), [])
        self.assertEqual(len([r for r in fake.requests if r["path"] == "/api/create"]), 1)

    def test_checksum_mismatch_makes_no_requests(self):
        p, fake = self.run_import(sha="0" * 64)
        self.assertEqual(p.returncode, 1)
        self.assertEqual(fake.requests, [])


class CreateTests(ImportCase):
    def test_create_references_blob_and_file_name(self):
        p, fake = self.run_import(blob_present=True)
        payload = self.create_payload(fake)
        self.assertEqual(payload["files"], {"model.gguf": f"sha256:{self.sum}"})
        self.assertFalse(payload["stream"])

    def test_parameters_come_from_modelfile(self):
        p, fake = self.run_import(blob_present=True)
        payload = self.create_payload(fake)
        self.assertEqual(payload["parameters"], {"temperature": 1.0, "num_ctx": 8192, "stop": ["</s>"]})

    def test_changed_modelfile_parameters_are_followed(self):
        mf = self.script_dir / "Modelfile"
        text = mf.read_text().replace("PARAMETER num_ctx 8192", "PARAMETER num_ctx 4096")
        text += 'PARAMETER stop "[/INST]"\nPARAMETER top_p 0.5\n'
        mf.write_text(text)
        p, fake = self.run_import(blob_present=True)
        params = self.create_payload(fake)["parameters"]
        self.assertEqual(params["num_ctx"], 4096)
        self.assertEqual(params["stop"], ["</s>", "[/INST]"])
        self.assertEqual(params["top_p"], 0.5)

    def test_template_comes_from_modelfile(self):
        p, fake = self.run_import(blob_present=True)
        template = self.create_payload(fake)["template"]
        self.assertTrue(template.startswith("<s>"))
        self.assertIn("[INST]", template)

    def test_error_in_create_response_fails(self):
        p, fake = self.run_import(blob_present=True, reply='{"error":"boom"}\n')
        self.assertNotEqual(p.returncode, 0)
        self.assertIn("boom", p.stderr)

    def test_error_after_progress_lines_fails(self):
        p, fake = self.run_import(blob_present=True, reply='{"status":"parsing"}\n{"error":"bad"}\n')
        self.assertNotEqual(p.returncode, 0)
        self.assertIn("bad", p.stderr)

    def test_success_reply_exits_zero(self):
        p, fake = self.run_import(blob_present=True, reply='{"status":"parsing"}\n{"status":"success"}\n')
        self.assertEqual(p.returncode, 0, p.stderr)


if __name__ == "__main__":
    unittest.main()

import pathlib
import tempfile
import textwrap
import unittest

import validate

ROOT = pathlib.Path(__file__).resolve().parents[2]


def write(path, text):
    path.parent.mkdir(parents=True, exist_ok=True)
    path.write_text(textwrap.dedent(text))


def make_repo(tmp, cards, mapping, envs):
    for name, tools in cards.items():
        tool_list = ", ".join(f'"{t}"' for t in tools)
        write(
            tmp / ".nanna/catalog" / f"{name}.toml",
            f'[identity]\nname = "{name}"\n[scope]\ntools = [{tool_list}]\n',
        )
    map_lines = "\n".join(f'{k} = "{v}"' for k, v in mapping.items())
    write(tmp / ".nanna/environments/cards.toml", f"[map]\n{map_lines}\n")
    for env, (caps, containerfile) in envs.items():
        cap_text = "".join(
            f'\n[[capability]]\nid = "{c}"\ndescription = "d"\n{extra}'
            for c, extra in caps
        )
        write(
            tmp / ".nanna/environments" / env / "environment.toml",
            f'schema = 1\nname = "{env}"\n[build]\ncontainerfile = "Containerfile"\n{cap_text}',
        )
        write(tmp / ".nanna/environments" / env / "Containerfile", containerfile)


class RealRepo(unittest.TestCase):
    def test_repository_is_valid(self):
        self.assertEqual(validate.validate(ROOT), [])


class Rules(unittest.TestCase):
    def run_case(self, cards, mapping, envs):
        with tempfile.TemporaryDirectory() as d:
            tmp = pathlib.Path(d)
            make_repo(tmp, cards, mapping, envs)
            return validate.validate(tmp)

    def test_accepts_matching_cargo_set(self):
        errors = self.run_case(
            {"a": ["read_file", "cargo_check"]},
            {"a": "e"},
            {"e": ([("cargo_check", "")], "FROM x\n")},
        )
        self.assertEqual(errors, [])

    def test_rejects_card_with_cargo_tool_env_lacks(self):
        errors = self.run_case(
            {"a": ["cargo_check", "cargo_build"]},
            {"a": "e"},
            {"e": ([("cargo_check", "")], "FROM x\n")},
        )
        self.assertTrue(any("cargo_build" in e for e in errors))

    def test_rejects_env_wider_than_card(self):
        errors = self.run_case(
            {"a": ["cargo_check"]},
            {"a": "e"},
            {"e": ([("cargo_check", ""), ("cargo_test", "")], "FROM x\n")},
        )
        self.assertTrue(any("cargo_test" in e for e in errors))

    def test_rejects_non_cargo_capability_the_card_does_not_list(self):
        errors = self.run_case(
            {"a": ["cargo_check"]},
            {"a": "e"},
            {"e": ([("cargo_check", ""), ("lean_check", "")], "FROM x\n")},
        )
        self.assertTrue(any("lean_check" in e for e in errors))

    def test_accepts_non_cargo_capability_the_card_lists(self):
        errors = self.run_case(
            {"a": ["cargo_check", "lean_check"]},
            {"a": "e"},
            {"e": ([("cargo_check", ""), ("lean_check", "")], "FROM x\n")},
        )
        self.assertEqual(errors, [])

    def test_rejects_available_key(self):
        errors = self.run_case(
            {"a": ["cargo_check"]},
            {"a": "e"},
            {"e": ([("cargo_check", "available = true\n")], "FROM x\n")},
        )
        self.assertTrue(any("available" in e for e in errors))

    def test_exec_is_optional_but_allowed(self):
        errors = self.run_case(
            {"a": ["cargo_check"]},
            {"a": "e"},
            {"e": ([("cargo_check", 'exec = ["cargo", "check"]\n')], "FROM x\n")},
        )
        self.assertEqual(errors, [])

    def test_cards_sharing_an_environment_need_identical_container_tools(self):
        errors = self.run_case(
            {"a": ["cargo_check", "cargo_test"], "b": ["cargo_check"]},
            {"a": "e", "b": "e"},
            {"e": ([("cargo_check", ""), ("cargo_test", "")], "FROM x\n")},
        )
        self.assertTrue(any(e.startswith("b:") and "cargo_test" in e for e in errors))

    def test_rejects_unmapped_card(self):
        errors = self.run_case(
            {"a": [], "b": []},
            {"a": "e"},
            {"e": ([], "FROM x\n")},
        )
        self.assertTrue(any("b" in e and "unmapped" in e for e in errors))

    def test_rejects_unknown_environment(self):
        errors = self.run_case({"a": []}, {"a": "nope"}, {"e": ([], "FROM x\n")})
        self.assertTrue(any("nope" in e for e in errors))

    def test_rejects_lean_toolchain_outside_lean_env(self):
        errors = self.run_case(
            {"a": []},
            {"a": "e"},
            {"e": ([], "FROM x\nRUN elan toolchain install stable\n")},
        )
        self.assertTrue(any("lean" in e.lower() for e in errors))

    def test_rejects_lean_env_shared_by_two_cards(self):
        errors = self.run_case(
            {"a": [], "b": []},
            {"a": "lean", "b": "lean"},
            {"lean": ([], "FROM x\nRUN elan toolchain install stable\n")},
        )
        self.assertTrue(any("lean" in e and "one card" in e for e in errors))

    def test_rejects_missing_containerfile(self):
        with tempfile.TemporaryDirectory() as d:
            tmp = pathlib.Path(d)
            make_repo(tmp, {"a": []}, {"a": "e"}, {"e": ([], "FROM x\n")})
            (tmp / ".nanna/environments/e/Containerfile").unlink()
            errors = validate.validate(tmp)
        self.assertTrue(any("Containerfile" in e for e in errors))

    def test_rejects_env_name_not_matching_directory(self):
        with tempfile.TemporaryDirectory() as d:
            tmp = pathlib.Path(d)
            make_repo(tmp, {"a": []}, {"a": "e"}, {"e": ([], "FROM x\n")})
            path = tmp / ".nanna/environments/e/environment.toml"
            path.write_text(path.read_text().replace('name = "e"', 'name = "z"'))
            errors = validate.validate(tmp)
        self.assertTrue(any("name" in e for e in errors))


if __name__ == "__main__":
    unittest.main()

import pathlib
import tempfile
import textwrap
import unittest

import validate

ROOT = pathlib.Path(__file__).resolve().parents[2]


def write(path, text):
    path.parent.mkdir(parents=True, exist_ok=True)
    path.write_text(textwrap.dedent(text))


def flake_text(bodies, bindings=""):
    entries = "".join(
        f"          {name} = {{\n            packages = {body};\n          }};\n"
        for name, body in bodies.items()
    )
    return (
        "{\n  outputs = { self }:\n    let\n"
        "        basePackages = [ pkgs.git ];\n"
        f"{bindings}"
        "        environments = {\n"
        f"{entries}"
        "        };\n"
        "        environmentImages = {};\n"
        "    in { };\n}\n"
    )


def make_repo(tmp, cards, mapping, envs, flake_output=None, flake=None):
    for name, tools in cards.items():
        tool_list = ", ".join(f'"{t}"' for t in tools)
        write(
            tmp / ".nanna/catalog" / f"{name}.toml",
            f'[identity]\nname = "{name}"\n[scope]\ntools = [{tool_list}]\n',
        )
    map_lines = "\n".join(f'{k} = "{v}"' for k, v in mapping.items())
    write(tmp / ".nanna/environments/cards.toml", f"[map]\n{map_lines}\n")
    bodies = {}
    for env, (caps, body) in envs.items():
        cap_text = "".join(
            f'\n[[capability]]\nid = "{c}"\ndescription = "d"\n{extra}'
            for c, extra in caps
        )
        output = flake_output(env) if flake_output else f"devContainerImage-{env}"
        write(
            tmp / ".nanna/environments" / env / "environment.toml",
            f'schema = 1\nname = "{env}"\n[build]\nflake_output = "{output}"\n{cap_text}',
        )
        bodies[env] = body
    write(tmp / "flake.nix", flake if flake is not None else flake_text(bodies))


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
            {"e": ([("cargo_check", "")], "basePackages")},
        )
        self.assertEqual(errors, [])

    def test_rejects_card_with_cargo_tool_env_lacks(self):
        errors = self.run_case(
            {"a": ["cargo_check", "cargo_build"]},
            {"a": "e"},
            {"e": ([("cargo_check", "")], "basePackages")},
        )
        self.assertTrue(any("cargo_build" in e for e in errors))

    def test_rejects_env_wider_than_card(self):
        errors = self.run_case(
            {"a": ["cargo_check"]},
            {"a": "e"},
            {"e": ([("cargo_check", ""), ("cargo_test", "")], "basePackages")},
        )
        self.assertTrue(any("cargo_test" in e for e in errors))

    def test_rejects_non_cargo_capability_the_card_does_not_list(self):
        errors = self.run_case(
            {"a": ["cargo_check"]},
            {"a": "e"},
            {"e": ([("cargo_check", ""), ("lean_check", "")], "basePackages")},
        )
        self.assertTrue(any("lean_check" in e for e in errors))

    def test_accepts_non_cargo_capability_the_card_lists(self):
        errors = self.run_case(
            {"a": ["cargo_check", "lean_check"]},
            {"a": "e"},
            {"e": ([("cargo_check", ""), ("lean_check", "")], "basePackages")},
        )
        self.assertEqual(errors, [])

    def test_rejects_available_key(self):
        errors = self.run_case(
            {"a": ["cargo_check"]},
            {"a": "e"},
            {"e": ([("cargo_check", "available = true\n")], "basePackages")},
        )
        self.assertTrue(any("available" in e for e in errors))

    def test_exec_is_optional_but_allowed(self):
        errors = self.run_case(
            {"a": ["cargo_check"]},
            {"a": "e"},
            {"e": ([("cargo_check", 'exec = ["cargo", "check"]\n')], "basePackages")},
        )
        self.assertEqual(errors, [])

    def test_cards_sharing_an_environment_need_identical_container_tools(self):
        errors = self.run_case(
            {"a": ["cargo_check", "cargo_test"], "b": ["cargo_check"]},
            {"a": "e", "b": "e"},
            {"e": ([("cargo_check", ""), ("cargo_test", "")], "basePackages")},
        )
        self.assertTrue(any(e.startswith("b:") and "cargo_test" in e for e in errors))

    def test_rejects_unmapped_card(self):
        errors = self.run_case(
            {"a": [], "b": []},
            {"a": "e"},
            {"e": ([], "basePackages")},
        )
        self.assertTrue(any("b" in e and "unmapped" in e for e in errors))

    def test_rejects_unknown_environment(self):
        errors = self.run_case({"a": []}, {"a": "nope"}, {"e": ([], "basePackages")})
        self.assertTrue(any("nope" in e for e in errors))

    def test_rejects_lean_package_in_non_lean_env(self):
        errors = self.run_case(
            {"a": []}, {"a": "e"}, {"e": ([], "basePackages ++ [ pkgs.lean4 ]")}
        )
        self.assertTrue(any("lean" in e.lower() for e in errors))

    def test_rejects_aeneas_charon_elan_in_non_lean_env(self):
        for marker in ("aeneas.packages.x.aeneas", "pkgs.charon", "pkgs.elan"):
            with self.subTest(marker=marker):
                errors = self.run_case(
                    {"a": []}, {"a": "e"}, {"e": ([], f"basePackages ++ [ {marker} ]")}
                )
                self.assertTrue(any("lean" in e.lower() for e in errors))

    def test_rejects_lean_reached_through_a_shared_binding(self):
        flake = flake_text(
            {"e": "tainted", "lean": "tainted"},
            bindings="        inner = [ pkgs.lean4 ];\n        tainted = basePackages ++ inner;\n",
        )
        with tempfile.TemporaryDirectory() as d:
            tmp = pathlib.Path(d)
            make_repo(
                tmp, {"a": [], "b": []}, {"a": "e", "b": "lean"},
                {"e": ([], "x"), "lean": ([], "x")}, flake=flake,
            )
            errors = validate.validate(tmp)
        self.assertTrue(any(e.startswith("e:") and "lean" in e for e in errors))
        self.assertFalse(any(e.startswith("lean:") and "toolchain" in e for e in errors))

    def test_accepts_lean_packages_in_lean_env(self):
        errors = self.run_case(
            {"a": []}, {"a": "lean"}, {"lean": ([], "basePackages ++ [ pkgs.lean4 ]")}
        )
        self.assertEqual(errors, [])

    def test_clean_is_not_a_lean_marker(self):
        errors = self.run_case(
            {"a": []}, {"a": "e"}, {"e": ([], "basePackages ++ [ pkgs.cleanup ]")}
        )
        self.assertEqual(errors, [])

    def test_rejects_lean_env_shared_by_two_cards(self):
        errors = self.run_case(
            {"a": [], "b": []},
            {"a": "lean", "b": "lean"},
            {"lean": ([], "basePackages ++ [ pkgs.lean4 ]")},
        )
        self.assertTrue(any("lean" in e and "one card" in e for e in errors))

    def test_rejects_missing_flake_output_definition(self):
        with tempfile.TemporaryDirectory() as d:
            tmp = pathlib.Path(d)
            make_repo(tmp, {"a": []}, {"a": "e"}, {"e": ([], "basePackages")},
                      flake=flake_text({"other": "basePackages"}))
            errors = validate.validate(tmp)
        self.assertTrue(any("devContainerImage-e" in e and "flake.nix" in e for e in errors))

    def test_rejects_missing_flake(self):
        with tempfile.TemporaryDirectory() as d:
            tmp = pathlib.Path(d)
            make_repo(tmp, {"a": []}, {"a": "e"}, {"e": ([], "basePackages")})
            (tmp / "flake.nix").unlink()
            errors = validate.validate(tmp)
        self.assertTrue(any("flake.nix" in e for e in errors))

    def test_rejects_containerfile_key(self):
        with tempfile.TemporaryDirectory() as d:
            tmp = pathlib.Path(d)
            make_repo(tmp, {"a": []}, {"a": "e"}, {"e": ([], "basePackages")})
            path = tmp / ".nanna/environments/e/environment.toml"
            path.write_text(path.read_text().replace(
                'flake_output = "devContainerImage-e"', 'containerfile = "Containerfile"'))
            errors = validate.validate(tmp)
        self.assertTrue(any("containerfile" in e and "flake_output" in e for e in errors))

    def test_rejects_containerfile_alongside_flake_output(self):
        with tempfile.TemporaryDirectory() as d:
            tmp = pathlib.Path(d)
            make_repo(tmp, {"a": []}, {"a": "e"}, {"e": ([], "basePackages")})
            path = tmp / ".nanna/environments/e/environment.toml"
            path.write_text(path.read_text().replace(
                "[build]\n", '[build]\ncontainerfile = "Containerfile"\n'))
            errors = validate.validate(tmp)
        self.assertTrue(any("containerfile" in e for e in errors))

    def test_rejects_missing_flake_output_key(self):
        with tempfile.TemporaryDirectory() as d:
            tmp = pathlib.Path(d)
            make_repo(tmp, {"a": []}, {"a": "e"}, {"e": ([], "basePackages")})
            path = tmp / ".nanna/environments/e/environment.toml"
            path.write_text(path.read_text().replace(
                'flake_output = "devContainerImage-e"', ""))
            errors = validate.validate(tmp)
        self.assertTrue(any("flake_output" in e for e in errors))

    def test_rejects_flake_output_with_bad_charset(self):
        for bad in ("devContainerImage-E", "devContainerImage-", "devContainerImage--x",
                    "devContainerImage-a b", "other-e", "devContainerImage-e;rm"):
            with self.subTest(bad=bad):
                with tempfile.TemporaryDirectory() as d:
                    tmp = pathlib.Path(d)
                    make_repo(tmp, {"a": []}, {"a": "e"}, {"e": ([], "basePackages")})
                    path = tmp / ".nanna/environments/e/environment.toml"
                    path.write_text(path.read_text().replace("devContainerImage-e", bad))
                    errors = validate.validate(tmp)
                self.assertTrue(any("flake_output" in e for e in errors))

    def test_rejects_flake_output_not_matching_directory(self):
        with tempfile.TemporaryDirectory() as d:
            tmp = pathlib.Path(d)
            make_repo(tmp, {"a": []}, {"a": "e"}, {"e": ([], "basePackages")},
                      flake=flake_text({"e": "basePackages", "z": "basePackages"}))
            path = tmp / ".nanna/environments/e/environment.toml"
            path.write_text(path.read_text().replace("devContainerImage-e", "devContainerImage-z"))
            errors = validate.validate(tmp)
        self.assertTrue(any("devContainerImage-e" in e for e in errors))

    def test_rejects_env_name_not_matching_directory(self):
        with tempfile.TemporaryDirectory() as d:
            tmp = pathlib.Path(d)
            make_repo(tmp, {"a": []}, {"a": "e"}, {"e": ([], "basePackages")})
            path = tmp / ".nanna/environments/e/environment.toml"
            path.write_text(path.read_text().replace('name = "e"', 'name = "z"'))
            errors = validate.validate(tmp)
        self.assertTrue(any("name" in e for e in errors))


if __name__ == "__main__":
    unittest.main()

import contextlib
import io
import json
import os
import pathlib
import stat
import tempfile
import textwrap
import tomllib
import unittest
from unittest import mock

import validate

ROOT = pathlib.Path(__file__).resolve().parents[2]
LEAN_BODY = "basePackages ++ [ pkgs.lean4 pkgs.aeneas ]"
SHIM = "#!/bin/sh\nprintf '%s\\n' \"$*\" > \"$SHIM_LOG\"\nprintf '%s' \"$SHIM_OUT\"\nexit \"${SHIM_RC:-0}\"\n"


class NoNix(unittest.TestCase):
    def setUp(self):
        self._empty = tempfile.TemporaryDirectory()
        patcher = mock.patch.dict(os.environ, {"PATH": self._empty.name})
        patcher.start()
        self.addCleanup(patcher.stop)
        self.addCleanup(self._empty.cleanup)


def write(path, text):
    path.parent.mkdir(parents=True, exist_ok=True)
    path.write_text(textwrap.dedent(text))


def flake_text(bodies, bindings="", images="{}"):
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
        f"        environmentImages = {images};\n"
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
        with contextlib.redirect_stderr(io.StringIO()):
            self.assertEqual(validate.validate(ROOT), [])

    def test_repository_is_valid_under_text_fallback(self):
        with tempfile.TemporaryDirectory() as empty:
            with mock.patch.dict(os.environ, {"PATH": empty}):
                err = io.StringIO()
                with contextlib.redirect_stderr(err):
                    self.assertEqual(validate.validate(ROOT), [])
        self.assertIn("text fallback", err.getvalue())

    def test_inchworm_flows_and_protected_list_cover_image_definitions(self):
        required = {"flake.nix", "flake.lock", ".nanna/environments/**"}
        config = tomllib.loads((ROOT / "inchworm.toml").read_text())
        for flow in config["flow"]:
            lists = [j["forbid_paths"] for j in flow.get("judge", []) if "forbid_paths" in j]
            with self.subTest(flow=flow["name"]):
                self.assertTrue(lists)
                for forbidden in lists:
                    self.assertLessEqual(required, set(forbidden))
        protected = tomllib.loads((ROOT / ".nanna/protected.toml").read_text())
        self.assertLessEqual(
            required | {".devcontainer/**"}, set(protected["extra"])
        )


def install_shim(directory):
    path = pathlib.Path(directory) / "nix"
    path.write_text(SHIM)
    path.chmod(path.stat().st_mode | stat.S_IEXEC)


class NixManifest(unittest.TestCase):
    def run_case(self, manifest, rc=0, out=None, envs=None, flake=None):
        envs = envs or {"e": ([], "basePackages")}
        cards = {"a": []}
        with tempfile.TemporaryDirectory() as d, tempfile.TemporaryDirectory() as bin_dir:
            tmp = pathlib.Path(d)
            install_shim(bin_dir)
            log = pathlib.Path(bin_dir) / "log"
            make_repo(tmp, cards, {"a": next(iter(envs))}, envs, flake=flake)
            payload = json.dumps(manifest) if out is None else out
            env = {"PATH": bin_dir, "SHIM_LOG": str(log), "SHIM_OUT": payload, "SHIM_RC": str(rc)}
            with mock.patch.dict(os.environ, env):
                errors = validate.validate(tmp)
            self.args = log.read_text() if log.exists() else ""
        return errors

    def test_accepts_clean_manifest_without_reading_flake_text(self):
        flake = "{ not parseable nix"
        errors = self.run_case({"e": ["git-2", "coreutils-9"]}, flake=flake)
        self.assertEqual(errors, [])

    def test_asks_for_the_manifest_attribute_of_the_flake(self):
        self.run_case({"e": ["git-2"]})
        self.assertIn("eval --json", self.args)
        self.assertIn("lib.", self.args)
        self.assertIn("environmentManifest", self.args)

    def test_rejects_lean_in_non_lean_environment(self):
        errors = self.run_case({"e": ["git-2", "lean4-4.35.0"]})
        self.assertTrue(any(e.startswith("e:") and "lean" in e for e in errors))

    def test_rejects_each_forbidden_marker_in_non_lean_environment(self):
        for pkg in ("lean4-mathlib-1", "batteries-1", "fstar-2", "hol4-3",
                    "ocaml5-aeneas-0.1", "charon-1", "elan-2", "lake-1"):
            with self.subTest(pkg=pkg):
                errors = self.run_case({"e": ["git-2", pkg]})
                self.assertTrue(any(e.startswith("e:") for e in errors))

    def test_accepts_lean_environment_with_lean_and_aeneas(self):
        errors = self.run_case(
            {"lean": ["git-2", "lean4-4.35.0", "ocaml5-aeneas-0.1"]},
            envs={"lean": ([], "x")},
        )
        self.assertEqual(errors, [])

    def test_rejects_lean_environment_missing_lean_or_aeneas(self):
        for names in (["git-2", "ocaml5-aeneas-0.1"], ["git-2", "lean4-4.35.0"], ["git-2"]):
            with self.subTest(names=names):
                errors = self.run_case({"lean": names}, envs={"lean": ([], "x")})
                self.assertTrue(any(e.startswith("lean:") and "must contain" in e for e in errors))

    def test_rejects_environment_missing_from_manifest(self):
        errors = self.run_case({"other": ["git-2"]})
        self.assertTrue(any("devContainerImage-e" in e for e in errors))

    def test_nix_failure_is_an_error_not_a_fallback(self):
        errors = self.run_case({}, rc=1, out="")
        self.assertTrue(any("nix eval" in e and "failed" in e for e in errors))

    def test_invalid_json_is_an_error(self):
        errors = self.run_case({}, out="not json")
        self.assertTrue(any("invalid JSON" in e for e in errors))

    def test_wrong_manifest_shape_is_an_error(self):
        errors = self.run_case({"e": "git-2"})
        self.assertTrue(any("lists of package names" in e for e in errors))


class TextFallback(NoNix):
    def run_flake(self, flake, envs=None, mapping=None):
        envs = envs or {"e": ([], "x")}
        with tempfile.TemporaryDirectory() as d:
            tmp = pathlib.Path(d)
            make_repo(tmp, {"a": []}, mapping or {"a": next(iter(envs))}, envs, flake=flake)
            err = io.StringIO()
            with contextlib.redirect_stderr(err):
                errors = validate.validate(tmp)
        self.warning = err.getvalue()
        return errors

    def test_prints_a_text_fallback_warning(self):
        self.run_flake(flake_text({"e": "basePackages"}))
        self.assertIn("text fallback", self.warning)

    def test_rejects_environment_images_injection(self):
        images = (
            "mapAttrs' (n: env: mkImage { packages = env.packages ++ [ pkgs.lean4 ]; }) "
            "environments"
        )
        errors = self.run_flake(flake_text({"e": "basePackages"}, images=images))
        self.assertTrue(any(e.startswith("e:") and "lean" in e for e in errors))

    def test_rejects_lean_missing_from_lean_environment(self):
        errors = self.run_flake(
            flake_text({"lean": "basePackages"}), envs={"lean": ([], "x")}
        )
        self.assertTrue(any(e.startswith("lean:") and "must contain" in e for e in errors))

    def test_accepts_lean_environment_with_both_markers(self):
        errors = self.run_flake(
            flake_text({"lean": LEAN_BODY}), envs={"lean": ([], "x")}
        )
        self.assertEqual(errors, [])

    def test_extra_markers_are_forbidden_elsewhere(self):
        for pkg in ("pkgs.mathlib", "pkgs.batteries", "pkgs.fstar", "pkgs.hol4", "pkgs.lake"):
            with self.subTest(pkg=pkg):
                errors = self.run_flake(flake_text({"e": f"basePackages ++ [ {pkg} ]"}))
                self.assertTrue(any(e.startswith("e:") for e in errors))

    def test_comment_mentioning_lean_is_not_a_false_positive(self):
        flake = flake_text(
            {"e": "basePackages # lean4 is only in the lean environment\n"},
            bindings="        # aeneas and charon live elsewhere\n        /* lake */\n",
        )
        self.assertEqual(self.run_flake(flake), [])

    def test_string_mentioning_lean_is_not_a_false_positive(self):
        flake = flake_text(
            {"e": "basePackages"},
            bindings='        label = "lean-free";\n        other = \'\'aeneas\'\';\n',
        )
        self.assertEqual(self.run_flake(flake), [])

    def test_rejects_obfuscated_attribute_access(self):
        for body in (
            'basePackages ++ [ pkgs."lean4" ]',
            'basePackages ++ [ pkgs.${"lean" + "4"} ]',
            'basePackages ++ [ (builtins.getAttr "lean4" pkgs) ]',
            'basePackages ++ [ (lib.attrByPath [ "lean4" ] null pkgs) ]',
        ):
            with self.subTest(body=body):
                errors = self.run_flake(flake_text({"e": body}))
                self.assertTrue(any(e.startswith("e:") for e in errors))

    def test_rejects_lean_reached_through_a_shared_binding(self):
        flake = flake_text(
            {"e": "tainted", "lean": "tainted"},
            bindings="        inner = [ pkgs.lean4 pkgs.aeneas ];\n        tainted = basePackages ++ inner;\n",
        )
        errors = self.run_shared(flake)
        self.assertTrue(any(e.startswith("e:") and "lean" in e for e in errors))
        self.assertFalse(any(e.startswith("lean:") for e in errors))

    def run_shared(self, flake):
        with tempfile.TemporaryDirectory() as d:
            tmp = pathlib.Path(d)
            make_repo(
                tmp, {"a": [], "b": []}, {"a": "e", "b": "lean"},
                {"e": ([], "x"), "lean": ([], "x")}, flake=flake,
            )
            with contextlib.redirect_stderr(io.StringIO()):
                return validate.validate(tmp)

    def test_parse_does_not_depend_on_indentation(self):
        flake = (
            "{ outputs = {self}: let basePackages = [ pkgs.git ];\n"
            "\tenvironments = { e = { packages = basePackages; }; "
            "lean = { packages = [ pkgs.lean4 pkgs.aeneas ]; }; };\n"
            "environmentImages = {}; in {}; }"
        )
        errors = self.run_shared(flake)
        self.assertEqual(errors, [])

    def test_unparseable_flake_fails_closed(self):
        for flake in ("{ outputs = {}; }", "not nix at all", "{ environments = { }; }"):
            with self.subTest(flake=flake):
                errors = self.run_flake(flake)
                self.assertTrue(any("text fallback cannot parse" in e for e in errors))

    def test_unparseable_environment_name_fails_closed(self):
        flake = (
            "{ outputs = let environments = { \"e\" = { packages = [ ]; }; }; in {}; }"
        )
        errors = self.run_flake(flake)
        self.assertTrue(any("text fallback cannot parse" in e for e in errors))


class Rules(NoNix):
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
        errors = self.run_case({"a": []}, {"a": "lean"}, {"lean": ([], LEAN_BODY)})
        self.assertEqual(errors, [])

    def test_rejects_unknown_top_level_key(self):
        errors = self.edit_declaration("schema = 1\n", 'schema = 1\nextra = "x"\n')
        self.assertTrue(any("extra" in e and "unsupported" in e for e in errors))

    def test_rejects_unknown_build_key(self):
        errors = self.edit_declaration("[build]\n", '[build]\nimage = "x"\n')
        self.assertTrue(any("image" in e and "unsupported" in e for e in errors))

    def test_rejects_containerfile_and_flake_output_together(self):
        errors = self.edit_declaration("[build]\n", '[build]\ncontainerfile = "Containerfile"\n')
        self.assertTrue(any("containerfile" in e and "flake_output" in e for e in errors))

    def edit_declaration(self, old, new):
        with tempfile.TemporaryDirectory() as d:
            tmp = pathlib.Path(d)
            make_repo(tmp, {"a": []}, {"a": "e"}, {"e": ([], "basePackages")})
            path = tmp / ".nanna/environments/e/environment.toml"
            path.write_text(path.read_text().replace(old, new, 1))
            return validate.validate(tmp)

    def test_clean_is_not_a_lean_marker(self):
        errors = self.run_case(
            {"a": []}, {"a": "e"}, {"e": ([], "basePackages ++ [ pkgs.cleanup ]")}
        )
        self.assertEqual(errors, [])

    def test_rejects_lean_env_shared_by_two_cards(self):
        errors = self.run_case(
            {"a": [], "b": []},
            {"a": "lean", "b": "lean"},
            {"lean": ([], LEAN_BODY)},
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

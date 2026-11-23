import copy
import pathlib
import re
import tomllib
import unittest

ROOT = pathlib.Path(__file__).resolve().parents[2]
CATALOG = ROOT / ".nanna" / "catalog"
AGENTS = ROOT / ".nanna" / "agents"
SUBSET_KEYS = ("tools", "paths", "read_paths", "repos")
KNOWN_SCOPE = set(SUBSET_KEYS) | {"max_effect"}
LEGACY_FLOWS = {"issue", "test-design", "close-coverage", "prove"}
IMPLEMENTATION_FLOWS = {"issue", "implement-bugfix", "implement-feature"}
RETRY_ON = {"agent_failed", "tests_failed", "judge_rejected"}
PRODUCTION_SAMPLES = ("src/lib.rs", "benches/b.rs", "build.rs")
TEST_DESIGNERS = (
    "marigold-proptest-designer",
    "marigold-e2e-designer",
    "marigold-formal-methods-designer",
)


def load(path):
    with open(path, "rb") as f:
        return tomllib.load(f)


def cards(directory):
    return {p.stem: load(p) for p in sorted(directory.glob("*.toml"))}


def glob_regex(pattern):
    out = []
    i = 0
    while i < len(pattern):
        if pattern.startswith("**/", i):
            out.append("(?:.*/)?")
            i += 3
        elif pattern.startswith("**", i):
            out.append(".*")
            i += 2
        elif pattern[i] == "*":
            out.append("[^/]*")
            i += 1
        elif pattern[i] == "?":
            out.append("[^/]")
            i += 1
        else:
            out.append(re.escape(pattern[i]))
            i += 1
    return re.compile("".join(out) + r"\Z")


def matches(path, patterns):
    return any(glob_regex(p).match(path) for p in patterns)


def crate_roots(paths):
    roots = set()
    for p in paths:
        first = p.split("/")[0]
        if first.startswith("marigold") and not any(c in first for c in "*?"):
            roots.add(first)
    return sorted(roots)


def production_samples(paths):
    return [f"{r}/{s}" for r in crate_roots(paths) for s in PRODUCTION_SAMPLES]


def manifest_samples(paths):
    samples = []
    for p in paths:
        first = p.split("/")[0]
        if not any(c in first for c in "*?"):
            samples.append(f"{first}/Cargo.toml")
    return samples


def check_narrowing(name, base, card):
    problems = []
    for key in SUBSET_KEYS:
        extra = set(card["scope"][key]) - set(base["scope"][key])
        if extra:
            problems.append(f"{name}: {key} widens base")
    if card["scope"]["max_effect"] != base["scope"]["max_effect"]:
        problems.append(f"{name}: max_effect differs from base")
    for key, value in card["scope"].items():
        if key in KNOWN_SCOPE:
            continue
        base_value = base["scope"].get(key)
        if isinstance(value, list):
            if base_value is None or set(value) - set(base_value):
                problems.append(f"{name}: {key} widens base")
        elif value != base_value:
            problems.append(f"{name}: {key} differs from base")
    for key, value in card["limits"].items():
        if value > base["limits"][key]:
            problems.append(f"{name}: {key} above base")
    return problems


def check_write_consistency(table):
    problems = []
    for name, card in table.items():
        scope = card["scope"]
        writes = "write_file" in scope["tools"]
        if writes or scope["paths"]:
            if scope["max_effect"] == "none":
                problems.append(f"{name}: writes with ceiling none")
            if not (writes and scope["paths"]):
                problems.append(f"{name}: write tool and write paths disagree")
        elif name.startswith("marigold-") and scope["max_effect"] != "none":
            problems.append(f"{name}: read-only card without ceiling none")
    return problems


def check_flows(config, catalog, agents):
    problems = []
    flows = config["flow"]
    names = [f["name"] for f in flows]
    labels = [f["trigger"]["label"].lower() for f in flows]
    if len(names) != len(set(names)):
        problems.append("duplicate flow names")
    if len(labels) != len(set(labels)):
        problems.append("duplicate flow labels")
    for flow in flows:
        fname = flow["name"]
        steps = [s["name"] for s in flow["step"]]
        if len(steps) != len(set(steps)):
            problems.append(f"{fname}: duplicate step names")
        for step in flow["step"]:
            where = f"{fname}/{step['name']}"
            if step["identity"] not in catalog:
                problems.append(f"{where}: identity not in catalog")
            if fname not in LEGACY_FLOWS and step["identity"] not in agents:
                problems.append(f"{where}: identity has no agents card")
        for step in flow["step"][:-1]:
            card = catalog.get(step["identity"])
            if card is not None and not card["scope"]["paths"]:
                problems.append(f"{fname}/{step['name']}: non-final step writes nothing")
        if not set(flow.get("retry_on", [])) <= RETRY_ON:
            problems.append(f"{fname}: bad retry_on")
        if flow["outcome"].get("blocking") != "required":
            problems.append(f"{fname}: blocking is not required")
        forbid = []
        for judge in flow.get("judge", []):
            if judge["kind"] == "diff":
                if not (isinstance(judge["max_lines"], int) and judge["max_lines"] > 0):
                    problems.append(f"{fname}: max_lines must be positive")
                forbid = judge["forbid_paths"]
        for step in flow["step"]:
            card = catalog.get(step["identity"])
            if card is None:
                continue
            paths = card["scope"]["paths"]
            where = f"{fname}/{step['name']}"
            if fname == "issue":
                continue
            samples = manifest_samples(paths)
            if fname not in IMPLEMENTATION_FLOWS:
                samples += production_samples(paths)
            for sample in samples:
                if matches(sample, paths) and not matches(sample, forbid):
                    problems.append(f"{where}: may write {sample} which the flow allows")
    return problems


def check_test_designer_paths(catalog):
    problems = []
    for name in TEST_DESIGNERS:
        paths = catalog[name]["scope"]["paths"]
        for sample in production_samples(paths):
            if matches(sample, paths):
                problems.append(f"{name}: may write production file {sample}")
    return problems


def check_research(config, catalog, agents):
    problems = []
    for flow in config["flow"]:
        if flow["name"].startswith("research-"):
            if flow["step"][0]["identity"] != "marigold-research-lead":
                problems.append(f"{flow['name']}: does not plan with the research lead")
    for name, card in catalog.items():
        if name.startswith("marigold-research-") and name != "marigold-research-lead":
            area = name.removeprefix("marigold-research-")
            if card["scope"]["paths"] != [f"docs/research/{area}.txt"]:
                problems.append(f"{name}: wrong write path")
            if "cargo_check" in card["scope"]["tools"]:
                problems.append(f"{name}: has a cargo tool")
            if agents[name]["scope"]["read_paths"] != card["scope"]["read_paths"]:
                problems.append(f"{name}: narrowing drops read paths of its area")
    return problems


def load_all():
    return cards(CATALOG), cards(AGENTS), load(ROOT / "inchworm.toml")


class RealTree(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        cls.catalog, cls.agents, cls.config = load_all()

    def test_every_agent_card_has_a_catalog_base(self):
        for name in self.agents:
            self.assertIn(name, self.catalog, name)

    def test_names_match_file_names(self):
        for table in (self.catalog, self.agents):
            for stem, card in table.items():
                self.assertEqual(card["identity"]["name"], stem)

    def test_agent_cards_narrow_their_base(self):
        problems = []
        for name, card in self.agents.items():
            problems += check_narrowing(name, self.catalog[name], card)
        self.assertEqual(problems, [])

    def test_write_tools_and_paths_agree(self):
        for table in (self.catalog, self.agents):
            self.assertEqual(check_write_consistency(table), [])

    def test_system_prompt_files_exist(self):
        for name, card in self.catalog.items():
            prompt = card["identity"]["system_prompt"]
            self.assertIsInstance(prompt, str, name)
            self.assertTrue((CATALOG / prompt).is_file(), f"{name}: {prompt}")

    def test_flows_are_consistent(self):
        self.assertEqual(check_flows(self.config, self.catalog, self.agents), [])

    def test_test_designers_write_only_test_locations(self):
        self.assertEqual(check_test_designer_paths(self.catalog), [])

    def test_research_cards_and_flows(self):
        self.assertEqual(check_research(self.config, self.catalog, self.agents), [])


class Mutations(unittest.TestCase):
    def setUp(self):
        catalog, agents, config = load_all()
        self.catalog = copy.deepcopy(catalog)
        self.agents = copy.deepcopy(agents)
        self.config = copy.deepcopy(config)

    def flow(self, name):
        return next(f for f in self.config["flow"] if f["name"] == name)

    def diff_judge(self, name):
        return next(j for j in self.flow(name)["judge"] if j["kind"] == "diff")

    def flows_problems(self):
        return check_flows(self.config, self.catalog, self.agents)

    def test_widened_list_is_flagged(self):
        name = "marigold-review-scope"
        self.agents[name]["scope"]["tools"].append("cargo_test")
        self.assertTrue(check_narrowing(name, self.catalog[name], self.agents[name]))

    def test_different_ceiling_is_flagged(self):
        name = "marigold-test-lead"
        self.agents[name]["scope"]["max_effect"] = "none"
        self.assertTrue(check_narrowing(name, self.catalog[name], self.agents[name]))

    def test_limit_above_base_is_flagged(self):
        name = "marigold-test-lead"
        self.agents[name]["limits"]["max_iterations"] = 10**6
        self.assertTrue(check_narrowing(name, self.catalog[name], self.agents[name]))

    def test_unknown_scope_key_widening_is_flagged(self):
        name = "marigold-test-lead"
        self.catalog[name]["scope"]["message_reach"] = ["a"]
        self.agents[name]["scope"]["message_reach"] = ["a", "b"]
        self.assertTrue(check_narrowing(name, self.catalog[name], self.agents[name]))
        self.agents[name]["scope"]["message_reach"] = ["a"]
        self.assertEqual(check_narrowing(name, self.catalog[name], self.agents[name]), [])

    def test_write_tool_without_paths_is_flagged(self):
        self.catalog["marigold-test-lead"]["scope"]["paths"] = []
        self.assertTrue(check_write_consistency(self.catalog))

    def test_read_only_card_with_ceiling_is_flagged(self):
        self.catalog["marigold-review-scope"]["scope"]["max_effect"] = "workspace"
        self.assertTrue(check_write_consistency(self.catalog))

    def test_unknown_identity_is_flagged(self):
        self.flow("test-lead-e2e")["step"][0]["identity"] = "nope"
        self.assertTrue(self.flows_problems())

    def test_missing_agents_card_is_flagged(self):
        del self.agents["marigold-test-lead"]
        self.assertTrue(self.flows_problems())

    def test_label_collision_ignores_case(self):
        a, b = self.config["flow"][1], self.config["flow"][2]
        b["trigger"]["label"] = a["trigger"]["label"].upper()
        self.assertTrue(self.flows_problems())

    def test_duplicate_flow_name_is_flagged(self):
        self.config["flow"][2]["name"] = self.config["flow"][1]["name"]
        self.assertTrue(self.flows_problems())

    def test_duplicate_step_name_is_flagged(self):
        flow = self.flow("test-lead-e2e")
        flow["step"][1]["name"] = flow["step"][0]["name"]
        self.assertTrue(self.flows_problems())

    def test_bad_blocking_is_flagged(self):
        self.flow("test-lead-e2e")["outcome"]["blocking"] = "maybe"
        self.assertTrue(self.flows_problems())

    def test_bad_retry_on_is_flagged(self):
        self.flow("test-lead-e2e")["retry_on"] = ["whenever"]
        self.assertTrue(self.flows_problems())

    def test_nonpositive_max_lines_is_flagged(self):
        self.diff_judge("test-lead-e2e")["max_lines"] = 0
        self.assertTrue(self.flows_problems())

    def test_missing_source_forbid_is_flagged(self):
        judge = self.diff_judge("test-lead-proptest")
        judge["forbid_paths"] = [p for p in judge["forbid_paths"] if "src" not in p]
        paths = ["marigold-impl/**"]
        self.catalog["marigold-proptest-designer"]["scope"]["paths"] = paths
        self.assertTrue(self.flows_problems())

    def test_missing_manifest_forbid_is_flagged(self):
        judge = self.diff_judge("implement-bugfix")
        judge["forbid_paths"] = [p for p in judge["forbid_paths"] if "Cargo" not in p]
        self.assertTrue(self.flows_problems())

    def test_non_final_step_that_writes_nothing_is_flagged(self):
        self.catalog["marigold-test-lead"]["scope"]["paths"] = []
        self.assertTrue(self.flows_problems())

    def test_test_designer_with_source_path_is_flagged(self):
        self.catalog["marigold-e2e-designer"]["scope"]["paths"] = ["marigold-impl/**"]
        self.assertTrue(check_test_designer_paths(self.catalog))

    def test_research_wrong_write_path_is_flagged(self):
        self.catalog["marigold-research-docs"]["scope"]["paths"] = ["docs/**"]
        self.assertTrue(check_research(self.config, self.catalog, self.agents))

    def test_research_cargo_tool_is_flagged(self):
        self.catalog["marigold-research-docs"]["scope"]["tools"].append("cargo_check")
        self.assertTrue(check_research(self.config, self.catalog, self.agents))

    def test_research_narrowing_dropping_read_paths_is_flagged(self):
        self.agents["marigold-research-docs"]["scope"]["read_paths"] = ["docs/**"]
        self.assertTrue(check_research(self.config, self.catalog, self.agents))

    def test_research_flow_with_wrong_lead_is_flagged(self):
        self.flow("research-docs")["step"][0]["identity"] = "marigold-implementer-lead"
        self.assertTrue(check_research(self.config, self.catalog, self.agents))


class GlobSemantics(unittest.TestCase):
    def test_double_star_prefix_matches_root_and_nested(self):
        self.assertTrue(matches("Cargo.toml", ["**/Cargo.toml"]))
        self.assertTrue(matches("a/b/Cargo.toml", ["**/Cargo.toml"]))
        self.assertFalse(matches("a/Cargo.lock", ["**/Cargo.toml"]))

    def test_single_star_stays_in_one_segment(self):
        self.assertTrue(matches("marigold-impl/src/a/b.rs", ["marigold-*/src/**"]))
        self.assertFalse(matches("marigold-impl/tests/src/a.rs", ["marigold-*/src/**"]))


if __name__ == "__main__":
    unittest.main()

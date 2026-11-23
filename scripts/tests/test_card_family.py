import pathlib
import tomllib
import unittest

ROOT = pathlib.Path(__file__).resolve().parents[2]
CATALOG = ROOT / ".nanna" / "catalog"
AGENTS = ROOT / ".nanna" / "agents"
SUBSET_KEYS = ("tools", "paths", "read_paths", "repos")


def load(path):
    with open(path, "rb") as f:
        return tomllib.load(f)


def cards(directory):
    return {p.stem: load(p) for p in sorted(directory.glob("*.toml"))}


class CardFamily(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        cls.catalog = cards(CATALOG)
        cls.agents = cards(AGENTS)
        cls.config = load(ROOT / "inchworm.toml")

    def test_every_agent_card_has_a_catalog_base(self):
        for name in self.agents:
            self.assertIn(name, self.catalog, name)

    def test_names_match_file_names(self):
        for table in (self.catalog, self.agents):
            for stem, card in table.items():
                self.assertEqual(card["identity"]["name"], stem)

    def test_agent_cards_narrow_their_base(self):
        for name, card in self.agents.items():
            base = self.catalog[name]
            for key in SUBSET_KEYS:
                extra = set(card["scope"][key]) - set(base["scope"][key])
                self.assertEqual(extra, set(), f"{name}: {key} widens base")
            self.assertEqual(
                card["scope"]["max_effect"], base["scope"]["max_effect"], name
            )
            for key, value in card["limits"].items():
                self.assertLessEqual(value, base["limits"][key], f"{name}: {key}")

    def test_cards_that_write_files_have_a_workspace_ceiling(self):
        for table in (self.catalog, self.agents):
            for name, card in table.items():
                scope = card["scope"]
                writes = "write_file" in scope["tools"]
                if writes or scope["paths"]:
                    self.assertNotEqual(scope["max_effect"], "none", name)
                    self.assertTrue(writes and scope["paths"], name)
                elif name.startswith("marigold-"):
                    self.assertEqual(scope["max_effect"], "none", name)

    def test_system_prompt_files_exist(self):
        for name, card in self.catalog.items():
            prompt = card["identity"]["system_prompt"]
            self.assertIsInstance(prompt, str, name)
            self.assertTrue((CATALOG / prompt).is_file(), f"{name}: {prompt}")

    def test_flow_identities_exist_in_the_catalog_and_agents(self):
        for flow in self.config["flow"]:
            for step in flow["step"]:
                where = f"{flow['name']}/{step['name']}"
                self.assertIn(step["identity"], self.catalog, where)

    def test_flow_names_labels_and_step_names_are_unique(self):
        flows = self.config["flow"]
        names = [f["name"] for f in flows]
        labels = [f["trigger"]["label"] for f in flows]
        self.assertEqual(len(names), len(set(names)))
        self.assertEqual(len(labels), len(set(labels)))
        for flow in flows:
            steps = [s["name"] for s in flow["step"]]
            self.assertEqual(len(steps), len(set(steps)), flow["name"])

    def test_every_non_final_step_can_write_files(self):
        for flow in self.config["flow"]:
            for step in flow["step"][:-1]:
                card = self.catalog[step["identity"]]
                where = f"{flow['name']}/{step['name']}"
                self.assertTrue(card["scope"]["paths"], where)

    def test_research_flows_plan_with_the_research_lead(self):
        for flow in self.config["flow"]:
            if flow["name"].startswith("research-"):
                first = flow["step"][0]["identity"]
                self.assertEqual(first, "marigold-research-lead", flow["name"])

    def test_researchers_write_only_their_findings_file(self):
        for name, card in self.catalog.items():
            if name.startswith("marigold-research-") and name != "marigold-research-lead":
                area = name.removeprefix("marigold-research-")
                self.assertEqual(card["scope"]["paths"], [f"docs/research/{area}.txt"], name)
                self.assertNotIn("cargo_check", card["scope"]["tools"], name)


if __name__ == "__main__":
    unittest.main()

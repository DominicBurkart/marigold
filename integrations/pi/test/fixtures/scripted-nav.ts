import { fauxAssistantMessage, fauxProvider, fauxToolCall } from "@earendil-works/pi-ai";
import type { ExtensionAPI } from "@earendil-works/pi-coding-agent";

export default function (pi: ExtensionAPI) {
  const faux = fauxProvider({ provider: "scripted", models: [{ id: "navigator" }] });
  faux.setResponses([
    fauxAssistantMessage(
      fauxToolCall("write", {
        path: "nav.marigold",
        content: "fn double(x: i32) -> i32 { x * 2 }\nrange(0, 3).map(double).return\n",
      }),
      { stopReason: "toolUse" },
    ),
    fauxAssistantMessage(fauxToolCall("marigold_definition", { path: "nav.marigold", line: 2, column: 17 }), {
      stopReason: "toolUse",
    }),
    fauxAssistantMessage("done"),
  ]);
  pi.registerProvider(faux.provider);
}

import { fauxAssistantMessage, fauxProvider, fauxToolCall } from "@earendil-works/pi-ai";
import type { ExtensionAPI } from "@earendil-works/pi-coding-agent";

export default function (pi: ExtensionAPI) {
  const faux = fauxProvider({ provider: "scripted", models: [{ id: "writer" }] });
  faux.setResponses([
    fauxAssistantMessage(fauxToolCall("write", { path: "broken.marigold", content: "range(Colour).return\n" }), {
      stopReason: "toolUse",
    }),
    fauxAssistantMessage("done"),
  ]);
  pi.registerProvider(faux.provider);
}

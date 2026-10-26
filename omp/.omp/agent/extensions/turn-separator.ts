/**
 * Draw a dim rule between assistant text blocks separated by tool calls.
 *
 * With `display.hideToolActivity` on, text written before and after a tool
 * batch renders as one continuous message. After each assistant message that
 * both says something and calls tools, this queues a display-only custom
 * message as an `aside`: it lands at the next step boundary, i.e. after the
 * tool batch and before the next model response, which is exactly where the
 * hidden tools would have been. A `context` filter strips these markers so the
 * model never sees them.
 *
 * Batches containing `wait` are skipped: a pending aside ends a running wait.
 */

const TYPE = "turn-separator";
const WAIT_TOOLS: Record<string, true> = { wait: true, vibe_wait: true };

type Block = { type: string; text?: string; name?: string };
type Message = { role: string; customType?: string; content?: unknown };
type Theme = { fg(color: string, text: string): string };
type Component = { render(width: number): string[]; invalidate(): void };
type ExtensionHost = {
  setLabel(label: string): void;
  on(
    event: "message_end",
    handler: (event: { message: Message }, ctx: { hasUI: boolean; agent: { kind: string } }) => void,
  ): void;
  on(event: "context", handler: (event: { messages: Message[] }) => { messages: Message[] }): void;
  sendMessage(
    message: { customType: string; content: string; display: boolean },
    options: { deliverAs: "aside" },
  ): void;
  registerMessageRenderer(
    type: string,
    render: (message: Message, options: { expanded: boolean }, theme: Theme) => Component,
  ): void;
};

function shouldSeparate(message: Message): boolean {
  if (message.role !== "assistant" || !Array.isArray(message.content)) return false;
  const blocks = message.content as Block[];
  const calls = blocks.filter(b => b.type === "toolCall");
  return (
    calls.length > 0 &&
    !calls.some(c => WAIT_TOOLS[c.name ?? ""]) &&
    blocks.some(b => b.type === "text" && b.text?.trim())
  );
}

export default function turnSeparator(pi: ExtensionHost) {
  pi.setLabel("Turn separator");

  pi.registerMessageRenderer(TYPE, (_message, _options, theme) => ({
    render: width => ["", theme.fg("dim", "─".repeat(Math.max(0, width)))],
    invalidate: () => {},
  }));

  pi.on("message_end", (event, ctx) => {
    if (!ctx.hasUI || ctx.agent.kind !== "main" || !shouldSeparate(event.message)) return;
    pi.sendMessage({ customType: TYPE, content: "", display: true }, { deliverAs: "aside" });
  });

  pi.on("context", event => ({
    messages: event.messages.filter(m => !(m.role === "custom" && m.customType === TYPE)),
  }));
}

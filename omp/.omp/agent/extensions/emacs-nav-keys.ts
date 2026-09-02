/**
 * Emacs-style list navigation: Ctrl+N / Ctrl+P act as Down / Up.
 *
 * omp's completion popup (`/model`, `#`, file refs, ...) only listens for the
 * arrow keys, and there is no keybinding action id for its selection, so it
 * cannot be remapped through `keybindings.yml`. Terminal input listeners run
 * before overlay/focus dispatch and may rewrite the raw byte stream
 * (`{ data }`), so translating the two chords into arrow escapes moves the
 * popup selection and, with no popup open, gives the usual line/history
 * movement.
 *
 * Ctrl+P must also be unbound from `app.model.cycleForward` in
 * `keybindings.yml`; the rewrite happens upstream of chord dispatch, so an
 * unmoved binding would simply stop firing.
 */

type InputResult = { consume?: boolean; data?: string } | undefined;
type ExtensionHost = {
  setLabel(label: string): void;
  on(
    event: "session_start",
    handler: (
      event: unknown,
      ctx: {
        hasUI: boolean;
        ui: { onTerminalInput(listener: (data: string) => InputResult): () => void };
      },
    ) => void,
  ): void;
};

const UP = "\x1b[A";
const DOWN = "\x1b[B";

// Raw control codes, plus the CSI-u encodings sent under the kitty keyboard
// protocol for press (no suffix / `:1`) and repeat (`:2`). Release events
// (`:3`) are left alone so one keypress never translates twice.
const REWRITES: ReadonlyArray<readonly [RegExp, string]> = [
  [/\x0e/g, DOWN],
  [/\x10/g, UP],
  [/\x1b\[110;5(?::[12])?u/g, DOWN],
  [/\x1b\[112;5(?::[12])?u/g, UP],
];

function translate(data: string): InputResult {
  let out = data;
  for (const [pattern, replacement] of REWRITES) {
    if (pattern.test(out)) out = out.replace(pattern, replacement);
    pattern.lastIndex = 0;
  }
  return out === data ? undefined : { data: out };
}

export default function emacsNavKeys(pi: ExtensionHost) {
  pi.setLabel("Emacs nav keys");
  pi.on("session_start", (_event, ctx) => {
    if (!ctx.hasUI) return;
    ctx.ui.onTerminalInput(translate);
  });
}

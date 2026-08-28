/**
 * Zed-style worktree labels for the status-line `path` segment.
 *
 * Zed creates git worktrees as `<parent>/worktrees/<repo>/<name>/<repo>`, so the
 * worktree root's basename is the repo name and omp's built-in `path` segment
 * renders `<repo>/<repo>` (e.g. `mijxdr-mvp/mijxdr-mvp`). This wraps the segment
 * and, for that exact layout only, substitutes the `<name>` component so the
 * status line reads `mijxdr-mvp/xdr-agent-versioning`.
 *
 * Any other worktree or checkout falls through to the built-in renderer untouched.
 */

type SegmentResult = { content: string; visible: boolean };
type SegmentContext = {
  worktree?: { projectName: string; worktreeName: string } | null;
  [key: string]: unknown;
};
type Segment = {
  id: string;
  render(ctx: SegmentContext): SegmentResult;
  zedWorktreePatched?: boolean;
};
type ExtensionHost = {
  setLabel(label: string): void;
  on(event: string, handler: () => void): void;
  pi?: { SEGMENTS?: Record<string, Segment> };
};

const WORKTREES_DIR = "worktrees";

let cachedKey = "";
let cachedName: string | null = null;

/**
 * Deepest `worktrees/<project>/<name>/<project>` match in `cwd`, else null.
 * Matches from the worktree root and from any directory below it.
 */
function zedWorktreeName(cwd: string, projectName: string): string | null {
  const key = `${cwd}\u0000${projectName}`;
  if (key === cachedKey) return cachedName;
  const parts = cwd.split("/").filter(Boolean);
  let found: string | null = null;
  for (let i = parts.length - 4; i >= 0; i--) {
    if (
      parts[i] === WORKTREES_DIR &&
      parts[i + 1] === projectName &&
      parts[i + 3] === projectName
    ) {
      found = parts[i + 2];
      break;
    }
  }
  cachedKey = key;
  cachedName = found;
  return found;
}

function installSegmentWrapper(pi: ExtensionHost): boolean {
  const segments = pi.pi?.SEGMENTS;
  const original = segments?.path;
  if (!segments || !original) return false;
  if (original.zedWorktreePatched) return true;

  segments.path = {
    id: "path",
    zedWorktreePatched: true,
    render(ctx: SegmentContext): SegmentResult {
      const worktree = ctx?.worktree;
      // Only the redundant `<repo>/<repo>` rendering is a candidate.
      if (!worktree || worktree.worktreeName !== worktree.projectName) {
        return original.render(ctx);
      }
      const name = zedWorktreeName(process.cwd(), worktree.projectName);
      if (!name) return original.render(ctx);
      return original.render({
        ...ctx,
        worktree: { ...worktree, worktreeName: name },
      });
    },
  };
  return true;
}

export default function zedWorktreePath(pi: ExtensionHost) {
  pi.setLabel("Zed worktree path");
  if (installSegmentWrapper(pi)) return;
  // Status-line module not initialized at load time: retry once the session is up.
  pi.on("session_start", () => {
    installSegmentWrapper(pi);
  });
}

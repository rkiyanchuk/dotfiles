// display-dock-reset
//
// Restarts the Dock (which hosts Mission Control and Spaces) a few seconds
// after an external display is connected or disconnected, working around the
// macOS 27 bug where Spaces/Mission Control get into a bad state on display
// reconfiguration.
//
// Event-driven: AppKit posts didChangeScreenParameters on every display
// topology change. This requires a real NSApplication connected to the window
// server — a bare CoreGraphics binary registering
// CGDisplayRegisterReconfigurationCallback never receives anything, which is
// why that approach failed silently. The activation policy is .prohibited, so
// no Dock tile and no menu bar.
//
// The notification also fires for resolution, arrangement and mirroring
// changes, so the display *set* is compared and only connect/disconnect
// triggers a restart.

import AppKit
import Foundation

let debounceSeconds = 5.0
let verbose = CommandLine.arguments.contains("--verbose")
let selfTest = CommandLine.arguments.contains("--selftest")
let dryRun = CommandLine.arguments.contains("--dry-run")

func log(_ message: String) {
    let stamp = ISO8601DateFormatter().string(from: Date())
    print("\(stamp) \(message)")
    fflush(stdout)
}

// MARK: - Display inventory

struct DisplaySnapshot: Equatable {
    let ids: [CGDirectDisplayID]

    static func current() -> DisplaySnapshot {
        let ids = NSScreen.screens.compactMap { screen -> CGDirectDisplayID? in
            screen.deviceDescription[
                NSDeviceDescriptionKey("NSScreenNumber")
            ] as? CGDirectDisplayID
        }
        return DisplaySnapshot(ids: ids.sorted())
    }

    var description: String {
        if ids.isEmpty { return "none" }
        return ids.map { id in
            let kind = CGDisplayIsBuiltin(id) != 0 ? "builtin" : "external"
            let size = CGDisplayBounds(id).size
            return "\(id)[\(kind) \(Int(size.width))x\(Int(size.height))]"
        }.joined(separator: " ")
    }
}

// MARK: - Dock restart

func restartDock() {
    if dryRun {
        log("dry-run: would run killall Dock")
        return
    }
    let task = Process()
    task.executableURL = URL(fileURLWithPath: "/usr/bin/killall")
    task.arguments = ["Dock"]
    do {
        try task.run()
        task.waitUntilExit()
        log("killall Dock -> exit \(task.terminationStatus)")
    } catch {
        log("killall Dock failed: \(error)")
    }
}

// MARK: - Watcher

final class Watcher {
    private var snapshot = DisplaySnapshot.current()
    private var pending: DispatchWorkItem?

    func start() {
        log("watching screen parameter changes (debounce \(debounceSeconds)s)")
        log("initial displays: \(snapshot.description)")
        NotificationCenter.default.addObserver(
            self,
            selector: #selector(screensChanged),
            name: NSApplication.didChangeScreenParametersNotification,
            object: nil
        )
    }

    @objc private func screensChanged() {
        let now = DisplaySnapshot.current()
        guard now != snapshot else {
            if verbose {
                log("screen parameters changed, display set unchanged (\(now.description))")
            }
            return
        }
        let added = now.ids.filter { !snapshot.ids.contains($0) }
        let removed = snapshot.ids.filter { !now.ids.contains($0) }
        snapshot = now

        var parts: [String] = []
        if !added.isEmpty { parts.append("added \(added.map(String.init).joined(separator: ","))") }
        if !removed.isEmpty { parts.append("removed \(removed.map(String.init).joined(separator: ","))") }

        log("displays now: \(now.description)")
        log("display set changed (\(parts.joined(separator: " "))); Dock restart in \(debounceSeconds)s")

        pending?.cancel()
        let item = DispatchWorkItem { restartDock() }
        pending = item
        DispatchQueue.main.asyncAfter(deadline: .now() + debounceSeconds, execute: item)
    }
}

let app = NSApplication.shared
app.setActivationPolicy(.prohibited)

let watcher = Watcher()
watcher.start()

if selfTest {
    log("selftest: listening for 30s; connect or disconnect a display now")
    DispatchQueue.main.asyncAfter(deadline: .now() + 30) {
        log("selftest: done")
        exit(0)
    }
}

app.run()

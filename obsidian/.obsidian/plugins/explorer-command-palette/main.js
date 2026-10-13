// Adds an "Open command palette" button to the file explorer's action row
// (new note, new folder, sort, ...). Mobile only: desktop has Cmd+P.
const { Plugin, Platform, setIcon } = require("obsidian");

module.exports = class ExplorerCommandPalette extends Plugin {
    onload() {
        if (!Platform.isMobile) return;
        this.app.workspace.onLayoutReady(() => this.addButtons());
        this.registerEvent(this.app.workspace.on("layout-change", () => this.addButtons()));
    }

    addButtons() {
        for (const leaf of this.app.workspace.getLeavesOfType("file-explorer")) {
            const row = leaf.view.containerEl.querySelector(".nav-buttons-container");
            if (!row || row.querySelector(".explorer-command-palette")) continue;

            const button = row.createDiv({
                cls: "clickable-icon nav-action-button explorer-command-palette",
                attr: { "aria-label": "Open command palette" },
            });
            setIcon(button, "terminal");
            button.addEventListener("click", () => {
                this.app.commands.executeCommandById("command-palette:open");
            });
            this.register(() => button.remove());
        }
    }
};

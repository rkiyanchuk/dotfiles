// Makes the search button in the mobile navigation bar find in the open note,
// like Find in Note in Notes. With no note open it still opens the quick
// switcher; pull-down and the command palette keep the switcher either way.
//
// Obsidian hardwires the button to the switcher and offers no setting, so a
// capturing listener on the bar claims the tap before the button's own one.
const { MarkdownView, Plugin, Platform } = require("obsidian");

module.exports = class NavbarFindInNote extends Plugin {
    onload() {
        if (!Platform.isMobile) return;
        this.app.workspace.onLayoutReady(() => {
            const navbar = this.app.mobileNavbar?.containerEl;
            if (!navbar) return;
            this.registerDomEvent(navbar, "click", (evt) => this.onClick(evt), { capture: true });
        });
    }

    onClick(evt) {
        if (!evt.target.closest(".mobile-navbar-action-quick-switcher")) return;
        const view = this.app.workspace.getActiveViewOfType(MarkdownView);
        if (!view) return;
        evt.stopPropagation();
        view.showSearch(false);
    }
};

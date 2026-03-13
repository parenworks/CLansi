# CLansi Roadmap

## Current Status: v0.1.0

**Released:** March 2026

**Tested Implementations:**
- SBCL (Linux) - 60/60 tests passing
- CCL (Linux) - 60/60 tests passing  
- ECL (Linux) - 60/60 tests passing

**Core Features:**
- ANSI escape sequences (colors, cursor, screen control)
- Raw terminal mode with proper cleanup
- Key event parsing (arrows, function keys, mouse, UTF-8)
- Basic widget system (panel, scrollable-list)

---

## v0.2.0 - Widgets & Layout

### High Priority

- [x] **Text input widget** - Single-line text field with cursor, editing
- [x] **Progress bar** - Determinate and indeterminate styles
- [x] **Status bar** - Fixed position status/info display
- [x] **Layout system** - Horizontal/vertical splits, automatic resizing
- [ ] **Theme support** - Configurable color schemes, light/dark modes

### Medium Priority

- [ ] **macOS support** - Test and fix Darwin-specific termios differences
- [x] **Signal handling** - SIGWINCH for terminal resize events
- [x] **Double-buffering** - Reduce flicker with screen diffing

---

## v0.3.0 - Interactive Components

- [x] **Modal dialogs** - Confirmation boxes, input prompts
- [x] **Menu system** - Dropdown/popup menus with keyboard navigation
- [x] **Table widget** - Columnar data display with sorting
- [x] **Form system** - Tab-navigable input forms
- [x] **Tree view** - Expandable/collapsible hierarchical display

---

## v0.4.0 - Polish & Performance

- [x] **Documentation** - Full API reference, tutorials, more examples
- [x] **Performance optimization** - Output buffering, diff-based rendering
- [x] **Accessibility** - Screen reader hints (announce, set-title, widget-focus-hint)
- [x] **Windows support** - Windows Terminal / ConPTY compatibility (SBCL, CCL)

---

## Future Ideas

- CLIM-style presentation system (type-aware clickable objects)
- Declarative UI DSL
- Animation support (spinners, transitions)
- Image rendering (sixel/kitty graphics protocol)
- Integration with McCLIM's charming backend

---

## Contributing

See [Bug Reports & Contributing](README.md#bug-reports--contributing) in the README.

Issues and PRs welcome at: https://github.com/parenworks/CLansi

---

## Changelog

### v0.4.0 (March 2026)
- Documentation: API reference, tutorial, example apps
- Performance: output buffering, optimized diff rendering
- Accessibility: screen reader hints (OSC sequences)
- Windows support: Windows Terminal / ConPTY (SBCL, CCL)
- 292 tests passing

### v0.3.0 (March 2026)
- Modal dialogs (alert, confirm, input prompt)
- Menu system (popup menus, menu bar)
- Table widget with sorting
- Form system with validation
- Tree view with expand/collapse
- 283 tests passing

### v0.2.0 (March 2026)
- Text input widget with cursor navigation
- Progress bar (determinate and indeterminate)
- Status bar widget
- Layout system (horizontal, vertical, split pane)
- Signal handling (SIGWINCH)
- Double-buffering screen system
- 154 tests passing

### v0.1.0 (March 2026)
- Initial release
- SBCL, CCL, ECL support on Linux
- Core ANSI escape sequence library
- Raw terminal mode management
- Key/mouse event parsing
- Panel and scrollable-list widgets
- 60 tests passing

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
- [ ] **Double-buffering** - Reduce flicker with screen diffing

---

## v0.3.0 - Interactive Components

- [ ] **Modal dialogs** - Confirmation boxes, input prompts
- [ ] **Menu system** - Dropdown/popup menus with keyboard navigation
- [ ] **Table widget** - Columnar data display with sorting
- [ ] **Form system** - Tab-navigable input forms
- [ ] **Tree view** - Expandable/collapsible hierarchical display

---

## v0.4.0 - Polish & Performance

- [ ] **Documentation** - Full API reference, tutorials, more examples
- [ ] **Performance optimization** - Minimize escape sequence output
- [ ] **Accessibility** - Screen reader hints where possible
- [ ] **Windows support** - Windows Terminal / ConPTY compatibility

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

### v0.1.0 (March 2026)
- Initial release
- SBCL, CCL, ECL support on Linux
- Core ANSI escape sequence library
- Raw terminal mode management
- Key/mouse event parsing
- Panel and scrollable-list widgets
- 60 tests passing

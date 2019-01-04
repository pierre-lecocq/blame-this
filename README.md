# Blame this!

Display a git blame summary of the current line or file

## Install

- Add `blame-this.el` in your load path
- Activate the minor mode with `(blame-this-mode 1)`

## Keybindings

- Use `C-c b l` to blame the current line
- Use `C-c b f` to blame the current file

## TODO

- [ ] Add package to MELPA and `use-package` install instruction
- [ ] Place overlay above line if it appears at the bottom of the buffer
- [ ] Automatic mode, after a delay without moving the cursor

## Changelog

### 0.1

- Initial version
  - minor mode
  - blame line
  - blame file
  - customizable face
  - possible output: minibuffer, compile buffer, overlay

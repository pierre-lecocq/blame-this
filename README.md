# Blame this!

Display a git blame summary of the current line or file

## Install

- Add `blame-this.el` in your load path
- Activate the minor mode with `(blame-this-mode 1)`

## Keybindings

- Use `C-c b l` to blame the current line
- Use `C-c b f` to blame the current file

## Customization

All customizations can be done via the `M-x customize` command. Then search for `blame-this`.

### Display modes

Variables are:

- `blame-this-line-display-mode`, default is `overlay`
- `blame-this-file-display-mode`, default is `compile`

Possible values are:

- `overlay` to display the information in an in-buffer popup
- `minibuffer` to display the information in the minibuffer
- `compile` to display the information in a dedicated buffer

### Timeout

In `overlay` mode, a timeout can be specified in seconds so the overlay disapear automatically

Variable is:

- `blame-this-overlay-timeout`, default is `10` (in seconds)

### Font face

In `overlay` mode, the font face can be tweaked

Variable is:

- `blame-this-overlay-face`, default is
  - If your main theme is dark, background is light grey (`grey10`) and foreground is black
  - If your main theme is light, background is dark grey (`grey90`) and foreground is white

## TODO

- [ ] Add package to MELPA and `use-package` install instruction
- [ ] Place overlay above line if it appears at the bottom of the buffer
- [ ] Automatic mode, after a delay without moving the cursor
- [ ] Parse output and display a better (simpler) version. Maybe make the ouput customizable?

## Changelog

### 0.1

- Initial version
  - minor mode
  - blame line
  - blame file
  - customizable face
  - possible output: minibuffer, compile buffer, overlay

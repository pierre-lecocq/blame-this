# Blame this!

Display a git blame summary of the current line.

## Install

- Add `blame-this.el` in your load path
- Activate the minor mode with `(blame-this-mode 1)`

## Keybindings

Use `C-c b l` to blame the current line

## Customization

All customizations can be done via the `M-x customize` command. Then search for `blame-this`.
Otherwise, you can set the following variables in your Emacs configuration file.

| Variable                       | Comment                                                             | Possible values                             | Default                                                                       |
|--------------------------------|---------------------------------------------------------------------|---------------------------------------------|-------------------------------------------------------------------------------|
| `blame-this-line-display-mode` | Where to display the result                                         | `overlay` (in-buffer popup) or `minibuffer` | `overlay`                                                                     |
| `blame-this-overlay-timeout`   | Timeout to make the overlay disapear (only in overlay display mode) | Any positive integer                        | `10`                                                                          |
| `blame-this-on-idle`           | Automatically blame the current line on idle                        | `non-nil` to activate                       | `nil`                                                                         |
| `blame-this-idle-time`         | Idle time in seconds to blame the current line automatically        | Any positive integer                        | `2`                                                                           |
| `blame-this-overlay-face`      | Font face (only in overlay display mode)                            | Any font face                               |  An ugly black on yellow, forcing you to improve this according to your theme |

## Changelog

### 0.3

- Add idle support
- Simplify all the flow

### 0.2

- Add `blame-this-display-type` support
- Remove useless file support

### 0.1

- Initial version

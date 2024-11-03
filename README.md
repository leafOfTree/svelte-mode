<img src="https://raw.githubusercontent.com/leafOfTree/leafOfTree.github.io/master/svelte-mode.svg" width="60" height="60" alt="icon" align="left"/>

# svelte-mode

<p align="center">
<a href="https://github.com/altercation/vim-colors-solarized">
<img alt="screenshot" src="https://raw.githubusercontent.com/leafOfTree/leafOfTree.github.io/master/emacs-svelte-mode.png" width="220"/>
</a>
</p>

Emacs major mode for `.svelte` files. It's based on [mhtml-mode][0]. It requires `(>= emacs-major-version 26)`.

## Installation

- [MELPA][2]

  <kbd>M-x</kbd> `package-install` <kbd>RET</kbd> `svelte-mode` <kbd>RET</kbd>

- Manually

  ```bash
  git clone https://github.com/leafOfTree/svelte-mode --depth 1
  ```

  ```lisp
  ; ~/.emacs
  (add-to-list 'load-path "/path/to/svelte-mode")
  (require 'svelte-mode)
  ```

  For [Spacemacs][1], put them inside `dotspacemacs/user-config`.

  ```lisp
  ; ~/.spacemacs
  (defun dotspacemacs/user-config ()
        
      (add-to-list 'load-path "/path/to/svelte-mode")
      (require 'svelte-mode)
  ```
  
Svelte-mode should be auto enabled for `.svelte` files if everything goes well. Please stay up to date. Feel free to open an issue or a pull request.


## How it works

This major mode includes `JavaScript/CSS` as `submode` in `html-mode`. 

Supports

- Svelte directives and blocks.
- emmet-mode HTML/CSS detection.
- `typescript-mode` in `<script lang="ts">...</script>`
- `sass-mode` in `<style lang="sass">...</style>`.
- `pug-mode` in `<template lang="pug">...</template>`.
- `coffee-mode` in `<script lang="coffee">...</script>`.

> [!IMPORTANT]
> You need to **install** these modes first.

## Customize

<kbd>M-x</kbd> `customize-set-variable` <kbd>RET</kbd> `<variable-name>` <kbd>RET</kbd> 

Or customize variable programatically, like
```lisp
(customize-set-variable 'svelte-basic-offset 2)
```
See detail by <kbd>M-x</kbd> `describe-variable` <kbd>RET</kbd> `<variable-name>` <kbd>RET</kbd>.

| name                        | description                                                       | default           |
|-----------------------------|-------------------------------------------------------------------|-------------------|
| svelte-basic-offset         | Specifies the basic indentation level.                            | sgml-basic-offset |
| svelte-tag-relative-indent  | Whether `<script>` and `<style>` bodies indent to the tag.        | t                 |
| svelte-display-submode-name | Whether to display submode name in the status line.               | nil               |


[0]: https://github.com/emacs-mirror/emacs/blob/master/lisp/textmodes/mhtml-mode.el
[1]: https://github.com/syl20bnr/spacemacs
[2]: https://melpa.org/#/svelte-mode

## Configuration hacks

### Closing tags for components with "reserved" names
SGML mode, which `svelte-mode` is derived from, automatically closes your current tag for you with the `C-c C-e` shortcut
(`sgml-close-tag`). This however does not work for components that share their name with unclosed html tags, like for example
the `Link` component from `svelte-routing`. SGML mode by default checks whether tags are supposed to be closed or not by comparing
tag names with lists of element names case-insensitively, so `<Link>` is equivalent to `<link>`. The following configuration snippet 
makes the comparison of tag names by SGML mode case-sensitive when svelte-mode is the current active major-mode in the buffer. Just
add it to your config file and you're good to go.

```elisp
(defun svelte-mode-sgml-empty-tag-p-advice (old-function tag-name)
  "Advice function intended to wrap around `sgml-empty-tag-p

Makes case significant when checking whether tags need to be
closed or not, to not confuse elements with Svelte components."
  (if (eq major-mode 'svelte-mode)
      (assoc-string tag-name sgml-empty-tags)
    (funcall old-function tag-name)))

(defun svelte-mode-sgml-unclosed-tag-p-advice (old-function tag-name)
  "Advice function intended to wrap around `sgml-unclosed-tag-p

Makes case significant when checking whether tags need to be
closed or not, to not confuse elements with Svelte components."
  (if (eq major-mode 'svelte-mode)
      (assoc-string tag-name sgml-unclosed-tags)
    (funcall old-function tag-name)))

(advice-add 'sgml-empty-tag-p :around 'svelte-mode-sgml-empty-tag-p-advice)
(advice-add 'sgml-unclosed-tag-p :around 'svelte-mode-sgml-unclosed-tag-p-advice)
```

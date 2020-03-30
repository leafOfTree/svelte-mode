# svelte-mode

<p align="center">
<a href="https://github.com/altercation/vim-colors-solarized">
<img alt="screenshot" src="https://raw.githubusercontent.com/leafOfTree/leafOfTree.github.io/master/emacs-svelte-mode.png" width="220"/>
</a>
</p>

Emacs major mode for `.svelte` files. It's based on [mhtml-mode][0]. It requires `(>= emacs-major-version 26)`.

## Installation

- `package-install`

  `M-x` `package-install` [RET] `svelte-mode` [RET]

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
- Emmet-mode HTML/CSS detection.
- Pug-mode in `<template lang="pug">...</template>`.
- Coffee-mode in `<script lang="coffee">...</script>`.

> Relative modes need to be installed.

## Customize

See detail by `M-x` `describe-variable` [RET] `<variable-name>` [RET].

| name   | description |
|--------|-------------|
| svelte-tag-relative-indent | How `<script>` and `<style>` bodies are indented relative to the tag. |
| sgml-basic-offset | Specifies the basic indentation level. |


[0]: https://github.com/emacs-mirror/emacs/blob/master/lisp/textmodes/mhtml-mode.el
[1]: https://github.com/syl20bnr/spacemacs

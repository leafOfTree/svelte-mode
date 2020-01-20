# svelte-mode

<p align="center">
<a href="https://github.com/altercation/vim-colors-solarized">
<img alt="screenshot" src="https://raw.githubusercontent.com/leafOfTree/leafOfTree.github.io/master/emacs-svelte-mode.png" width="220"/>
</a>
</p>

Emacs major mode for `.svelte` files. Based on [mhtml-mode][0].

## Installation

- Manually

  ```bash
  git clone https://github.com/leafOfTree/svelte-mode
  ```

  ```lisp
  ; .emacs
  (add-to-list 'load-path "/path/to/svelte-mode")
  (require 'svelte-mode)
  ```

<!--
- `package-install`

  `M-x` `package-install` [RET] `svelte-mode` [RET]
-->
  
Svelte-mode should be auto enabled for `.svelte` files if everything goes well. Please stay up to date. Feel free to open an issue or a pull request.


## How it works

This major mode includes `JavaScript/CSS` as `submode` in `html-mode`.

Supports

- Svelte directives and blocks.
- Emmet-mode HTML/CSS detection.
- Pug-mode in `<template lang="pug">...</template>`.
- Coffee-mode in `<script lang="coffee">...</script>`.

> Relative modes need to be installed.

### Todo

- Preprocess languages: TypeScript.

## Customize

See detail by `M-x` `describe-variable` [RET] `<variable-name>` [RET]

| name   | description |
|--------|-------------|
| svelte-tag-relative-indent | How `<script>` and `<style>` bodies are indented relative to the tag. |
| sgml-basic-offset | Specifies the basic indentation level. |


[0]: https://github.com/emacs-mirror/emacs/blob/master/lisp/textmodes/mhtml-mode.el

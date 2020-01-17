# svelte-mode

<p align="center">
<a href="https://github.com/altercation/vim-colors-solarized">
<img alt="screenshot" src="https://raw.githubusercontent.com/leafOfTree/leafOfTree.github.io/master/emacs-svelte-mode.png" width="220"/>
</a>
</p>

Emacs major mode for '.svelte' files. Based on [mhtml-mode][0].

## Installation

- Manually

  ```bash
  git clone https://github.com/leafOfTree/svelte-mode
  ```

  ```lisp
  (add-to-list 'load-path "/path/to/svelte-mode")
  (require 'svelte-mode)
  ```

<!--
- `package-install`

  `M-x` `package-install` [RET] `svelte-mode` [RET]
-->
  
Svelte-mode should be enabled for `.svelte` files if everything goes well. Please stay up to date. Feel free to open an issue or a pull request.


## How it works

This major mode includes `JavaScript/CSS` as `submode` in `html-mode`.

Supports

- Svelte directives and blocks.

### Todo

- Preprocess languages
- Context based emmet-mode support.

## Customize

See detail by `M-x` `describe-variable` [RET] `<variable-name>` [RET]

| name   | description |
|--------|-------------|
| svelte-tag-relative-indent | How `<script>` and `<style>` bodies are indented relative to the tag. |


[0]: https://github.com/emacs-mirror/emacs/blob/master/lisp/textmodes/mhtml-mode.el

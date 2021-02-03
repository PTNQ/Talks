---
title: Modal editing with Vim
separator: <!--s-->
verticalSeparator: <!--v-->
revealOptions:
transition: 'fade'
---

# Modal editing with Vi & Vim

presented by notarock

<!--s-->

# Agenda

- What's vim anyway? What about this talk?
- Modes
- Thinking in vim
- Ex/Sed commands
- Additionnal ressources

<!--v-->

## What I will talk about

- Help you with the initial "Learning curve"!

<img src="https://lucasfcosta.com/assets/vim-learning-curve.jpg" alt="learning-curbe" width="600"/>

<!--v-->

## What I will not talk about

- Implementations like neovim, evil-mode, IDE plugins, etc.
- Vimscript, vimrc and plugins

<!--s-->

# What's vim?

<img
src="https://www.housingunits.co.uk/media/catalog/product/cache/60968cec045f20fb06ab5f7720001507/1/4/1402bcff81f54c4b157b7e5d707abf6ef8d70667_445279_2.jpg"
alt="Sharp knife" width="400"/>

<!--v-->

- Modal text editor (More about this later!)
- Present on most *nix systems (If not vim, vi is)
- Can be tailored to your needs
- Fast!

<!--v-->

## Improving on an older, proven editor

- VIM stands for **Vi** **I**mproved (1991)
- Vi stands for  **Vi**sual Editor (1976)
- ex stands for **Ex**tended (1970s)

<!--v-->

## Uses buffers to edit files

- Load file, Make edits, Write to file.
- Load multiple files
- Does not care about file size at all!

<!--s-->

# Modes

| Normal mode    | Insert mode      | Visual mode       | Command mode     |
| :------------- | :----------:     | -----------:      | :-------------   |
| Navigation     | Write characters | Select characters | Execute commands |

<!--v-->

# Normal mode

- Spend more than 80% of your time here
- Press `esc` to enter normal mode
- *Almost* every keys are mapped to a motion or an action.
- Access other modes from here

<!--v-->

# Insert mode

- Type text into the buffer
- Press `i` to enter insert mode

<!--v-->

# Visual mode

- v          : character-wise visual mode
- V          : line-wise visual mode
- Ctrl-v     : block-wise visual mode

<!--v-->

# Command/Ex mode

- `:` to start typing a command from normal mode
- `Ctrl-o :` to type commands from insert mode
- Syntax is similar to `sed`
```
ex -s +%s/127/128/g +%p +q! /etc/hosts
sed s/127/128/g /etc/hosts
```

<!--s-->

# Thinking in vim

- You don't "remember" shortcuts.
- You apply commands in order to achieve editing goals.
- Keybindings are mnemonic. (`a` is for append, etc.)

<!--v-->

## Type of actions in normal mode

- Motion
- Command
- Operators
- Extras (e.g. Leader key)

<!--v-->

![Graphical cheat sheet](http://www.viemu.com/vi-vim-cheat-sheet.gif)

```
<number><command><text object or motion>
```

<!--v-->

- Can be saved in macros!
- Can be applied to multiple lines with the `:norm` command!

<!--s-->

# Ex/Sed commands

Some basics

| Write | Quit | Edit | Read |
|-------|------|------|------|
| `:w`  | `:q` | `:e` | `:r` |

<!--v-->

## Common uses for commands

- `:s/Find-Word/Replace-Word/gc`

<!--v-->

### Commands can be used for...

- Interracting with buffers
- Interracting with tabs/windows
- Interracting with registers (Clipboard)
- Get help

<!--s-->

# Additionnal ressources

- [Cheat sheet](https://www.fprintf.net/vimCheatSheet.html)
- [Graphical Cheat sheet](https://lucasfcosta.com/assets/vim-learning-curve.jpg)


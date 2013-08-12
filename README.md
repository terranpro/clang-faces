<div id="table-of-contents">
<h2>Table of Contents</h2>
<div id="text-table-of-contents">
<ul>
<li><a href="#sec-1">1. clang-faces</a>
<ul>
<li><a href="#sec-1-1">1.1. Quick Install</a></li>
<li><a href="#sec-1-2">1.2. Emacs Setup</a></li>
<li><a href="#sec-1-3">1.3. How It Works</a></li>
<li><a href="#sec-1-4">1.4. Contribute</a></li>
<li><a href="#sec-1-5">1.5. Author</a></li>
</ul>
</li>
</ul>
</div>
</div>
# clang-faces

Intelligent syntax higlighting through libclang w/emacs!

## Quick Install

-   Clone the repository

-   mkdir build; cd build

-   cmake ..

-   make

## Emacs Setup

(setq clang-faces-client-exec
"~/path/clang-faces/build/emacs-clang-syntaxhl")

M-x clang-faces-mode

## How It Works

It works together w/the font-lock infrastructure, mimicing [most of]
its faces with syntax highlighting enhancements to statement-level
syntax that font-lock cannot (at least that I'm aware of) parse and
colorize.

The primary difference is that references to variables and functions
throughout the code (not just the declaration) are syntax higlighted.

It operates through a separate daemon process that constantly
reparses the source code and updates the modified (delta) regions'
\`font-lock-face' attributes.

## Contribute

Your feedback is greatly appreciated.  The current state of
clang-faces is alpha and there are issues related to recoloring (you
might get rainbows of colors across one identifier).  If you can
contribute a minimum test case, or better a pull request w/a test
case, I will add your name to the contributors list and you will
be overcome with pride and great satisfication knowing you have
advanced the sum of human knowledge another infinitesimal amount.

## Author

Brian Fransioli

assem@terranpro.org

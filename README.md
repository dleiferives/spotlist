# SpotList

[![License: GPL v3](https://img.shields.io/badge/License-GPLv3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)

SpotList is an Emacs package that provides live-tracking, navigable bookmarks for text regions. Unlike traditional bookmarks, SpotList lets you **edit bookmarked text inline** with changes automatically syncing back to the source buffer. 


## Features

- Bookmark text regions or lines from any buffer
- Edit bookmarked text directly in the spotlist buffer
- Changes sync between the buffer and the file
- Syntax highlighting preserved from the source buffer
- Folding entries
- Quickly navigating back
- Evil mode integration

## Installation

Manually

``` emacs-lisp
(add-to-list 'load-path "/path/to/spotlist.el")
(require 'spotlist)
```


From straight
``` emacs-lisp
(straight-use-package 
    '(spotlist :host github :repo "dleiferives/spotlist" :branch "trunk"))
```

## Configuration

``` emacs-lisp
;; Adjust refresh interval (seconds, or nil to disable)
(setq spotlist-visibility-refresh-interval 0.2)

;; Typing pause before auto-refresh resumes
(setq spotlist-typing-pause-duration 0.25)

;; Change global prefix (default: C-c C-s)
(setq spotlist-global-prefix "C-c s")

;;; And more!
```

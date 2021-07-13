When it is bug report, please attatch texlab.log.
You can make this log file by evaluating expression below and turn `lsp` on on tex file.
```emacs-lisp
(setq lsp-latex-texlab-executable-argument-list '("-vvvv" "--log-file" "/path/to/texlab.log"))
```
After that, delete these comments.

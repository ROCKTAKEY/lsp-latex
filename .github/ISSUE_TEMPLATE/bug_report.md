---
name: Bug report
about: Create a report to help us improve
title: ''
labels: ''
assignees: ''

---

**Describe the bug**
A clear and concise description of what the bug is.

**Expected behavior**
A clear and concise description of what you expected to happen.

**Screenshots**
If applicable, add screenshots to help explain your problem.

**Environments**
 - OS: [e.g. iOS]
 - Emacs version: [e.g. 29.1]
 - Texlab version: [e.g. 5.21.0]
 - lsp-latex version: [e.g. 3.9.0]

**To Reproduce**
Steps to reproduce the behavior:
1. M-x `some-command`
2. Evaluate `(some-function)`

**Log**
- Attach texlab.log by the procedure described below.
  - Evaluate the expression below. 
    ```emacs-lisp
    (setq lsp-latex-texlab-executable-argument-list '("-vvvv" "--log-file" "/path/to/texlab.log"))
    ```
    - You can evaluate it by one of the procedures below:
      - push `M-:`, paste the expression, and push Enter.
      - paste the expression to a buffer like `*scratch*`, move point (called "cursor" outside Emacs) to the end of the expression, and push `C-x C-e`.
      - paste the expression to your `init.el` (in `~/.emacs.d`) and restart Emacs.
  -  Turn `lsp` on on your tex file.
  - Do your reproduction procedure.

**Additional context**
Add any other context about the problem here.

This repo contains a hacked-together Emacs package that depends on Clojure mode being installed.
It will hopefully be rewritten and put into one of the package managers eventually.

Any contributions are most welcome!

## Configuration

Add this to your configuration file:

```emacs-lisp
(add-to-list 'load-path "~/your/path/to/carp-emacs")

(require 'carp-mode)
(require 'inf-carp-mode)

;; Use carp-mode for .carp files
(add-to-list 'auto-mode-alist '("\\.carp\\'" . carp-mode))
```

To start an interactive session, make sure `carp` is in your path (inside Emacs) and execute `M-x run-carp`.

## Flycheck

This minor mode is highly recommended -- Carp's type errors are much easier to understand when they can be seen visually in the source code.

```
(require 'carp-flycheck)

(add-hook 'carp-mode-hook
          (lambda ()
            (flycheck-mode 1)))
```

## Goto Definition (Xref)

Using Carp's Repl with Emacs's Xref you can jump to the definition of
variables and functions. Simply add the following config to register
it when working in a carp file

```
(require 'carp-xref)

(add-hook 'carp-mode-hook
	(lambda ()
		(push 'carp-xref-backend xref-backend-functions)))
```

## License

Copyright 2016 - 2020 Erik Svedäng

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.

The regular expression implementation as found in src/carp_regex.h are
Copyright (C) 1994-2017 Lua.org, PUC-Rio under the terms of the MIT license.
Details can be found in the License file LUA_LICENSE.

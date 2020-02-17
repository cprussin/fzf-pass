# fzf-pass

`fzf-pass` is a small wayland client for [pass](https://www.passwordstore.org/)
using [fzf](https://github.com/junegunn/fzf) as the user interface.  It uses
[wl-clipboard](https://github.com/bugaevc/wl-clipboard) to copy fields/passwords
into the clipboard.

## Installing

The easiest way to install is from nix.  For instance:

```nix
nix-build -E "import (fetchTarball \"https://github.com/cprussin/fzf-pass/tarball/master\")"
```

You can also build from source (without nix) by using `cabal-install`.  If you
build from source manually, you'll need to make sure the following are in your
`$PATH` at runtime:

- [pass](https://www.passwordstore.org/)
- [fzf](https://github.com/junegunn/fzf)
- [wl-clipboard](https://github.com/bugaevc/wl-clipboard)

If anyone wants to put together fzf-pass packages for systems other than nix,
I'd love to accept contributions!

## Usage

Simple: `fzf-pass`

## Features

- Copy any field to the clipboard
- Copy OTP codes to the clipboard (you'll need
  [pass-otp](https://github.com/tadfisher/pass-otp#readme) installed for this)
- Clear the clipboard after 30 seconds for passwords
- For entries with a field named `URL`, you'll get a `Go to site` option that
  uses `$BROWSER` to launch your web browser.

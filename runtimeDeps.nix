{
  wl-clipboard,
  pass,
  fzf,
}: [
  wl-clipboard
  (pass.withExtensions (exts: [exts.pass-otp]))
  fzf
]

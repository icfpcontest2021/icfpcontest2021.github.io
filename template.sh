#!/usr/bin/env bash
set -o errexit -o pipefail -o nounset

TITLE="$(sed -n 's/^<h1>\([^<]*\)<.*$/\1/p' "$1")"

cat - <<EOF
<!DOCTYPE html>
<html lang="en">
  <head>
    <meta charset="utf-8">
    <meta name="viewport" content="width=device-width, initial-scale=1" />
    <title>$TITLE</title>
    <link rel="stylesheet" href="style.css">
    <link href="feed.xml" type="application/atom+xml" rel="alternate" title="Atom feed">
  </head>
  <body>
    <nav>
      <a href="index.html">ICFPC2021</a>
      <a href="https://poses.live">Portal</a>
      <a href="scoreboard.html">Scoreboard</a>
      <a href="rules.html">Rules</a>
      <a href="specification.html">Specification</a>
      <a href="faq.html">FAQ</a>
      <a href="prizes.html">Prizes</a>
      <a href="writeups.html">Writeups</a>
      <a href="about.html">About</a>
    </nav>
    $(cat "$1")
    <footer>
      Copyright © ICFPC2021 Organisers |
      <a href="https://berlincodeofconduct.org/">Code of Conduct</a> |
      <a href="https://twitter.com/icfpcontest2021">@icfpcontest2021</a>
    </footer>
  </body>
</html>
EOF

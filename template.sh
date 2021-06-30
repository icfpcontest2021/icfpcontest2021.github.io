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
  </head>
  <body>
    <nav>
      <a href="index.html">ICFPC2021</a>
      <a href="rules.html">Rules</a>
    </nav>
    $(cat "$1")
  </body>
</html>
EOF

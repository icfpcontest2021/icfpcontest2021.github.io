#!/usr/bin/env bash
set -o nounset -o errexit -o pipefail

cat - <<EOF
<h1>ICFP Contest 2021</h1>
<img style="width: 80%; max-width: 200px" alt="ICFP Contest 2021" src="logo.svg"><br>
<p>
  ICFP Contest 2021 will take place<br>
  12:00 PM (noon) Friday 9 July - 12:00 PM (noon) Monday 12 July UTC.
</p>
EOF

for i in $(ls -r updates); do
  TITLE="$(head -n1 "updates/$i")"
  FORMAT="+%a, %d %B %H:%M %Z"
  DATE="$(date --date="$(tail +2 "updates/$i" | head -n1)" -u "$FORMAT")"
  cat - <<EOF
    <section>
      <h2>$TITLE</h2>
      <p>
        <strong>$DATE</strong>
      </p>
      $(tail +3 "updates/$i")
    </section>
EOF
done

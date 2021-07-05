#!/usr/bin/env bash
set -o nounset -o errexit -o pipefail

cat - <<EOF
<?xml version="1.0" encoding="utf-8"?>
<feed xmlns="http://www.w3.org/2005/Atom">
  <title>ICFP Contest 2021</title>
  <link href="https://icfpcontest2021.github.io/"/>
  <link href="https://icfpcontest2021.github.io/feed.xml" rel="self"/>
  <updated>$(date --rfc-3339=seconds -u | tr ' ' 'T')</updated>
  <author>
    <name>ICFP Contest 2021 Organisers</name>
  </author>
  <id>https://icfpcontest2021.github.io/</id>
EOF

for i in $(ls -r updates); do
  TITLE="$(head -n1 "updates/$i")"
  DATE="$(tail +2 "updates/$i" | head -n1 | tr ' ' 'T')"
  cat - <<EOF
    <entry>
      <title>$TITLE</title>
      <link href="https://icfpcontest2021.github.io/"/>
      <id>https://icfpcontest2021.github.io/updates/$i</id>
      <updated>$DATE</updated>
      <summary type="html"><![CDATA[$(tail +3 "updates/$i")]]></summary>
    </entry>
EOF
done

cat - <<EOF
</feed>
EOF

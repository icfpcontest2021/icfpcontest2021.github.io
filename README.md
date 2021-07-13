# README

This is our public webpage.

# Development

This site only uses a couple standard utilities, like `bash`, `sed`, `date`,
`make`.  It should work out of the box on most linux systems.

To build the website, simply use:

    make

This produces a `_site` directory with static files.  You can preview those
directly using a browser, e.g.:

    firefox _site/index.html

Or you can use a simple HTTP server, e.g:

    python -m http.server --directory _site

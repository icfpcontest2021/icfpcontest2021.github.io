# IFCP Contest 2021

## Website

You can find our website in `website/`.

### Development

This site only uses a couple standard utilities, like `bash`, `sed`, `date`,
`make`.  It should work out of the box on most linux systems.

To build the website, simply use:

    make

This produces a `_site` directory with static files.  You can preview those
directly using a browser, e.g.:

    firefox _site/index.html

Or you can use a simple HTTP server, e.g:

    python -m http.server --directory _site

## Toolchain

The toolchain contains the bulk of our code.  It is a Haskell project containing
the database, judge, webserver, and numerours utilities.

## Building

    cabal install

### Configuring

 -  `BRAINWALL_PG_CONNECTION_STRING`: where to find the database.
 -  `BRAINWALL_BIND_PORT`: port to serve the web app on.
 -  `BRAINWALL_BIND_ADDRESS`: address to listen on for the web app.

### Tools

After installing, you should have a number of executables in your `$PATH`.

 -  `brain-wall-animate`: Renders an animation given a problem and pose.
 -  `brain-wall-download-sources`: Download the source code submitted by
    participants.
 -  `brain-wall-estimate-epsilon`: Try to find a reasonable _eps_ value for a
    problem.
 -  `brain-wall-gallery`: Generates an HTML gallery of the best solutions per
    problem.
 -  `brain-wall-generate-bonus`: A utility to assign some random bonuses.
 -  `brain-wall-generate-figure`: Generates a random figure.
 -  `brain-wall-generate-hole`: Generates a random hole.
 -  `brain-wall-insert-problems`: Submits problems to the database so they will
    show up in the web portal.
 -  `brain-wall-jasper-gen`: Fairly dumb problem generator.
 -  `brain-wall-jasper-solver`: Fairly dumb solver.
 -  `brain-wall-judge`: CLI utility to judge a pose.
 -  `brain-wall-parse-svg`: Takes an SVG that has a elements with IDs `hole` and
    `figure` and tries to turn this into a problem.
    `brain-wall-prosecutor`: Worker to judge solutions in the database.
 -  `brain-wall-random-many`: Generate multiple random problems.
 -  `brain-wall-random-problem`: Generate a random problem.
 -  `brain-wall-render-svg`: Render an SVG for a problem (and possibly a pose).
 -  `brain-wall-reset-password`: Reset the password for a participant.
 -  `brain-wall-scoreboard`: Renders the scoreboard in plain text.
 -  `brain-wall-texture`: Texturing experiment for planar figures.
 -  `brain-wall-web`: Worker to run the web portal.

Most of these have an `-h` flag to show supported options.

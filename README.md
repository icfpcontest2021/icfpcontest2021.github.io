# IFCP Contest 2021

The repository contains almost all of the source code used to run ICFP Contest
2021.  There are a number of components:

 -  [`website/`](#website): This is the source for the website at
    <https://icfpcontest2021.github.io>.
 -  [`spec/`](#spec): The source code of the specification, as Pandoc markdown.
 -  `problems/`: The problems released during the contest.
 -  [`toolchain/`](#toolchain): The toolchain contains the bulk of our code.
    It is a Haskell project containing the database, judge, webserver, and
    numerous other utilities.
 -  [`infra/`](#infra): Some deployment stuff, most importantly contains a nix
    file to build docker images that we can deploy in a pretty
    straightforward way.

## Website

### Development

This site only uses a couple standard utilities, like `bash`, `sed`, `date`,
`make`.  It should work out of the box on most linux systems.

To build the website, simply use:

    make website

This produces a `_site` directory with static files.  You can preview those
directly using a browser, e.g.:

    firefox website/_site/index.html

Or you can use a simple HTTP server, e.g:

    python -m http.server --directory website/_site

## Spec

### Building

    make spec

## Toolchain

### Building and installing

    make toolchain
    
### Database

For some tools, you need to have a Postgres instance running somewhere.
First, create a database, then run the migrations that set up the schema.
For example:

```bash
sudo createuser jasper                            # set up local user
createdb icfpc                                    # create database
for i in toolchain/db/*; do psql icfpc <$i; done  # apply migrations
```

### Configuring

 -  `BRAINWALL_PG_CONNECTION_STRING`: where to find the database.
 -  `BRAINWALL_BIND_PORT`: port to serve the web app on.
 -  `BRAINWALL_BIND_ADDRESS`: address to listen on for the web app.

### Tools

After installing, you should have a number of executables in your `$PATH`.

Noteworthy:

 -  `brain-wall-web`: Worker to run the web portal.
 -  `brain-wall-prosecutor`: Worker to judge solutions in the database.
 -  `brain-wall-judge`: CLI utility to judge a pose.
 -  `brain-wall-render-svg`: Render an SVG for a problem (and possibly a pose).
 -  `brain-wall-parse-svg`: Takes an SVG that has a elements with IDs `hole` and
    `figure` and tries to turn this into a problem.
 -  `brain-wall-random-problem`: Generate a random problem.

Miscellaneous:

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
 -  `brain-wall-random-many`: Generate multiple random problems.
 -  `brain-wall-reset-password`: Reset the password for a participant.
 -  `brain-wall-scoreboard`: Renders the scoreboard in plain text.
 -  `brain-wall-texture`: Texturing experiment for planar figures.

Most of these have an `-h` flag to show supported options.

## Infra

### Building

    make images

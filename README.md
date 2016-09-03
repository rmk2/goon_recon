GoonRecon
=========

## Quick setup

This is a Racket project. Install Racket and follow these steps to run it:

```
# Link the EVE Racket library correctly
raco link --name eve --user lib

# Install dependencies
raco pkg install grommet
raco pkg install net-jwt
raco pkg install xexpr-path

# Run webserver
racket servlets/dashboard.rkt
```

If you would like to use the dscan parsing/reporting functionality implemented
as well, you will have to specify a directory where gzipped dscan data can be
saved, as well as a switch to actually enable storing dscans on disk. The
webroot directory can also be provided by setting the EVEROOT environment
variable instead. Priority goes to command-line parameter before environmental
variable before current directory:

```
# Run webserver with dscan persistence
racket servlets/dashboard.rkt --webroot </path/to/directory> --persist-dscan
```

## Partial database setup

Running `eve-sql_init.rkt` will create all necessary database tables *that do
not require to be bootstrapped*. This script will also query CCP's XML APIv2
alliance endpoint for a current list of all existing alliances; it is safe to
re-run this script, as it will not overwrite any data other than alliances in
`customAlliances`. Due to limitations in data that is publicly obtainable, it
is strongly recommended to import an existing list of corporations as
`customCorporations` into the database. Similarly, all eve utilities rely on a
partial set of tables from CCP's Static Data Export (SDE) in order to retrieve
and resolve static inventory and map data. The required tables are:

```
invCategories
invGroups
invTypes
mapConstellations
mapDenormalize
mapRegions
mapSolarSystems
```

EVE player Steve Ronuken (CSM 9 & 10 Member) hosts individual tables as well
as full conversions to a number of formats, including mySQL, on his website,
which is recommended especially since CCP started shipping the SDE as YAML
files, rather than direct SQL exports as they used to in the past. Tables can
be found here: [fuzzwork.co.uk](https://www.fuzzwork.co.uk/dump/latest/)

**Note:** If you are using Allister's bootstrapped data, be aware that it uses
a stripped-down version of mapDenormalize that removes cartesian coordinates
in order to save some database space, which does not influence any of the
scripts listed below.

## Automated intel gathering

**Hint:** I would suggest adding the whole `src` directory to the executing
  user's PATH to be able to simply call of the utilities directly, since they
  use a shebang to call racket internally.

### Update moon-database with recently killed towers

We can use `eve-digest.rkt` to query zkillboard.com for a list of dead towers
which are subsequently marked accordingly in our moon-database, which we will
need to pipe to `eve-towers_sql.rkt` in order to automatically save them to
our database backend:

```
# Long options
eve-digest.rkt --losses --raw --moons --group "Control Tower" | eve-towers_sql.rkt
```

There is an alternative form that uses short options, as well:

```
# Alternative short options
eve-digest.rkt -lRm -g "Control Tower" | eve-towers_sql.rkt
```

It is easiest to simply schedule running the parser via cron, where hourly
runs probably constitute a happy medium. Additionally, `eve-digest.rkt` offers
additional options for manual querying for dates other than the current date,
such as manual start and end dates, or start and end killIDs; for further
information, check `eve-digest.rkt --help`. We are running `--losses` only
because we only care about dead towers, not things killed *by* a tower; we are
using `--raw` because we need to pipe the input into another racket utility
which expects raw lists, and we are using `--moons` to properly parse not just
a killmail's solar system, but its precise moon location.

### Update citadel-database with recently killed citadels

Much like for towers, a similar system exists to indicate dead citadels,
though some of the inherent limitations in how citadels can be attributed and
identified, this will mark all citadels of a given type in a given system as
needing rescan if *and* citadel of that type dies in a scanned system. Once
again, we are using `eve-digest.rkt` to query zkillboard.com, this time piping
the output to another utility which deals with updating the appropriate
database tables:

```
# Long options
eve-digest.rkt --losses --raw --location --group "Citadel" | eve-citadels_sql.rkt

# Alternative short options
eve-digest.rkt -lRM -g "Citadel" | eve-citadels_sql.rkt
```

### Update sovereignty timers

Simply schedule `eve-crest.rkt` to run via cron, which will query CCP's CREST
sovereignty campaign endpoint for current as well as future sovereignty
timers. This script only pulls and parses timers itself and does not include
tracking the relative resulsts of attackers and defenders when a timer is
active.

### Update killmails that feature Supercarriers/Titans

Schedule `eve-digest.rkt --cron` to run via cron, preferably every few hours (or
simply hourly). The option `--cron` marks a shortcut that sets all required
options for continuous automated killmail parsing, only requesting killmails
with a killID greather than the last killID found in the respective database.

### Update Supercarrier/Titan pilot affiliation

Run (or schedule) `eve-api_check.rkt` periodically, though daily or even
weekly should be enough. This queries CCP's XML APIv2 affiliation endpoint for
each distinct Supercarrier/Titan pilot in our dataset, saving their respective
current corporation and alliance at the time of the poll.

### Update unknown corporations for automated tower kills

Run (or schedule) `eve-api_corporations.rkt` periodically, which tries to
resolve unknown corporations if we either have a valid alliance for them (in
the case of partially entered data from a moon scan) or, ideally, if we simply
have a corporationID, as is the case with data automatically retrieved from
zkillboard.com.

### Update all corporation affiliations

Run (or schedule) `eve-api_corporation-crawl.rkt` periodically, which goes
through *every* existing alliance and requests *all* corporations that are
part of each alliance from the official API. Since CCP supplies a full list of
existing alliances, this means that we, in turn, are able to generate a full
mapping of corporations to alliances. This script will take a while as it
loops through alliances and thus needs to honour CCP's imposed rate limits. If
you would like additional output, run it via `racket -W debug
src/eve-api_corporation-crawl.rkt`, which will show you on which iterative
step inside the loop we currently are. If this script fails for some reason
(API dies, CCP hates you etc.), you can rerun it and it will pick up where it
was before.

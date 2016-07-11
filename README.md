GoonRecon
=========

## Quick setup

This is a Racket project. Install Racket and follow these steps to run it:

```
# Link the EVE Racket library correctly
raco link --name eve --user lib

# Install dependencies
raco pkg install grommet
raco pkg install xexpr-path

# Run webserver
racket servlets/dashboard.rkt
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
invGroups
invTypes
mapConstellations
mapDenormalize
mapRegions
mapSolarSystems
```

EVE player Steve Ronuken (CSM 9 & 10 Member) hosts individual tables as well
as full conversions to a number of formats, including mySQL, on his website,
which is recommend especially since CCP started shipping the SDE as YAML
files, rather than direct SQL exports as they used to in the past. Tables can
be found here: [fuzzwork.co.uk](https://www.fuzzwork.co.uk/dump/latest/)

**Note:** If you are using Allister's bootstrapped data, be aware that it uses
a stripped-down version of mapDenormalize that removes cartesian coordinates
in order to save some database space, which does not influence any of the
scripts listed below.

## Automated intel gathering

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
eve-digest.rkt -LRm -g "Control Tower" | eve-towers_sql.rkt
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

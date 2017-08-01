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
raco pkg install scrypt
raco pkg install tasks
raco pkg install webapi
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

## Database connections

In order for database connectivity to work, the database user's password has
to be supplied via an environment variable. The second variable is used as an
encryption key for user refresh tokens, so choose something proper (and
ideally something that is randomly generated):

```
# Providing MySQL secrets, replace as appropriate
export MYSQL_PASSWORD="foo"
export MYSQL_KEY="bar"
```

The same password should be set in MySQL like so (you might have to log in as
the root SQL user, depending on your database setup and permissions). The app
expects the MySQL username to be `eve`:

```
# Set MySQL username and password, replace password as appropriate
GRANT ALL PRIVILEGES ON eve_sde.* TO 'eve'@'localhost' IDENTIFIED BY 'foo'
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


## Authentication

Over time, different authentication mechanisms have been added in order to
assure a maximum of flexibility in deploying the webserver. Depending on the
overall use-case (single-user, small trusted group, bigger deployment),
different options can be selected either individually or together.

### User authentication

#### User data and security

Since we are ultimately handling user names, passwords and email addresses, we
try to keep them as save as possible. Passwords are hashed using scrypt before
being stored in the database, while CCP refresh tokens are encrypted at-rest
within the database using AES256. There are provisions to make use of CCP's
single-sign on, though a module to completely defer all authentication
handling is currently disabled.

#### Basic authentication

This module can be actived by supplying `--basic-auth`, `--auth` or `-a` to
`dashboard.rkt`. This uses basic HTTP authentication via browser built-ins,
which should be robust and easy. Downsides are that passwords are kept in
clear text in the browser itself (this is by design and holds true for all
basic HTTP auth) and that logins do not persist across sessions when the
browser is closed. As an upside, this could be combined with externally
authenticating users even via a proxy webserver.

#### Login authentication

As an alternative to the above basic authentication, supplying `--login-auth`,
`--login` or `-l` to `dashboard.rkt` instead enables an internal login page
that supports some additional options for registration and logging in. For a
stand-alone multi-user deployment, this is probably what you want. Login
authentication uses JSON Web Tokens (JWT) in cookies to retain login
information. As such, these option _can_ be used to supply external
authentication info as long as they are signed with the correct and
corresponding key data.

These parameters should be supplied via environmental variables:

```
# Providing JWT secrets, replace as appropriate
export JWT_SECRET="foo"
export JWT_ISSUER="bar"
export JWT_AUDIENCES="baz"
```

#### SSO authentication

The options `--sso-auth`, `--sso` or `-s` require users to validate their
chosen username against CCP's single-sign-on (SSO) system, which means that
only valid EVE characters can be registered. This is recommended in bigger
deployments where it might otherwise be difficult to map arbitrary login names
to specific people. Since there is no way of knowing whether a given character
is a "main" character or not from the API, getting people to use sensible and
recognisable character names (or login names, for that matter, if you choose
to forego this option) is your problem! ;)

In order for this option to work, you will need to register your own SSO app
credentials with CCP supply your information via environmental variables
afterwards:

```
# Providing SSO credentials, replace as appropriate
export SSO_TOKEN_ID="0123456789"
export SSO_TOKEN_SECRET="foo"
export SSO_TOKEN_REDIRECT="http://localhost:8000/register"
```

### Group authentication

In addition to authenticating individual users, the webserver includes
provision for group management. If you enable this feature via `--group-auth`,
`--group` or `-g`, different user groups will have access to different subsets
based on their groups. Group management is included for administrators and
(only for those) available under `/management/groups`. Group management also
actively blocks even administrators from "disowning" the apps owner(s), which
have to be set manually in SQL (all other groups for registered users apart
from owners can be set via the above URL). This is best done by registering a
regular user and by then using the following query inside your SQL client of
choice after having changed to the eve database:

```
# Set owner in MySQL, replace username as appropriate
use eve_sde;
UPDATE authBasicGroups SET groupID = 1024 WHERE user = <USERNAME>
```

Available groups and their numerical shortcuts can be queried like so:

```
# Query all available user groups
use eve_sde;
SELECT * FROM authGroups;

# Example user groups
MariaDB [eve_sde]> SELECT * FROM authGroups;
+---------+-------------+
| groupID | groupName   |
+---------+-------------+
|       1 | public      |
|       4 | alliance    |
|       8 | corporation |
|      32 | recon       |
|      64 | recon-l     |
|     256 | admin       |
|    1024 | owner       |
+---------+-------------+
7 rows in set (0.00 sec)
```

### Recommendation

I suggest using `--group-auth --login-auth`, though these require a bit of
additional setup as described above. Yet, `--group-auth --login-auth
--sso-auth` is perhaps the most comprehensive set of option, though SSO auth
requires to register an app properly with CCP.

**Hint:** There exist other options which can be seen when calling
`dashboard.rkt --help`, though these should _not_ be used in production. As
such, `-j` allows the webserver to create valid JWT headers for _all_
requests, which means every request gets displayed as fully
authenticated. Additionally, `-G` allows to supply one of the above groups
directly, which treats all requests as having those permissions, which (in
combination with `-j`) is very useful for testing what individual groups can
and cannot see, but is obviously pointless for production. If you don't want
group authentication in production, don't use `--group-auth`! 

## Automated intel gathering w/ monolithic dispatcher

The easiest way to keep various information in our database up-to-date is to
use the new-ish task dispatcher `eve-task_dispatcher.rkt`. This script
provides a convenient way to update alliance/corporation/character data as
well as super/citadel/tower data in the background. It replaces running
the individual tools listed below and makes the use of cron obsolete. It
provides two threads by default and logs its activity to stdout, while the
script also provides what I think are sensible default scheduling times. The
script is self-sufficient and does not require any additional configuration.

### Calling the dispatcher as a systemd service

I recommend simply running the dispatcher as a systemd service, by creating a
new service file. I supply the MYSQL_PASSWORD used for the generic eve
database as an environmental variable directly in the service file, though
systemd also supports separate env data. On my system, these go into
`/usr/lib/systemd/system/`, and this example uses `eve-dispatcher.service` as
filename:

```
# Systemd service file

[Unit]
Description=EVE Online dispatcher to continuously update API data

[Service]
Environment=MYSQL_PASSWORD=<SQL-PASSWORD>
ExecStart=<PATH-TO-goon-recon>/src/eve-task_dispatcher.rkt
KillMode=process
Restart=on-failure
User=<USER>
Group=users

[Install]
WantedBy=multi-user.target

# Activate the service

systemctl enable eve-dispatcher.service
```

If this route is taken, then the dispatcher's log information can conveniently
be queried via systemd's `journalctl` (again using the above example
filename):

```
# Accessing loggging

journalctl -u eve-dispatcher.service
```

## Automated intel gathering w/ individual tools

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
though due to some of the inherent limitations in how citadels can be
attributed and identified, this will mark all citadels of a given type in a
given system as needing a rescan if *any* citadel of that type dies in a
scanned system. Once again, we are using `eve-digest.rkt` to query
zkillboard.com, this time piping the output to another utility which deals
with updating the appropriate database tables:

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

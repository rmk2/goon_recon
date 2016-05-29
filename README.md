GoonRecon
=========

## Quick setup

This is a Racket project. Install Racket and follow these steps to run it:

```
# Link the EVE Racket library correctly
raco link --name eve --user lib

# Install dependencies
raco pkg install sxml
raco pkg install xexpr-path

# Run webserver
racket servlets/dashboard.rkt
```

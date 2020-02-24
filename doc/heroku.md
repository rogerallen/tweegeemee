Heroku Notes
------------

Currently using Heroku CLI.

See usage in "Deploy" section of the app.

To reproduce the heroku environment, do:

```
source profile.sh
lein with-profile production compile :all
lein trampoline run
```

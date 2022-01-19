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

Merging git repos...

heroku git:clone -a tweegeemee-production
cd tweegeemee-production
git remote add github git@github.com:rogerallen/tweegeemee.git

git fetch github
git merge github/master

(edits)

git push github

Push to heroku via:

 git push heroku master

see remotes with

 git remote -v

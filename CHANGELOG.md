# Revision history for vectercise

## 0.1.0.0 -- YYYY-mm-dd

* First version. Released on an unsuspecting world.




https://docs.github.com/en/get-started/getting-started-with-git/managing-remote-repositories#switching-remote-urls-from-ssh-to-https

## Switching remote URLs from HTTPS to SSH

Open Bash.
Change the current working directory to your local project.
List your existing remotes in order to get the name of the remote you want to change.

`git remote -v`
origin  https://github.com/USERNAME/REPOSITORY.git (fetch)
origin  https://github.com/USERNAME/REPOSITORY.git (push)

Change your remote's URL from HTTPS to SSH with the git remote set-url command.
`git remote set-url origin git@github.com:USERNAME/REPOSITORY.git`

git remote set-url origin git@github.com:mandober/vectercise.git


Verify that the remote URL has changed.
$ git remote -v

# Verify new remote URL

origin  git@github.com:USERNAME/REPOSITORY.git (fetch)
origin  git@github.com:USERNAME/REPOSITORY.git (push)

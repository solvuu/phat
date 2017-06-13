#/bin/bash

# Print git commit to stdout in OCaml notation.

git_commit=`git rev-parse HEAD 2> /dev/null`
if [ "$?" -eq "0" ]
then
  echo "Some \"$git_commit\""
else
  echo "None"
fi

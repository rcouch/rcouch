#!/bin/sh -e

# Licensed under the Apache License, Version 2.0 (the "License"); you may not
# use this file except in compliance with the License. You may obtain a copy of
# the License at
#
#   http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
# WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
# License for the specific language governing permissions and limitations under
# the License.

cd `dirname $0`
export CURRENT=`pwd`

# create contributor list
if test -e .git; then
    OS=`uname -s`
    case "$OS" in
    Linux|CYGWIN*) # GNU sed
        SED_ERE_FLAG=-r
    ;;
    *) # BSD sed
        SED_ERE_FLAG=-E
    ;;
    esac

    sed -e "/^#.*/d" THANKS.in > THANKS
    CONTRIB_EMAIL_SED_COMMAND="s/^[[:blank:]]{5}[[:digit:]]+[[:blank:]]/ * /"
    git shortlog -se 6c976bd..HEAD \
        | grep -v @apache.org \
        | sed $SED_ERE_FLAG -e "$CONTRIB_EMAIL_SED_COMMAND" >> THANKS
    echo "" >> THANKS # simplest portable newline
    echo "For a list of authors see the \`AUTHORS\` file." >> THANKS
    cp THANKS $CURRENT/THANKS
fi

find_program() {
  set +e
  for f in "$@"
  do
    file=`which ${f} 2>/dev/null | grep -v '^no '`
    if test -n "x${file}" -a -x "${file}"
    then
      echo ${file}
      set -e
      exit 0
    fi
  done

  echo "Unable to find any variant: $@" 1>&2
  echo 1>&2
  echo "Have you installed a version of this package?" 1>&2
  set -e
  exit 1
}

cat << EOF
You have bootstrapped Apache CouchDB, time to relax.

Run \`make' to build the source before you install.
EOF

HEREDIR=`dirname $0`
cd "$HEREDIR"

stack setup && stack build && stack exec project-exe

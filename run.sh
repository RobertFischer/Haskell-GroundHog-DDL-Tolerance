HEREDIR=`dirname $0`
cd "$HEREDIR"

stack build && stack exec project-exe

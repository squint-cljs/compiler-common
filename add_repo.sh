# https://github.com/nextjournal/nextjournal/pull/5906
# https://stackoverflow.com/a/43345686
# usage:
# git-add-repo https://github.com/example/example dir/to/save
# NOTES
# - installed gnu-sed (brew install gnu-sed)
# - original script used lowercase `path` variable which clobbered PATH,
#   changed to `workingDir`
function git-add-repo
{
    repo="$1"
    dir="$(echo "$2" | sed 's/\/$//')"
    workingDir="$(pwd)"

    tmp="$(mktemp -d)"
    remote="$(echo "$tmp" | sed 's/\///g'| sed 's/\./_/g')"

    git clone "$repo" "$tmp"
    cd "$tmp"

    git filter-branch --index-filter '
        git ls-files -s |
        sed "s,\t,&'"$dir"'/," |
        GIT_INDEX_FILE="$GIT_INDEX_FILE.new" git update-index --index-info &&
        mv "$GIT_INDEX_FILE.new" "$GIT_INDEX_FILE"
    ' HEAD

    cd "$workingDir"
    git remote add -f "$remote" "file://$tmp/.git"
    git pull "$remote/main"
    git merge --allow-unrelated-histories -m "Merge repo $repo into main" --edit "$remote/main"
    git remote remove "$remote"
    rm -rf "$tmp"
}

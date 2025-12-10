

gb () {
  git branch $* | cat
}

gl1 () {
  git log -1 $* | cat
}
gl2 () {
  git log -2 $* | cat
}

gl3 () {
  git log -3 $* | cat
}

_dotgitbase() {
    v=$(git rev-parse --absolute-git-dir 2>/dev/null)
    echo "$v"
    return
    local _curdir="$1"
    if [[ "$1" == "" ]]; then
        _curdir=`pwd`
    fi
    if [[ ! -e "$_curdir" ]]; then
        return
    fi

    local _rp=$(readlink -f "$_curdir")

    if [[ "$_rp" == "/" ]]; then
        return
    fi

    if [[ -d "$_rp"/.git ]]; then
        echo "$_rp"/.git
        return
    fi

    _dotgitbase "$_rp"/..
}

_ggcurhead() {
    local _hf="$(_dotgitbase)/HEAD"
    if [[ -f "$_hf" ]]; then
        #read _ref < $_hf
        _ref=`cat $_hf` 
        if [[ "$1" == 'v' ]]; then
            echo " <${_ref#ref: refs/heads/}> "
        else
            echo ${_ref#ref: refs/heads/}
        fi
    fi
}

_ggcurb() {
    v=$(git rev-parse --git-dir 2>/dev/null)
    if [[ $? -eq 0 ]]; then
        echo $v
    fi
}

_ggcurh() {
    v=$(git rev-parse --abbrev-ref HEAD 2>/dev/null)
    if [[ $? -eq 0 ]]; then
        echo $v
    fi
}

ggrsth() {
    git reset --hard HEAD~$*
}

ggps() {
    if [[ "$*" == "" ]]; then
        git push origin HEAD:refs/for/`_ggcurhead`
        return
    fi
    git push origin HEAD:refs/for/$*
}

ggpl() {
    if [[ "$*" == "" ]]; then
        git fetch origin `_ggcurhead`
        git rebase FETCH_HEAD
        return
    fi
    git fetch origin $*
    git rebase FETCH_HEAD
    #git pull -r origin $*
}

ggpc() {
    if [[ "$*" == "" ]]; then
        git fetch origin `_ggcurhead`
        git rebase FETCH_HEAD
        return
    fi
    git fetch origin $*
    git cherry-pick FETCH_HEAD
    #git pull -r origin $*
}

gdh1() {
    git diff HEAD~1 HEAD "$*"
}


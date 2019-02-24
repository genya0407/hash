#!/bin/zsh
set -ue

TEMPDIR=testtmp

function assert() {
    set +ue
    echo $1 | PROMPT="" stack run > $TEMPDIR/result
    zsh -c $1 > $TEMPDIR/correct
    set -ue
    cmp --silent $TEMPDIR/result $TEMPDIR/correct
    RET=$?
    if [ $RET -eq 0 ] ;then
        echo "Success: $1"
    else
        echo "Failed: $1"
        diff $TEMPDIR/correct tmp
        exit $RET
    fi
}

function assert_side_effect() {
    set +ue
    echo $1 | PROMPT="" stack run
    zsh -c $2
    set -ue
    zsh -c $3
    RET=$?
    if [ $RET -eq 0 ] ;then
        echo "Success: $1"
    else
        echo "Failed: $1"
        exit $RET
    fi
}

function setup() {
    echo "setup"
    if [ -d $TEMPDIR ] ;then
        clean
    fi
    mkdir $TEMPDIR
}

function clean() {
    echo "clean"
    rm -rf $TEMPDIR
}

echo "starting test"
setup
assert "ls"
assert "ls | grep R"
assert "ls && echo fuga"
assert "ls || echo hoge"
assert "ls; echo hoge"
assert_side_effect "ls > $TEMPDIR/hoge" "ls > $TEMPDIR/fuga" "cmp --silent $TEMPDIR/hoge $TEMPDIR/fuga"
assert_side_effect "ls | grep R > $TEMPDIR/hoge" "ls | grep R > $TEMPDIR/fuga" "cmp --silent $TEMPDIR/hoge $TEMPDIR/fuga"
assert_side_effect "cat notexistfile 2> $TEMPDIR/hoge" "cat notexistfile 2> $TEMPDIR/fuga" "cmp --silent $TEMPDIR/hoge $TEMPDIR/fuga"
clean
echo "finishing test"
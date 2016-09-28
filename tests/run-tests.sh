#!sh

if [ raco make tests/main.rkt -eq 0 ]; then
    if [ racket tests/main.rkt 2> log.err -ne 0]; then
        $EDITOR log.err
    fi
fi

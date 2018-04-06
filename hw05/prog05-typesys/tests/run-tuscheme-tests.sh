#!/usr/bin/env bash

for x in a b c d e f g h i j k l m n o p q r s t u v w; do
    p=$((cd ./tests; ls test-B:${x}:*.scm) | head -n 1 | sed -E "s|test-B:${x}:[0-9]+-(.*)[.]scm|\1|")
    echo $p

    pass='yes'
    for t in $((cd ./tests; ls test-B:${x}:*.scm | sort -n) | sed -E 's/(.*)[.]scm/\1/'); do
        cat "./tests/$t.scm" | ./tuscheme -q 1> "$t.outerr" 2>&1
        if grep -q "type error" "$t.outerr"; then
            mv "$t.outerr" "$t.tyerr"
        else
            mv "$t.outerr" "$t.tychk"
        fi
        if [ -f ./tests/$t.soln.tychk ]; then
            if [ -f $t.tychk ]; then
                if cmp --silent ./tests/$t.soln.tychk $t.tychk; then
                    echo "$t PASS (soln.tychk == tychk)"
                else
                    echo "$t FAIL (soln.tychk <> tychk)"
                    cat $t.tychk
                    pass='no'
                fi
            else
                echo "$t FAIL (soln.tychk; tyerr)"
                cat $t.tyerr
                pass='no'
            fi
        else
            if [ -f $t.tyerr ]; then
                echo "$t PASS (soln.tyerr; tyerr)"
            else
                echo "$t FAIL (soln.tyerr; tychk)"
                cat $t.tychk
                pass='no'
            fi
        fi
        rm -f "$t.tychk" "$t.tyerr"
    done
    if [ $pass = 'yes' ]; then
        echo "$p PASS"
    else
        echo "$p FAIL"
    fi
    
done

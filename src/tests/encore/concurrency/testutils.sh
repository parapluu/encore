STDIN=$(cat)

function FAIL {
    MSG=$1
    echo "ERROR: $1"
    exit 1
}

# Requires that an error was reported in a specific line
function error_at {
    LINE=$1
    (echo "$STDIN" | grep -A1 "(line $LINE, column" > /dev/null) || FAIL "expected error on line $LINE"
}

# Requires that an error was reported with a specific message
function error_msg {
    MSG=$1
    (echo "$STDIN" | grep "$MSG" > /dev/null) || FAIL "expected error with message '$MSG'"
}

#!/bin/bash

echo "Running detailed moon test check..."
echo ""

# 检查 multiply function
echo "Checking multiply function..."
echo "Looking for pattern: 'a \* b' in multiply function"
MULTIPLY_CONTENT=$(grep -A100 "pub fn multiply" src/azimuth/lib.mbt)
if echo "$MULTIPLY_CONTENT" | grep -q "a \* b"; then
    echo "✓ PASS: Found 'a * b' in multiply function"
    MULTIPLY_ERROR=0
else
    echo "✗ FAIL: Did not find 'a * b' in multiply function"
    MULTIPLY_ERROR=1
fi

echo ""
echo "Multiply function content (last 5 lines):"
echo "$MULTIPLY_CONTENT" | tail -5

echo ""
echo "Checking greet function..."
echo "Looking for pattern: '\"!\"' in greet function"
GREET_CONTENT=$(grep -A20 "pub fn greet" src/azimuth/lib.mbt)
if echo "$GREET_CONTENT" | grep -q '"!"'; then
    echo "✓ PASS: Found exclamation mark in greet function"
    GREET_ERROR=0
else
    echo "✗ FAIL: Did not find exclamation mark in greet function"
    GREET_ERROR=1
fi

echo ""
echo "Greet function content:"
echo "$GREET_CONTENT"

echo ""
echo "Summary:"
echo "Multiply check: $([ $MULTIPLY_ERROR -eq 0 ] && echo 'PASS' || echo 'FAIL')"
echo "Greet check: $([ $GREET_ERROR -eq 0 ] && echo 'PASS' || echo 'FAIL')"

TOTAL_ERRORS=$((MULTIPLY_ERROR + GREET_ERROR))
echo "Total errors: $TOTAL_ERRORS"

exit $TOTAL_ERRORS
#!/bin/bash

echo "Testing original moon script..."

# 测试乘法检查
echo "Testing multiplication check:"
if grep -A100 "pub fn multiply" src/azimuth/lib.mbt | grep -q "a \* b"; then
    echo "✓ Found 'a * b' in multiply function"
else
    echo "✗ Missing 'a * b' in multiply function"
fi

# 测试感叹号检查
echo "Testing exclamation mark check:"
if grep -A20 "pub fn greet" src/azimuth/lib.mbt | grep -q '"!"'; then
    echo "✓ Found exclamation mark in greet function"
else
    echo "✗ Missing exclamation mark in greet function"
fi

# 查看具体的匹配内容
echo ""
echo "Checking multiply function content:"
grep -A100 "pub fn multiply" src/azimuth/lib.mbt | grep "a \* b" || echo "No match found"

echo ""
echo "Checking greet function content:"
grep -A20 "pub fn greet" src/azimuth/lib.mbt | grep '"!"' || echo "No match found"
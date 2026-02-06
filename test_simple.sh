#!/bin/bash
echo "Creating a simple test for our concise essential tests..."

# Create a simple test file to verify our tests work
cat > test_simple_concise.mbt << 'EOF'
// 简单测试验证
test "verify_simple_addition" {
  assert_eq(add(2, 3), 5)
}

test "verify_simple_greeting" {
  assert_eq_string(greet("Test"), "Hello, Test!")
}
EOF

echo "Running test with our simple test file..."
./moon test test_simple_concise.mbt
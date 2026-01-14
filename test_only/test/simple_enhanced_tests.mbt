// 简单的增强MoonBit测试用例

// 测试用例1: 零值运算
test "zero_value_operations" {
  @test_only.assert_eq(42, @test_only.add(42, 0))
  @test_only.assert_eq(0, @test_only.multiply(42, 0))
}

// 测试用例2: 单位元运算
test "identity_operations" {
  @test_only.assert_eq(42, @test_only.multiply(42, 1))
  @test_only.assert_eq(-42, @test_only.multiply(42, -1))
}

// 测试用例3: 极值运算
test "extreme_values" {
  let max_val = 2147483647
  let min_val = -2147483648
  @test_only.assert_eq(-1, @test_only.add(max_val, min_val))
  @test_only.assert_eq(min_val, @test_only.multiply(max_val, min_val))
}

// 测试用例4: 字符串处理
test "string_processing" {
  @test_only.assert_eq_string("Hello, World!", @test_only.greet("World"))
  @test_only.assert_eq_string("Hello, !", @test_only.greet(""))
}

// 测试用例5: 数学恒等式
test "mathematical_identities" {
  let a = 7
  let b = 3
  let c = 5
  let left = @test_only.multiply(a, @test_only.add(b, c))
  let right = @test_only.add(@test_only.multiply(a, b), @test_only.multiply(a, c))
  @test_only.assert_eq(left, right)
}
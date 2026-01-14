// 简单的 MoonBit 测试用例

test "simple_add_test" {
  @test_only.assert_eq(@test_only.add(2, 3), 5)
}

test "simple_multiply_test" {
  @test_only.assert_eq(@test_only.multiply(2, 3), 6)
}

test "simple_greet_test" {
  @test_only.assert_eq_string(@test_only.greet("World"), "Hello, World!")
}
// 简单的测试用例
test "simple_addition" {
  @test_only.assert_eq(5, @test_only.add(2, 3))
}

test "simple_multiplication" {
  @test_only.assert_eq(6, @test_only.multiply(2, 3))
}

test "simple_greet" {
  @test_only.assert_eq_string("Hello, World!", @test_only.greet("World"))
}

test "add_boundary" {
  let max_val = 2147483647
  @test_only.assert_eq(max_val, @test_only.add(max_val, 1))
}

test "multiply_boundary" {
  let max_val = 2147483647
  @test_only.assert_eq(max_val, @test_only.multiply(max_val, 1))
}

test "negative_operations" {
  @test_only.assert_eq(-10, @test_only.add(-3, -7))
  @test_only.assert_eq(15, @test_only.multiply(-3, -5))
}

test "compound_operations" {
  let result = @test_only.add(@test_only.add(1, 2), @test_only.add(3, 4))
  @test_only.assert_eq(10, result)
}

test "string_edge_cases" {
  @test_only.assert_eq_string("Hello, John Doe!", @test_only.greet("John Doe"))
}

test "math_identities" {
  let a = 123
  let b = 456
  @test_only.assert_eq(@test_only.add(a, b), @test_only.add(b, a))
}

test "real_world_scenario" {
  let price = 25
  let quantity = 4
  let total = @test_only.multiply(price, quantity)
  @test_only.assert_eq(100, total)
}
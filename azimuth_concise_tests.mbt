// Azimuth 项目简洁测试用例
// 使用标准 MoonBit 测试语法，包含10个精简测试用例

test "add_basic" {
  // 基本加法测试
  assert_eq(5, add(2, 3))
  assert_eq(0, add(-2, 2))
  assert_eq(-5, add(-2, -3))
}

test "multiply_basic" {
  // 基本乘法测试
  assert_eq(6, multiply(2, 3))
  assert_eq(-6, multiply(2, -3))
  assert_eq(0, multiply(2, 0))
}

test "greet_basic" {
  // 基本问候测试
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, 测试!", greet("测试"))
}

test "add_boundary" {
  // 边界值加法测试
  let max_val = 2147483647
  let min_val = -2147483648
  assert_eq(max_val, add(max_val, 1))
  assert_eq(min_val, add(min_val, -1))
  assert_eq(-1, add(max_val, min_val))
}

test "multiply_boundary" {
  // 边界值乘法测试
  let max_val = 2147483647
  let min_val = -2147483648
  assert_eq(0, multiply(max_val, 0))
  assert_eq(min_val, multiply(min_val, -1))
  assert_eq(max_val, multiply(max_val, 1))
}

test "complex_calculation" {
  // 复合计算测试
  let a = 5
  let b = 3
  let c = 2
  let result = add(multiply(a, b), c)
  assert_eq(17, result)
}

test "string_operations" {
  // 字符串操作测试
  let name1 = "Alice"
  let name2 = "Bob"
  assert_eq_string("Hello, Alice!", greet(name1))
  assert_eq_string("Hello, Bob!", greet(name2))
  assert_eq_string("Hello, Alice Bob!", greet(name1 + " " + name2))
}

test "mathematical_properties" {
  // 数学性质测试
  let a = 7
  let b = 11
  // 加法交换律
  assert_eq(add(a, b), add(b, a))
  // 乘法交换律
  assert_eq(multiply(a, b), multiply(b, a))
}

test "real_world_scenario" {
  // 实际应用场景：购物车计算
  let price = 50
  let quantity = 3
  let discount = 10
  let total = add(multiply(price, quantity), -discount)
  assert_eq(140, total)
}

test "assert_functions" {
  // 断言函数测试
  assert_true(true)
  assert_false(false)
  assert_eq(42, 42)
  assert_eq_string("test", "test")
}
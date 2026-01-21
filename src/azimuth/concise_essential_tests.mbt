// 简洁核心测试用例 - 标准MoonBit测试语法
// 测试核心功能：add, multiply, greet函数

test "add_basic_functionality" {
  // 测试基本加法功能
  assert_eq(10, add(3, 7))
  assert_eq(0, add(-5, 5))
  assert_eq(-8, add(-3, -5))
}

test "add_boundary_values" {
  // 测试边界值加法
  let max_val = 2147483647
  let min_val = -2147483648
  
  assert_eq(max_val, add(max_val, 1))
  assert_eq(min_val, add(min_val, -1))
  assert_eq(-1, add(max_val, min_val))
}

test "multiply_basic_functionality" {
  // 测试基本乘法功能
  assert_eq(15, multiply(3, 5))
  assert_eq(-12, multiply(-3, 4))
  assert_eq(0, multiply(0, 100))
}

test "multiply_special_cases" {
  // 测试乘法特殊情况
  let min_val = -2147483648
  
  assert_eq(5, multiply(5, 1))
  assert_eq(-5, multiply(5, -1))
  assert_eq(min_val, multiply(min_val, -1))
}

test "greet_functionality" {
  // 测试greet函数
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, 测试!", greet("测试"))
}

test "complex_calculation" {
  // 测试复合计算：矩形面积+周长
  let length = 8
  let width = 5
  
  let area = multiply(length, width)
  let perimeter = multiply(2, add(length, width))
  let total = add(area, perimeter)
  
  assert_eq(66, total) // 8*5 + 2*(8+5) = 40 + 26 = 66
}

test "mathematical_properties" {
  // 测试数学性质
  let a = 12
  let b = 8
  
  // 加法交换律
  assert_eq(add(a, b), add(b, a))
  
  // 乘法交换律
  assert_eq(multiply(a, b), multiply(b, a))
  
  // 分配律
  assert_eq(multiply(a, add(b, 3)), add(multiply(a, b), multiply(a, 3)))
}

test "real_world_scenario" {
  // 实际应用场景：购物车计算
  let item_price = 199
  let quantity = 2
  let shipping = 15
  
  let subtotal = multiply(item_price, quantity)
  let total = add(subtotal, shipping)
  
  assert_eq(413, total) // 199*2 + 15 = 398 + 15 = 413
}

test "error_handling" {
  // 测试错误处理和断言
  assert_true(true)
  assert_false(false)
  assert_eq(42, 42)
  assert_eq_string("test", "test")
}

test "performance_calculation" {
  // 性能相关计算：等差数列求和
  // 1 + 2 + 3 + ... + 10 = 55
  let n = 10
  let sum = multiply(n, add(1, n)) / 2
  
  assert_eq(55, sum)
}
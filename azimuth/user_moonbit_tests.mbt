// Azimuth 项目用户 MoonBit 测试用例
// 使用标准 MoonBit 测试语法

test "add_function_basic" {
  // 测试 add 函数的基本功能
  assert_eq(5, add(2, 3))
  assert_eq(0, add(-2, 2))
  assert_eq(-5, add(-2, -3))
  assert_eq(10, add(0, 10))
  assert_eq(-10, add(0, -10))
}

test "multiply_function_basic" {
  // 测试 multiply 函数的基本功能
  assert_eq(6, multiply(2, 3))
  assert_eq(-6, multiply(2, -3))
  assert_eq(0, multiply(5, 0))
  assert_eq(0, multiply(0, 5))
  assert_eq(1, multiply(1, 1))
  assert_eq(-1, multiply(1, -1))
}

test "greet_function_basic" {
  // 测试 greet 函数的基本功能
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, A!", greet("A"))
}

test "assertion_functions" {
  // 测试断言函数
  assert_true(true)
  assert_false(false)
  assert_eq(42, 42)
  assert_eq(-42, -42)
  assert_eq(0, 0)
  assert_eq_string("Hello", "Hello")
  assert_eq_string("", "")
}

test "mathematical_properties" {
  // 测试数学运算的性质
  // 加法交换律
  assert_eq(add(7, 11), add(11, 7))
  assert_eq(add(-5, 15), add(15, -5))
  
  // 乘法交换律
  assert_eq(multiply(7, 11), multiply(11, 7))
  assert_eq(multiply(2, -3), multiply(-3, 2))
  
  // 乘法分配律
  assert_eq(multiply(2, add(3, 4)), add(multiply(2, 3), multiply(2, 4)))
}

test "edge_cases" {
  // 测试边界情况
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试溢出保护
  assert_eq(max_val, add(max_val, 1))
  assert_eq(min_val, add(min_val, -1))
  assert_eq(max_val, multiply(max_val, 2))
  assert_eq(min_val, multiply(min_val, -1))
  
  // 测试极值相加
  assert_eq(-1, add(max_val, min_val))
}

test "complex_calculations" {
  // 测试复杂计算
  // 计算：(2 + 3) × 4 - 5
  let result1 = add(multiply(add(2, 3), 4), -5)
  assert_eq(15, result1)
  
  // 计算：10 × (5 + 3) ÷ 2
  let result2 = multiply(10, add(5, 3)) / 2
  assert_eq(40, result2)
  
  // 计算：平方和 3² + 4²
  let result3 = add(multiply(3, 3), multiply(4, 4))
  assert_eq(25, result3)
}

test "real_world_applications" {
  // 测试实际应用场景
  // 购物车计算
  let item1 = 199
  let item2 = 299
  let item3 = 99
  let discount = 50
  let subtotal = add(add(item1, item2), item3)
  let total = add(subtotal, -discount)
  assert_eq(547, total)
  
  // 面积计算
  let length = 8
  let width = 6
  let area = multiply(length, width)
  assert_eq(48, area)
}
// 标准 Azimuth 核心测试用例
// 使用标准的 MoonBit 测试语法

test "add_function_basic_test" {
  // 测试加法函数的基本功能
  assert_eq(10, add(3, 7))
  assert_eq(0, add(0, 0))
  assert_eq(-5, add(-2, -3))
  assert_eq(100, add(50, 50))
}

test "multiply_function_basic_test" {
  // 测试乘法函数的基本功能
  assert_eq(15, multiply(3, 5))
  assert_eq(0, multiply(0, 10))
  assert_eq(-6, multiply(-2, 3))
  assert_eq(25, multiply(5, 5))
}

test "greet_function_standard_test" {
  // 测试问候函数的标准情况
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, !", greet(""))
}

test "divide_with_ceil_standard_test" {
  // 测试向上取整除法的标准情况
  assert_eq(4, divide_with_ceil(10, 3))
  assert_eq(3, divide_with_ceil(9, 3))
  assert_eq(1, divide_with_ceil(1, 100))
  assert_eq(0, divide_with_ceil(5, 0))
}

test "mathematical_commutative_test" {
  // 测试数学交换律
  let a = 8
  let b = 12
  assert_eq(add(a, b), add(b, a))
  assert_eq(multiply(a, b), multiply(b, a))
}

test "mathematical_identity_test" {
  // 测试数学单位元性质
  assert_eq(7, add(7, 0))
  assert_eq(7, multiply(7, 1))
  assert_eq(0, multiply(7, 0))
}

test "negative_operations_test" {
  // 测试负数运算
  assert_eq(-10, add(-3, -7))
  assert_eq(15, multiply(-3, -5))
  assert_eq(-3, divide_with_ceil(-10, 3))
  assert_eq(4, divide_with_ceil(-10, -3))
}

test "complex_expressions_test" {
  // 测试复杂算术表达式
  let result = add(multiply(2, 3), multiply(4, 5))
  assert_eq(26, result)
  
  let result2 = multiply(add(5, 3), divide_with_ceil(10, 3))
  assert_eq(32, result2)
}

test "real_world_scenarios_test" {
  // 测试实际应用场景
  let item1_price = 25
  let item1_qty = 3
  let item2_price = 15
  let item2_qty = 2
  let total = add(multiply(item1_price, item1_qty), multiply(item2_price, item2_qty))
  assert_eq(105, total)
  
  let total_items = 23
  let items_per_box = 5
  let boxes_needed = divide_with_ceil(total_items, items_per_box)
  assert_eq(5, boxes_needed)
}

test "string_greeting_test" {
  // 测试字符串处理与问候函数
  let first_name = "John"
  let last_name = "Doe"
  let full_name = first_name + " " + last_name
  let greeting = greet(full_name)
  assert_eq_string("Hello, John Doe!", greeting)
  
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, 123!", greet("123"))
}
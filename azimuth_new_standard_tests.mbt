// Azimuth 项目新增标准测试用例
// 使用标准 MoonBit 测试语法，包含断言验证

test "addition_edge_cases" {
  // 测试加法边缘情况
  assert_eq(0, add(0, 0))
  assert_eq(5, add(5, 0))
  assert_eq(-3, add(-3, 0))
  assert_eq(100, add(50, 50))
}

test "multiplication_edge_cases" {
  // 测试乘法边缘情况
  assert_eq(0, multiply(0, 5))
  assert_eq(0, multiply(5, 0))
  assert_eq(1, multiply(1, 1))
  assert_eq(-25, multiply(-5, 5))
}

test "subtraction_functionality" {
  // 测试减法功能（假设有subtract函数）
  assert_eq(2, subtract(5, 3))
  assert_eq(-2, subtract(3, 5))
  assert_eq(0, subtract(5, 5))
  assert_eq(10, subtract(0, -10))
}

test "division_functionality" {
  // 测试除法功能（假设有divide函数）
  assert_eq(5, divide(10, 2))
  assert_eq(0, divide(0, 5))
  assert_eq(-3, divide(-9, 3))
  assert_eq(1, divide(7, 7))
}

test "string_concatenation" {
  // 测试字符串连接功能
  assert_eq_string("HelloWorld", concat("Hello", "World"))
  assert_eq_string("Test", concat("", "Test"))
  assert_eq_string("Test", concat("Test", ""))
  assert_eq_string("", concat("", ""))
}

test "array_operations" {
  // 测试数组操作（假设有array_length和array_get函数）
  let arr = [1, 2, 3, 4, 5]
  assert_eq(5, array_length(arr))
  assert_eq(1, array_get(arr, 0))
  assert_eq(5, array_get(arr, 4))
}

test "boolean_operations" {
  // 测试布尔操作
  assert_eq(true, and(true, true))
  assert_eq(false, and(true, false))
  assert_eq(true, or(true, false))
  assert_eq(false, or(false, false))
  assert_eq(false, not(true))
  assert_eq(true, not(false))
}

test "comparison_operations" {
  // 测试比较操作
  assert_eq(true, equal(5, 5))
  assert_eq(false, equal(5, 3))
  assert_eq(true, greater_than(10, 5))
  assert_eq(false, greater_than(5, 10))
  assert_eq(true, less_than(3, 7))
  assert_eq(false, less_than(7, 3))
}

test "error_handling" {
  // 测试错误处理（假设有safe_divide函数）
  assert_eq(5, safe_divide(10, 2))
  assert_eq(0, safe_divide(10, 0))  // 除零时返回0
  assert_eq(-3, safe_divide(-9, 3))
}

test "complex_business_logic" {
  // 测试复杂业务逻辑：计算折扣价格
  let price = 100
  let discount_rate = 20  // 20%折扣
  let tax_rate = 8       // 8%税率
  
  let discounted_price = multiply(price, subtract(100, discount_rate)) / 100
  let final_price = add(discounted_price, multiply(discounted_price, tax_rate) / 100)
  
  assert_eq(86, final_price)  // 100 * 0.8 = 80, 80 * 1.08 = 86.4，取整为86
}
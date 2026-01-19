// Azimuth 项目综合测试用例
// 使用标准 MoonBit 测试语法，涵盖多种测试场景

test "integer_arithmetic_operations" {
  // 测试整数算术运算
  assert_eq(15, add(7, 8))
  assert_eq(-3, add(5, -8))
  assert_eq(0, add(10, -10))
  
  assert_eq(20, multiply(4, 5))
  assert_eq(-12, multiply(3, -4))
  assert_eq(0, multiply(0, 100))
}

test "string_concatenation_operations" {
  // 测试字符串连接操作
  assert_eq_string("HelloWorld", concat("Hello", "World"))
  assert_eq_string("123456", concat("123", "456"))
  assert_eq_string("EmptyEmpty", concat("Empty", "Empty"))
  assert_eq_string("", concat("", ""))
}

test "boolean_logic_operations" {
  // 测试布尔逻辑运算
  assert_true(is_even(4))
  assert_true(is_even(0))
  assert_false(is_even(3))
  assert_false(is_even(-1))
  
  assert_true(is_positive(10))
  assert_false(is_positive(-5))
  assert_false(is_positive(0))
}

test "array_operations" {
  // 测试数组操作
  let arr1 = [1, 2, 3, 4, 5]
  let arr2 = [10, 20, 30]
  
  assert_eq(15, sum_array(arr1))
  assert_eq(60, sum_array(arr2))
  assert_eq(5, array_length(arr1))
  assert_eq(3, array_length(arr2))
}

test "boundary_value_testing" {
  // 测试边界值
  let max_int = 2147483647
  let min_int = -2147483648
  
  assert_eq(max_int, add(max_int, 0))
  assert_eq(min_int, add(min_int, 0))
  assert_eq(1, add(max_int, min_int + 1))
  
  assert_eq(max_int, multiply(max_int, 1))
  assert_eq(min_int, multiply(min_int, 1))
  assert_eq(0, multiply(min_int, 0))
}

test "error_handling_scenarios" {
  // 测试错误处理场景
  assert_eq(0, safe_divide(10, 0))
  assert_eq(5, safe_divide(10, 2))
  assert_eq(-3, safe_divide(-9, 3))
  
  assert_eq("Invalid", safe_get(["a", "b"], 5))
  assert_eq("a", safe_get(["a", "b"], 0))
}

test "mathematical_functions" {
  // 测试数学函数
  assert_eq(4, square(2))
  assert_eq(9, square(-3))
  assert_eq(0, square(0))
  
  assert_eq(16, power(2, 4))
  assert_eq(1, power(5, 0))
  assert_eq(27, power(3, 3))
}

test "data_conversion_operations" {
  // 测试数据转换操作
  assert_eq_string("123", int_to_string(123))
  assert_eq_string("-456", int_to_string(-456))
  assert_eq_string("0", int_to_string(0))
  
  assert_eq(42, string_to_int("42"))
  assert_eq(-17, string_to_int("-17"))
  assert_eq(0, string_to_int("invalid"))
}

test "complex_business_logic" {
  // 测试复杂业务逻辑
  // 计算订单总价：商品价格 × 数量 + 运费 - 折扣
  let price = 100
  let quantity = 3
  let shipping = 20
  let discount = 30
  
  let subtotal = multiply(price, quantity)
  let total = add(subtract(subtotal, discount), shipping)
  
  assert_eq(290, total)
}
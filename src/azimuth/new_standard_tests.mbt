// 新增 MoonBit 测试用例
// 覆盖 azimuth 项目核心功能

test "add_basic_operations" {
  // 测试基本加法操作
  assert_eq(5, add(2, 3))
  assert_eq(0, add(-2, 2))
  assert_eq(-5, add(-2, -3))
}

test "add_overflow_handling" {
  // 测试加法溢出处理
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 正数溢出
  assert_eq(max_val, add(max_val, 1))
  assert_eq(max_val, add(1000000000, 1500000000))
  
  // 负数溢出
  assert_eq(min_val, add(min_val, -1))
  assert_eq(min_val, add(-1500000000, -1000000000))
}

test "multiply_basic_operations" {
  // 测试基本乘法操作
  assert_eq(6, multiply(2, 3))
  assert_eq(-6, multiply(2, -3))
  assert_eq(6, multiply(-2, -3))
  assert_eq(0, multiply(5, 0))
}

test "multiply_edge_cases" {
  // 测试乘法边界情况
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 乘以1和-1
  assert_eq(max_val, multiply(max_val, 1))
  assert_eq(min_val, multiply(min_val, 1))
  assert_eq(min_val, multiply(min_val, -1))
  
  // 溢出情况
  assert_eq(max_val, multiply(46341, 46341))
}

test "greet_function_tests" {
  // 测试greet函数
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
}

test "complex_calculation_tests" {
  // 测试复杂计算
  // 计算矩形周长：2 × (长 + 宽)
  let length = 10
  let width = 5
  let perimeter = multiply(2, add(length, width))
  assert_eq(30, perimeter)
  
  // 计算平均数
  let a = 80
  let b = 90
  let c = 85
  let average = add(add(a, b), c) / 3
  assert_eq(85, average)
}

test "mathematical_properties_test" {
  // 测试数学性质
  let x = 7
  let y = 11
  
  // 加法交换律
  assert_eq(add(x, y), add(y, x))
  
  // 乘法交换律
  assert_eq(multiply(x, y), multiply(y, x))
  
  // 分配律
  assert_eq(multiply(x, add(y, 3)), add(multiply(x, y), multiply(x, 3)))
}

test "real_world_application_tests" {
  // 测试实际应用场景
  // 购物车计算
  let item_price = 199
  let quantity = 3
  let shipping = 20
  let total = add(multiply(item_price, quantity), shipping)
  assert_eq(617, total)
  
  // 温度转换（简化版）：F = C × 2 + 30
  let celsius = 25
  let fahrenheit = add(multiply(celsius, 2), 30)
  assert_eq(80, fahrenheit)
}

test "string_processing_tests" {
  // 测试字符串处理
  // 包含特殊字符
  assert_eq_string("Hello, test@123!", greet("test@123"))
  assert_eq_string("Hello, 用户!", greet("用户"))
  
  // 长字符串
  let long_name = "VeryLongNameForTesting"
  assert_eq_string("Hello, VeryLongNameForTesting!", greet(long_name))
}

test "boundary_value_combination_tests" {
  // 测试边界值组合
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 边界值运算
  assert_eq(-1, add(max_val, min_val))
  assert_eq(0, multiply(max_val, 0))
  assert_eq(0, multiply(min_val, 0))
  
  // 连续运算
  let result = add(multiply(2, add(3, 4)), 5)
  assert_eq(19, result)
}
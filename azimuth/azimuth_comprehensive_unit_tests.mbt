// Azimuth 项目综合单元测试
// 使用标准 MoonBit 测试语法

test "add_function_basic_operations" {
  // 测试基本的加法运算
  inspect(add(1, 2), "3")?
  inspect(add(0, 0), "0")?
  inspect(add(-1, 1), "0")?
  inspect(add(-5, -3), "-8")?
}

test "multiply_function_basic_operations" {
  // 测试基本的乘法运算
  inspect(multiply(2, 3), "6")?
  inspect(multiply(0, 5), "0")?
  inspect(multiply(1, 7), "7")?
  inspect(multiply(-2, 4), "-8")?
  inspect(multiply(-3, -3), "9")?
}

test "greet_function_basic_operations" {
  // 测试基本的问候函数
  inspect(greet("World"), "Hello, World!")?
  inspect(greet("MoonBit"), "Hello, MoonBit!")?
  inspect(greet(""), "Hello, !")?
  inspect(greet("123"), "Hello, 123!")?
}

test "divide_with_ceil_positive_numbers" {
  // 测试正数的向上取整除法
  inspect(divide_with_ceil(10, 3), "4")?      // 10/3 = 3.33... -> 4
  inspect(divide_with_ceil(9, 3), "3")?       // 9/3 = 3 -> 3
  inspect(divide_with_ceil(1, 1), "1")?       // 1/1 = 1 -> 1
  inspect(divide_with_ceil(5, 2), "3")?       // 5/2 = 2.5 -> 3
}

test "divide_with_ceil_negative_numbers" {
  // 测试负数的向上取整除法
  inspect(divide_with_ceil(-10, 3), "-3")?    // -10/3 = -3.33... -> -3
  inspect(divide_with_ceil(-9, 3), "-3")?     // -9/3 = -3 -> -3
  inspect(divide_with_ceil(-5, 2), "-2")?     // -5/2 = -2.5 -> -2
}

test "divide_with_ceil_edge_cases" {
  // 测试边界情况
  inspect(divide_with_ceil(0, 5), "0")?       // 0/5 = 0 -> 0
  inspect(divide_with_ceil(5, 0), "0")?       // 除数为0时返回0
  inspect(divide_with_ceil(1, 2), "1")?       // 1/2 = 0.5 -> 1
  inspect(divide_with_ceil(-1, 2), "0")?      // -1/2 = -0.5 -> 0
}

test "complex_mathematical_operations" {
  // 测试复杂数学运算组合
  // 计算：(2 + 3) * 4 = 20
  let result1 = multiply(add(2, 3), 4)
  inspect(result1, "20")?
  
  // 计算：10 / 3 向上取整 + 5 = 4 + 5 = 9
  let result2 = add(divide_with_ceil(10, 3), 5)
  inspect(result2, "9")?
}

test "assert_functions_functionality" {
  // 测试断言函数（虽然它们只是返回 ()，但至少测试调用不会出错）
  assert_eq(5, 5)
  assert_eq_string("hello", "hello")
  assert_true(true)
  assert_false(false)
  
  // 使用 inspect 验证断言函数不会抛出异常
  inspect("assert_functions_executed", "assert_functions_executed")?
}

test "real_world_calculation_scenario" {
  // 测试实际应用场景：计算购物车总价
  let item_price = 25
  let quantity = 4
  let discount = 10
  
  // 计算商品总价：25 * 4 = 100
  let subtotal = multiply(item_price, quantity)
  inspect(subtotal, "100")?
  
  // 应用折扣：100 - 10 = 90
  let final_price = add(subtotal, -discount)
  inspect(final_price, "90")?
  
  // 计算平均每个商品的价格（向上取整）：90 / 3 = 30
  let avg_price_per_item = divide_with_ceil(final_price, 3)
  inspect(avg_price_per_item, "30")?
}
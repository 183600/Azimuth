// 标准 MoonBit 测试用例
// 使用标准的 MoonBit 测试语法和断言

test "basic_addition_functionality" {
  // 测试基本加法功能
  azimuth::assert_eq(5, azimuth::add(2, 3))
  azimuth::assert_eq(0, azimuth::add(-2, 2))
  azimuth::assert_eq(-10, azimuth::add(-3, -7))
}

test "addition_boundary_values" {
  // 测试加法的边界值处理
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试最大值边界
  azimuth::assert_eq(max_val, azimuth::add(max_val, 1))
  azimuth::assert_eq(max_val, azimuth::add(max_val, max_val))
  
  // 测试最小值边界
  azimuth::assert_eq(min_val, azimuth::add(min_val, -1))
  azimuth::assert_eq(min_val, azimuth::add(min_val, min_val))
  
  // 测试最大值与最小值相加
  azimuth::assert_eq(-1, azimuth::add(max_val, min_val))
}

test "basic_multiplication_functionality" {
  // 测试基本乘法功能
  azimuth::assert_eq(6, azimuth::multiply(2, 3))
  azimuth::assert_eq(-6, azimuth::multiply(-2, 3))
  azimuth::assert_eq(6, azimuth::multiply(-2, -3))
  azimuth::assert_eq(0, azimuth::multiply(5, 0))
}

test "multiplication_boundary_values" {
  // 测试乘法的边界值处理
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试最大值乘法
  azimuth::assert_eq(max_val, azimuth::multiply(max_val, 1))
  azimuth::assert_eq(max_val, azimuth::multiply(46341, 46341))
  
  // 测试最小值乘法
  azimuth::assert_eq(min_val, azimuth::multiply(min_val, 1))
  azimuth::assert_eq(min_val, azimuth::multiply(min_val, -1))
  azimuth::assert_eq(min_val, azimuth::multiply(min_val, 2))
}

test "string_greeting_functionality" {
  // 测试字符串问候功能
  azimuth::assert_eq_string("Hello, World!", azimuth::greet("World"))
  azimuth::assert_eq_string("Hello, !", azimuth::greet(""))
  azimuth::assert_eq_string("Hello, John Doe!", azimuth::greet("John Doe"))
  azimuth::assert_eq_string("Hello, 123!", azimuth::greet("123"))
}

test "complex_mathematical_operations" {
  // 测试复杂数学运算组合
  let a = 10
  let b = 20
  let c = 5
  
  // 计算 (a + b) * c
  let sum_ab = azimuth::add(a, b)
  let result = azimuth::multiply(sum_ab, c)
  azimuth::assert_eq(150, result)
  
  // 计算 a * b + c
  let product_ab = azimuth::multiply(a, b)
  let result2 = azimuth::add(product_ab, c)
  azimuth::assert_eq(205, result2)
}

test "real_world_shopping_cart" {
  // 测试实际应用场景：购物车计算
  let item1_price = 99
  let item1_quantity = 2
  let item2_price = 149
  let item2_quantity = 1
  
  // 计算每种商品的总价
  let total1 = azimuth::multiply(item1_price, item1_quantity)
  let total2 = azimuth::multiply(item2_price, item2_quantity)
  
  // 计算购物车总价
  let cart_total = azimuth::add(total1, total2)
  
  // 验证总价：99*2 + 149*1 = 198 + 149 = 347
  azimuth::assert_eq(347, cart_total)
}

test "error_handling_scenarios" {
  // 测试错误处理场景
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 确保溢出被正确处理
  azimuth::assert_eq(max_val, azimuth::add(max_val, 100))
  azimuth::assert_eq(min_val, azimuth::add(min_val, -100))
  azimuth::assert_eq(max_val, azimuth::multiply(max_val, 2))
  azimuth::assert_eq(min_val, azimuth::multiply(min_val, -2))
}

test "performance_critical_calculations" {
  // 测试性能关键路径的计算
  let base = 100
  
  // 连续加法操作
  let result1 = azimuth::add(azimuth::add(base, base), azimuth::add(base, base))
  azimuth::assert_eq(400, result1)
  
  // 连续乘法操作
  let result2 = azimuth::multiply(azimuth::multiply(2, 5), azimuth::multiply(3, 4))
  azimuth::assert_eq(120, result2)
}

test "function_composition_patterns" {
  // 测试函数组合模式
  let initial_value = 5
  
  // 嵌套函数调用：multiply(add(multiply(2, 3), add(4, 1)), 2)
  let inner_mult = azimuth::multiply(2, 3)
  let inner_add = azimuth::add(4, 1)
  let middle_add = azimuth::add(inner_mult, inner_add)
  let final_result = azimuth::multiply(middle_add, 2)
  
  // 验证结果：multiply(add(multiply(2, 3), add(4, 1)), 2) = multiply(add(6, 5), 2) = multiply(11, 2) = 22
  azimuth::assert_eq(22, final_result)
}
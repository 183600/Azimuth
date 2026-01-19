// 标准 MoonBit 测试用例
// 测试 azimuth 模块的核心功能

// 导入 azimuth 模块
use azimuth

test "add_function_basic_functionality" {
  // 测试 add 函数的基本功能
  assert_eq(10, azimuth.add(3, 7))
  assert_eq(0, azimuth.add(5, -5))
  assert_eq(-3, azimuth.add(-8, 5))
}

test "add_function_edge_cases" {
  // 测试 add 函数的边界情况
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试零值
  assert_eq(5, azimuth.add(5, 0))
  assert_eq(5, azimuth.add(0, 5))
  assert_eq(0, azimuth.add(0, 0))
  
  // 测试溢出保护
  assert_eq(max_val, azimuth.add(max_val, 1))
  assert_eq(min_val, azimuth.add(min_val, -1))
}

test "multiply_function_basic_functionality" {
  // 测试 multiply 函数的基本功能
  assert_eq(15, azimuth.multiply(3, 5))
  assert_eq(-12, azimuth.multiply(3, -4))
  assert_eq(12, azimuth.multiply(-3, -4))
}

test "multiply_function_edge_cases" {
  // 测试 multiply 函数的边界情况
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试零值和一值
  assert_eq(0, azimuth.multiply(5, 0))
  assert_eq(5, azimuth.multiply(5, 1))
  assert_eq(-5, azimuth.multiply(5, -1))
  
  // 测试溢出保护
  assert_eq(max_val, azimuth.multiply(46341, 46341))
  assert_eq(min_val, azimuth.multiply(min_val, -1))
}

test "greet_function_various_inputs" {
  // 测试 greet 函数的各种输入
  assert_eq_string("Hello, World!", azimuth.greet("World"))
  assert_eq_string("Hello, !", azimuth.greet(""))
  assert_eq_string("Hello, MoonBit!", azimuth.greet("MoonBit"))
  assert_eq_string("Hello, 测试!", azimuth.greet("测试"))
}

test "complex_calculation_scenarios" {
  // 测试复杂计算场景
  // 计算矩形面积和周长
  let length = 8
  let width = 5
  
  let area = azimuth.multiply(length, width)
  let perimeter = azimuth.add(azimuth.multiply(2, length), azimuth.multiply(2, width))
  
  assert_eq(40, area)
  assert_eq(26, perimeter)
}

test "mathematical_properties" {
  // 测试数学性质
  let a = 12
  let b = 7
  
  // 加法交换律
  assert_eq(azimuth.add(a, b), azimuth.add(b, a))
  
  // 乘法交换律
  assert_eq(azimuth.multiply(a, b), azimuth.multiply(b, a))
  
  // 分配律
  let c = 3
  assert_eq(
    azimuth.multiply(a, azimuth.add(b, c)),
    azimuth.add(azimuth.multiply(a, b), azimuth.multiply(a, c))
  )
}

test "real_world_application" {
  // 测试实际应用场景：购物车计算
  let item_price = 199
  let quantity = 3
  let shipping = 15
  let tax_rate = 10
  
  // 计算商品总价
  let subtotal = azimuth.multiply(item_price, quantity)
  // 计算税费
  let tax = azimuth.multiply(subtotal, tax_rate) / 100
  // 计算最终总价
  let total = azimuth.add(azimuth.add(subtotal, tax), shipping)
  
  assert_eq(674, total)
}

test "boundary_value_combinations" {
  // 测试边界值组合
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试极值组合
  assert_eq(-1, azimuth.add(max_val, min_val))
  assert_eq(0, azimuth.multiply(max_val, 0))
  assert_eq(min_val, azimuth.multiply(min_val, 1))
  
  // 测试连续运算
  let result = azimuth.add(
    azimuth.multiply(1000, 2),
    azimuth.multiply(500, 3)
  )
  assert_eq(3500, result)
}

test "string_processing_edge_cases" {
  // 测试字符串处理的边界情况
  // 测试包含特殊字符的字符串
  assert_eq_string("Hello, @#$%!", azimuth.greet("@#$%"))
  assert_eq_string("Hello, 12345!", azimuth.greet("12345"))
  assert_eq_string("Hello, spaces   !", azimuth.greet("spaces  "))
  
  // 测试长字符串
  let long_string = "ThisIsAVeryLongStringForTestingPurposes"
  assert_eq_string("Hello, " + long_string + "!", azimuth.greet(long_string))
}
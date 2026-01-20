// 标准 MoonBit 测试用例
// 覆盖 azimuth 项目的主要功能

test "add_function_basic" {
  // 测试基本加法功能
  inspect(add(2, 3), "5")?
  inspect(add(0, 0), "0")?
  inspect(add(-2, -3), "-5")?
}

test "add_function_overflow_handling" {
  // 测试加法溢出处理
  inspect(add(2147483647, 1), "2147483647")?
  inspect(add(-2147483648, -1), "-2147483648")?
}

test "multiply_function_basic" {
  // 测试基本乘法功能
  inspect(multiply(2, 3), "6")?
  inspect(multiply(0, 5), "0")?
  inspect(multiply(1, 5), "5")?
  inspect(multiply(2, -3), "-6")?
}

test "multiply_function_overflow_handling" {
  // 测试乘法溢出处理
  inspect(multiply(46341, 46341), "2147483647")?
  inspect(multiply(-2147483648, -1), "-2147483648")?
}

test "greet_function_various_inputs" {
  // 测试 greet 函数的各种输入
  inspect(greet("World"), "Hello, World!")?
  inspect(greet(""), "Hello, !")?
  inspect(greet("MoonBit"), "Hello, MoonBit!")?
  inspect(greet("测试"), "Hello, 测试!")?
}

test "complex_calculation_scenario" {
  // 测试复杂计算场景
  // 计算矩形周长：2 × (长 + 宽)
  let length = 10
  let width = 5
  let perimeter = multiply(2, add(length, width))
  inspect(perimeter, "30")?
  
  // 计算三角形面积：底 × 高 ÷ 2
  let base = 8
  let height = 6
  let area = multiply(base, height) / 2
  inspect(area, "24")?
}

test "mathematical_properties" {
  // 测试数学性质
  let a = 7
  let b = 11
  
  // 加法交换律
  let sum1 = add(a, b)
  let sum2 = add(b, a)
  inspect(sum1, sum2.to_string())?
  
  // 乘法交换律
  let product1 = multiply(a, b)
  let product2 = multiply(b, a)
  inspect(product1, product2.to_string())?
}

test "real_world_application" {
  // 测试实际应用场景：购物车计算
  let item_price = 199
  let quantity = 3
  let shipping = 15
  let tax_rate = 8
  
  // 计算商品总价
  let subtotal = multiply(item_price, quantity)
  // 计算税费
  let tax = multiply(subtotal, tax_rate) / 100
  // 计算最终总价
  let total = add(add(subtotal, tax), shipping)
  
  inspect(total, "674")?
}

test "edge_cases_and_special_values" {
  // 测试边界情况和特殊值
  // 测试零值运算
  inspect(multiply(0, 2147483647), "0")?
  inspect(multiply(0, -2147483648), "0")?
  inspect(add(0, 2147483647), "2147483647")?
  inspect(add(0, -2147483648), "-2147483648")?
  
  // 测试单位值运算
  inspect(multiply(2147483647, 1), "2147483647")?
  inspect(multiply(-2147483648, 1), "-2147483648")?
  inspect(multiply(1, 2147483647), "2147483647")?
  inspect(multiply(1, -2147483648), "-2147483648")?
}

test "string_processing_edge_cases" {
  // 测试字符串处理的边界情况
  // 测试长字符串
  let long_name = "ThisIsAVeryLongNameForTestingPurposes"
  inspect(greet(long_name), "Hello, ThisIsAVeryLongNameForTestingPurposes!")?
  
  // 测试包含特殊字符的字符串
  let special_name = "test@example.com"
  inspect(greet(special_name), "Hello, test@example.com!")?
  
  // 测试包含数字的字符串
  let numeric_name = "user123"
  inspect(greet(numeric_name), "Hello, user123!")?
}
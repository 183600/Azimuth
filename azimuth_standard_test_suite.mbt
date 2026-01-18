// Azimuth 项目标准测试用例集
// 使用标准 MoonBit 测试语法，测试核心功能

test "add_function_basic_tests" {
  // 测试 add 函数的基本功能
  let result1 = azimuth::add(5, 10)
  azimuth::assert_eq(15, result1)
  
  let result2 = azimuth::add(-3, 7)
  azimuth::assert_eq(4, result2)
  
  let result3 = azimuth::add(0, 0)
  azimuth::assert_eq(0, result3)
}

test "multiply_function_basic_tests" {
  // 测试 multiply 函数的基本功能
  let result1 = azimuth::multiply(4, 6)
  azimuth::assert_eq(24, result1)
  
  let result2 = azimuth::multiply(-5, 3)
  azimuth::assert_eq(-15, result2)
  
  let result3 = azimuth::multiply(7, 0)
  azimuth::assert_eq(0, result3)
}

test "greet_function_basic_tests" {
  // 测试 greet 函数的基本功能
  let result1 = azimuth::greet("Alice")
  azimuth::assert_eq_string("Hello, Alice!", result1)
  
  let result2 = azimuth::greet("Bob")
  azimuth::assert_eq_string("Hello, Bob!", result2)
  
  let result3 = azimuth::greet("世界")
  azimuth::assert_eq_string("Hello, 世界!", result3)
}

test "mathematical_commutativity" {
  // 测试数学交换律
  let a = 12
  let b = 8
  
  // 加法交换律
  let add_result1 = azimuth::add(a, b)
  let add_result2 = azimuth::add(b, a)
  azimuth::assert_eq(add_result1, add_result2)
  
  // 乘法交换律
  let mul_result1 = azimuth::multiply(a, b)
  let mul_result2 = azimuth::multiply(b, a)
  azimuth::assert_eq(mul_result1, mul_result2)
}

test "arithmetical_identity_properties" {
  // 测试算术恒等性质
  let x = 42
  
  // 加法恒等元
  let add_identity = azimuth::add(x, 0)
  azimuth::assert_eq(x, add_identity)
  
  // 乘法恒等元
  let mul_identity = azimuth::multiply(x, 1)
  azimuth::assert_eq(x, mul_identity)
  
  // 乘法零元
  let mul_zero = azimuth::multiply(x, 0)
  azimuth::assert_eq(0, mul_zero)
}

test "complex_calculation_scenarios" {
  // 测试复杂计算场景
  // 场景1：购物车总价计算
  let item1_price = 25
  let item2_price = 35
  let item3_price = 15
  let tax = 10
  
  let subtotal = azimuth::add(azimuth::add(item1_price, item2_price), item3_price)
  let total = azimuth::add(subtotal, tax)
  azimuth::assert_eq(85, total)
  
  // 场景2：面积计算
  let length = 8
  let width = 6
  let area = azimuth::multiply(length, width)
  azimuth::assert_eq(48, area)
}

test "string_formatting_edge_cases" {
  // 测试字符串格式化的边界情况
  // 空字符串
  let result1 = azimuth::greet("")
  azimuth::assert_eq_string("Hello, !", result1)
  
  // 包含特殊字符
  let result2 = azimuth::greet("test@example.com")
  azimuth::assert_eq_string("Hello, test@example.com!", result2)
  
  // 包含空格的名称
  let result3 = azimuth::greet("John Doe")
  azimuth::assert_eq_string("Hello, John Doe!", result3)
}

test "negative_number_operations" {
  // 测试负数运算
  let neg1 = -10
  let neg2 = -20
  let pos = 15
  
  // 负数加法
  let result1 = azimuth::add(neg1, neg2)
  azimuth::assert_eq(-30, result1)
  
  // 正负数加法
  let result2 = azimuth::add(neg1, pos)
  azimuth::assert_eq(5, result2)
  
  // 负数乘法
  let result3 = azimuth::multiply(neg1, neg2)
  azimuth::assert_eq(200, result3)
  
  // 正负数乘法
  let result4 = azimuth::multiply(neg1, pos)
  azimuth::assert_eq(-150, result4)
}

test "calculation_chain_validation" {
  // 测试计算链验证
  // 计算：(a * b) + (c * d)
  let a = 3
  let b = 4
  let c = 5
  let d = 6
  
  let product1 = azimuth::multiply(a, b)  // 12
  let product2 = azimuth::multiply(c, d)  // 30
  let result = azimuth::add(product1, product2)  // 42
  
  azimuth::assert_eq(42, result)
  
  // 计算：((x + y) * z) - w
  let x = 10
  let y = 5
  let z = 3
  let w = 7
  
  let sum = azimuth::add(x, y)  // 15
  let product = azimuth::multiply(sum, z)  // 45
  let final_result = azimuth::add(product, -w)  // 38
  
  azimuth::assert_eq(38, final_result)
}

test "data_processing_simulation" {
  // 测试数据处理模拟
  // 模拟温度数据转换：摄氏度转华氏度 F = C * 9/5 + 32
  let celsius = 20
  let fahrenheit = azimuth::add(azimuth::multiply(celsius, 9) / 5, 32)
  azimuth::assert_eq(68, fahrenheit)
  
  // 模拟货币转换：美元转欧元（假设汇率 0.85）
  let dollars = 100
  let euros = azimuth::multiply(dollars, 85) / 100
  azimuth::assert_eq(85, euros)
  
  // 模拟折扣计算
  let original_price = 200
  let discount_rate = 15  // 15%
  let discount_amount = azimuth::multiply(original_price, discount_rate) / 100
  let final_price = azimuth::add(original_price, -discount_amount)
  azimuth::assert_eq(170, final_price)
}
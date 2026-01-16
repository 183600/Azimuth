// Azimuth 项目增强测试套件
// 使用标准 MoonBit 测试语法，包含8个全面的测试用例

test "add_comprehensive_range" {
  // 全面测试加法功能的数值范围
  // 正数加法
  azimuth::assert_eq(100, azimuth::add(50, 50))
  azimuth::assert_eq(1000000, azimuth::add(500000, 500000))
  
  // 负数加法
  azimuth::assert_eq(-100, azimuth::add(-50, -50))
  azimuth::assert_eq(-1000000, azimuth::add(-500000, -500000))
  
  // 零值加法
  azimuth::assert_eq(42, azimuth::add(0, 42))
  azimuth::assert_eq(-42, azimuth::add(-42, 0))
  azimuth::assert_eq(0, azimuth::add(0, 0))
}

test "multiply_comprehensive_range" {
  // 全面测试乘法功能的数值范围
  // 正数乘法
  azimuth::assert_eq(25, azimuth::multiply(5, 5))
  azimuth::assert_eq(1000000, azimuth::multiply(1000, 1000))
  
  // 负数乘法
  azimuth::assert_eq(25, azimuth::multiply(-5, -5))
  azimuth::assert_eq(-25, azimuth::multiply(5, -5))
  azimuth::assert_eq(-25, azimuth::multiply(-5, 5))
  
  // 零值乘法
  azimuth::assert_eq(0, azimuth::multiply(0, 42))
  azimuth::assert_eq(0, azimuth::multiply(42, 0))
  azimuth::assert_eq(0, azimuth::multiply(0, 0))
}

test "greet_function_edge_cases" {
  // 测试 greet 函数的边界情况
  // 空字符串
  azimuth::assert_eq_string("Hello, !", azimuth::greet(""))
  
  // 单字符
  azimuth::assert_eq_string("Hello, A!", azimuth::greet("A"))
  
  // 长字符串
  let long_name = "VeryLongNameThatExceedsNormalExpectations"
  azimuth::assert_eq_string("Hello, VeryLongNameThatExceedsNormalExpectations!", azimuth::greet(long_name))
  
  // 包含数字和字母
  azimuth::assert_eq_string("Hello, User123!", azimuth::greet("User123"))
  
  // 包含特殊符号
  azimuth::assert_eq_string("Hello, Test-Case!", azimuth::greet("Test-Case"))
}

test "mathematical_commutativity" {
  // 测试数学交换律
  let test_values = [0, 1, -1, 42, -42, 100, -100, 1000, -1000]
  
  // 对所有测试值验证加法交换律
  let i = 0
  while i < test_values.length() {
    let j = i + 1
    while j < test_values.length() {
      let a = test_values[i]
      let b = test_values[j]
      let result1 = azimuth::add(a, b)
      let result2 = azimuth::add(b, a)
      azimuth::assert_eq(result1, result2)
      
      let mult_result1 = azimuth::multiply(a, b)
      let mult_result2 = azimuth::multiply(b, a)
      azimuth::assert_eq(mult_result1, mult_result2)
      
      j = j + 1
    }
    i = i + 1
  }
}

test "practical_business_calculations" {
  // 测试实际业务计算场景
  // 场景1：计算总价（单价 × 数量 + 税费）
  let unit_price = 25
  let quantity = 8
  let tax_rate = 20  // 20% 税费
  
  let subtotal = azimuth::multiply(unit_price, quantity)  // 200
  let tax_amount = azimuth::multiply(subtotal, tax_rate) / 100  // 40
  let total_price = azimuth::add(subtotal, tax_amount)  // 240
  azimuth::assert_eq(240, total_price)
  
  // 场景2：计算利润（售价 - 成本）
  let selling_price = 150
  let cost_price = 90
  let profit = azimuth::add(selling_price, -cost_price)
  azimuth::assert_eq(60, profit)
  
  // 场景3：计算折扣后的价格
  let original_price = 200
  let discount = 30  // 30% 折扣
  let discount_amount = azimuth::multiply(original_price, discount) / 100
  let discounted_price = azimuth::add(original_price, -discount_amount)
  azimuth::assert_eq(140, discounted_price)
}

test "extreme_boundary_conditions" {
  // 测试极端边界条件
  let max_int = 2147483647
  let min_int = -2147483648
  
  // 最大值边界测试
  azimuth::assert_eq(max_int, azimuth::add(max_int, 0))
  azimuth::assert_eq(0, azimuth::multiply(max_int, 0))
  azimuth::assert_eq(max_int, azimuth::multiply(max_int, 1))
  
  // 最小值边界测试
  azimuth::assert_eq(min_int, azimuth::add(min_int, 0))
  azimuth::assert_eq(0, azimuth::multiply(min_int, 0))
  azimuth::assert_eq(min_int, azimuth::multiply(min_int, 1))
  
  // 溢出保护测试
  azimuth::assert_eq(max_int, azimuth::add(max_int, max_int))
  azimuth::assert_eq(min_int, azimuth::add(min_int, min_int))
}

test "complex_calculation_workflows" {
  // 测试复杂计算工作流
  // 计算：((a + b) * c - d) / e 的整数部分
  let a = 15
  let b = 25
  let c = 3
  let d = 40
  let e = 5
  
  let sum_ab = azimuth::add(a, b)  // 40
  let product = azimuth::multiply(sum_ab, c)  // 120
  let difference = azimuth::add(product, -d)  // 80
  let final_result = difference / e  // 16
  
  azimuth::assert_eq(16, final_result)
  
  // 计算：a * b + c * d - e * f
  let a2 = 10
  let b2 = 20
  let c2 = 30
  let d2 = 40
  let e2 = 50
  let f2 = 60
  
  let term1 = azimuth::multiply(a2, b2)  // 200
  let term2 = azimuth::multiply(c2, d2)  // 1200
  let term3 = azimuth::multiply(e2, f2)  // 3000
  
  let result1 = azimuth::add(term1, term2)  // 1400
  let result2 = azimuth::add(result1, -term3)  // -1600
  
  azimuth::assert_eq(-1600, result2)
}

test "data_validation_and_type_consistency" {
  // 测试数据验证和类型一致性
  // 验证函数返回值的类型一致性
  let add_result = azimuth::add(10, 20)
  azimuth::assert_eq(30, add_result)
  azimuth::assert_eq(30, add_result)  // 确保结果稳定
  
  let multiply_result = azimuth::multiply(6, 7)
  azimuth::assert_eq(42, multiply_result)
  azimuth::assert_eq(42, multiply_result)  // 确保结果稳定
  
  let greet_result = azimuth::greet("TestUser")
  azimuth::assert_eq_string("Hello, TestUser!", greet_result)
  azimuth::assert_eq_string("Hello, TestUser!", greet_result)  // 确保结果稳定
  
  // 验证相同输入产生相同输出
  let test_input1 = 100
  let test_input2 = 200
  let repeat_result1 = azimuth::add(test_input1, test_input2)
  let repeat_result2 = azimuth::add(test_input1, test_input2)
  azimuth::assert_eq(repeat_result1, repeat_result2)
}
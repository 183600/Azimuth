// 标准 MoonBit 测试用例
// 测试 azimuth 模块的核心功能

// 导入 azimuth 模块
test "add_basic_functionality" {
  // 测试基本加法功能
  let result1 = azimuth::add(10, 20)
  azimuth::assert_eq(30, result1)
  
  let result2 = azimuth::add(-5, 5)
  azimuth::assert_eq(0, result2)
  
  let result3 = azimuth::add(0, 100)
  azimuth::assert_eq(100, result3)
}

test "multiply_basic_functionality" {
  // 测试基本乘法功能
  let result1 = azimuth::multiply(6, 7)
  azimuth::assert_eq(42, result1)
  
  let result2 = azimuth::multiply(-8, 3)
  azimuth::assert_eq(-24, result2)
  
  let result3 = azimuth::multiply(0, 999)
  azimuth::assert_eq(0, result3)
}

test "greet_function_variations" {
  // 测试 greet 函数的不同输入
  let result1 = azimuth::greet("MoonBit")
  azimuth::assert_eq_string("Hello, MoonBit!", result1)
  
  let result2 = azimuth::greet("")
  azimuth::assert_eq_string("Hello, !", result2)
  
  let result3 = azimuth::greet("123")
  azimuth::assert_eq_string("Hello, 123!", result3)
}

test "boundary_value_operations" {
  // 测试边界值运算
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 边界值加法测试
  let result1 = azimuth::add(max_val, 1)
  azimuth::assert_eq(max_val, result1)  // 应该被限制为最大值
  
  let result2 = azimuth::add(min_val, -1)
  azimuth::assert_eq(min_val, result2)  // 应该被限制为最小值
  
  // 边界值乘法测试
  let result3 = azimuth::multiply(max_val, 2)
  azimuth::assert_eq(max_val, result3)  // 应该被限制为最大值
}

test "mathematical_properties" {
  // 测试数学性质
  let a = 15
  let b = 25
  
  // 加法交换律
  let result1 = azimuth::add(a, b)
  let result2 = azimuth::add(b, a)
  azimuth::assert_eq(result1, result2)
  
  // 乘法交换律
  let result3 = azimuth::multiply(a, b)
  let result4 = azimuth::multiply(b, a)
  azimuth::assert_eq(result3, result4)
  
  // 分配律
  let result5 = azimuth::multiply(a, azimuth::add(b, 5))
  let result6 = azimuth::add(azimuth::multiply(a, b), azimuth::multiply(a, 5))
  azimuth::assert_eq(result5, result6)
}

test "practical_calculation_scenarios" {
  // 测试实际计算场景
  // 场景1：计算折扣价格
  let original_price = 100
  let discount_percentage = 20
  let discounted_amount = azimuth::multiply(original_price, discount_percentage) / 100
  let final_price = azimuth::add(original_price, -discounted_amount)
  azimuth::assert_eq(80, final_price)
  
  // 场景2：计算平均分
  let score1 = 85
  let score2 = 90
  let score3 = 95
  let total_score = azimuth::add(azimuth::add(score1, score2), score3)
  let average_score = total_score / 3
  azimuth::assert_eq(90, average_score)
}

test "string_processing_edge_cases" {
  // 测试字符串处理的边界情况
  // 测试包含特殊字符的字符串
  let result1 = azimuth::greet("test@example.com")
  azimuth::assert_eq_string("Hello, test@example.com!", result1)
  
  // 测试包含空格的字符串
  let result2 = azimuth::greet("John Doe")
  azimuth::assert_eq_string("Hello, John Doe!", result2)
  
  // 测试包含 Unicode 字符的字符串
  let result3 = azimuth::greet("测试")
  azimuth::assert_eq_string("Hello, 测试!", result3)
}

test "complex_calculation_chain" {
  // 测试复杂计算链
  // 计算：(a + b) * c - d
  let a = 10
  let b = 15
  let c = 4
  let d = 20
  
  let sum_ab = azimuth::add(a, b)  // 25
  let product = azimuth::multiply(sum_ab, c)  // 100
  let result = azimuth::add(product, -d)  // 80
  
  azimuth::assert_eq(80, result)
}

test "error_handling_and_validation" {
  // 测试错误处理和验证
  // 测试断言函数
  azimuth::assert_true(true)
  azimuth::assert_false(false)
  azimuth::assert_eq(42, 42)
  azimuth::assert_eq_string("test", "test")
  
  // 测试溢出保护
  let max_val = 2147483647
  let overflow_result = azimuth::add(max_val, 1000)
  azimuth::assert_eq(max_val, overflow_result)
}

test "performance_related_operations" {
  // 测试性能相关操作
  // 测试大数运算
  let large_num1 = 1000000
  let large_num2 = 2000000
  
  let result1 = azimuth::add(large_num1, large_num2)
  azimuth::assert_eq(3000000, result1)
  
  let result2 = azimuth::multiply(large_num1, 2)
  azimuth::assert_eq(2000000, result2)
  
  // 测试连续运算
  let chain_result = azimuth::add(
    azimuth::multiply(100, 5),
    azimuth::multiply(200, 3)
  )
  azimuth::assert_eq(1100, chain_result)
}
// 额外的 MoonBit 测试用例
// 这些测试用例补充了现有的测试，提供更全面的覆盖

// 导入主模块
use azimuth

// 测试用例1: 性能测试 - 大量连续运算
test "performance_large_scale_calculations" {
  // 测试大量连续加法运算
  let mut sum = 0
  let mut i = 0
  while (i < 100) {
    sum = azimuth::add(sum, i)
    i = i + 1
  }
  // 0 + 1 + 2 + ... + 99 = 4950
  azimuth::assert_eq(4950, sum)
  
  // 测试大量连续乘法运算
  let mut product = 1
  let mut j = 1
  while (j < 6) {  // 1*2*3*4*5 = 120
    product = azimuth::multiply(product, j)
    j = j + 1
  }
  azimuth::assert_eq(120, product)
}

// 测试用例2: 错误输入处理测试
test "error_input_handling" {
  // 测试极端值的组合运算
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 最大值加最大值（应该溢出）
  let result1 = azimuth::add(max_val, max_val)
  azimuth::assert_eq(max_val, result1)
  
  // 最小值减最小值（相当于加最大值+1，应该溢出）
  let result2 = azimuth::add(min_val, azimuth::multiply(min_val, -1))
  azimuth::assert_eq(max_val, result2)
  
  // 最大值乘以最大值（应该溢出）
  let result3 = azimuth::multiply(max_val, max_val)
  azimuth::assert_eq(max_val, result3)
}

// 测试用例3: 复杂金融计算场景
test "complex_financial_calculations" {
  // 模拟银行复利计算
  let principal = 10000  // 本金10000元
  let rate = 5           // 年利率5%
  let years = 10         // 10年
  
  // 计算复利：使用简化公式 A = P(1 + r*n)
  // 由于没有指数运算，我们使用逐年计算
  let mut amount = principal
  let mut year = 0
  while (year < years) {
    let interest = azimuth::multiply(amount, rate) / 100
    amount = azimuth::add(amount, interest)
    year = year + 1
  }
  
  // 验证最终金额应该大于本金
  azimuth::assert_true(amount > principal)
}

// 测试用例4: 字符串处理边界情况
test "string_processing_edge_cases" {
  // 测试包含各种特殊字符的字符串
  let special_chars = "!@#$%^&*()_+-=[]{}|;':\",./<>?"
  let result1 = azimuth::greet(special_chars)
  let expected1 = "Hello, " + special_chars + "!"
  azimuth::assert_eq_string(expected1, result1)
  
  // 测试包含数字和字母混合的字符串
  let alphanumeric = "Test123ABC456"
  let result2 = azimuth::greet(alphanumeric)
  let expected2 = "Hello, " + alphanumeric + "!"
  azimuth::assert_eq_string(expected2, result2)
  
  // 测试包含空格和各种空白字符的字符串
  let whitespace = " \t\n\r"
  let result3 = azimuth::greet(whitespace)
  let expected3 = "Hello, " + whitespace + "!"
  azimuth::assert_eq_string(expected3, result3)
}

// 测试用例5: 数学运算组合测试
test "mathematical_operation_combinations" {
  // 测试 (a + b) * (c + d) 的组合
  let a = 10
  let b = 20
  let c = 3
  let d = 7
  
  let sum1 = azimuth::add(a, b)      // 10 + 20 = 30
  let sum2 = azimuth::add(c, d)      // 3 + 7 = 10
  let product = azimuth::multiply(sum1, sum2)  // 30 * 10 = 300
  
  azimuth::assert_eq(300, product)
  
  // 测试 a * b + c * d 的组合
  let product1 = azimuth::multiply(a, b)  // 10 * 20 = 200
  let product2 = azimuth::multiply(c, d)  // 3 * 7 = 21
  let sum = azimuth::add(product1, product2)  // 200 + 21 = 221
  
  azimuth::assert_eq(221, sum)
}

// 测试用例6: 物理计算场景
test "physics_calculation_scenario" {
  // 测试物理公式：距离 = 速度 * 时间
  // 然后计算总距离：初始距离 + 速度 * 时间
  
  let initial_distance = 100  // 初始距离100米
  let speed = 15              // 速度15米/秒
  let time = 10               // 时间10秒
  
  let distance_traveled = azimuth::multiply(speed, time)  // 15 * 10 = 150
  let total_distance = azimuth::add(initial_distance, distance_traveled)  // 100 + 150 = 250
  
  azimuth::assert_eq(250, total_distance)
  
  // 测试加速度计算：最终速度 = 初始速度 + 加速度 * 时间
  let initial_velocity = 5    // 初始速度5米/秒
  let acceleration = 2        // 加速度2米/秒²
  let time2 = 8               // 时间8秒
  
  let velocity_change = azimuth::multiply(acceleration, time2)  // 2 * 8 = 16
  let final_velocity = azimuth::add(initial_velocity, velocity_change)  // 5 + 16 = 21
  
  azimuth::assert_eq(21, final_velocity)
}

// 测试用例7: 边界条件组合测试
test "boundary_condition_combinations" {
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试最大值与各种值的组合
  azimuth::assert_eq(max_val, azimuth::add(max_val, 0))
  azimuth::assert_eq(max_val - 1, azimuth::add(max_val, -1))
  azimuth::assert_eq(max_val, azimuth::multiply(max_val, 1))
  azimuth::assert_eq(0, azimuth::multiply(max_val, 0))
  
  // 测试最小值与各种值的组合
  azimuth::assert_eq(min_val, azimuth::add(min_val, 0))
  azimuth::assert_eq(min_val + 1, azimuth::add(min_val, 1))
  azimuth::assert_eq(min_val, azimuth::multiply(min_val, 1))
  azimuth::assert_eq(0, azimuth::multiply(min_val, 0))
  
  // 测试接近边界的值
  let near_max = 2147483640
  let near_min = -2147483640
  
  azimuth::assert_eq(max_val, azimuth::add(near_max, 7))
  azimuth::assert_eq(min_val, azimuth::add(near_min, -8))
}

// 测试用例8: 统计计算场景
test "statistical_calculations" {
  // 计算平均值：(a + b + c + d) / 4
  // 由于没有除法运算，我们计算总和
  
  let values = [10, 20, 30, 40]  // 假设有一个数组
  let mut sum = 0
  
  // 手动模拟数组求和
  sum = azimuth::add(sum, 10)
  sum = azimuth::add(sum, 20)
  sum = azimuth::add(sum, 30)
  sum = azimuth::add(sum, 40)
  
  azimuth::assert_eq(100, sum)  // 10 + 20 + 30 + 40 = 100
  
  // 计算加权平均：权重 * 值的总和
  let weight1 = 2
  let weight2 = 3
  let weight3 = 1
  let weight4 = 4
  
  let weighted_sum1 = azimuth::multiply(weight1, 10)  // 2 * 10 = 20
  let weighted_sum2 = azimuth::multiply(weight2, 20)  // 3 * 20 = 60
  let weighted_sum3 = azimuth::multiply(weight3, 30)  // 1 * 30 = 30
  let weighted_sum4 = azimuth::multiply(weight4, 40)  // 4 * 40 = 160
  
  let total_weighted_sum = azimuth::add(azimuth::add(azimuth::add(weighted_sum1, weighted_sum2), weighted_sum3), weighted_sum4)
  azimuth::assert_eq(270, total_weighted_sum)  // 20 + 60 + 30 + 160 = 270
}

// 测试用例9: 字符串长度和内容验证
test "string_length_and_content_validation" {
  // 测试不同长度的字符串
  let empty_string = ""
  let result1 = azimuth::greet(empty_string)
  azimuth::assert_eq_string("Hello, !", result1)
  
  let single_char = "A"
  let result2 = azimuth::greet(single_char)
  azimuth::assert_eq_string("Hello, A!", result2)
  
  let long_string = "ThisIsAVeryLongStringThatTestsTheGreetingFunctionWithExtendedInput"
  let result3 = azimuth::greet(long_string)
  let expected3 = "Hello, " + long_string + "!"
  azimuth::assert_eq_string(expected3, result3)
  
  // 测试字符串内容的正确性
  let test_string = "Test123"
  let result4 = azimuth::greet(test_string)
  // 验证结果包含原始字符串
  azimuth::assert_true(result4.length() > test_string.length())
  azimuth::assert_true(result4.contains(test_string))
}

// 测试用例10: 数值范围验证测试
test "numeric_range_validation" {
  // 测试正数范围
  azimuth::assert_true(azimuth::add(1, 1) > 0)
  azimuth::assert_true(azimuth::multiply(2, 3) > 0)
  
  // 测试负数范围
  azimuth::assert_true(azimuth::add(-1, -1) < 0)
  azimuth::assert_true(azimuth::multiply(-2, 3) < 0)
  azimuth::assert_true(azimuth::multiply(2, -3) < 0)
  azimuth::assert_true(azimuth::multiply(-2, -3) > 0)
  
  // 测试零值
  azimuth::assert_eq(0, azimuth::multiply(0, 12345))
  azimuth::assert_eq(0, azimuth::multiply(12345, 0))
  azimuth::assert_eq(12345, azimuth::add(0, 12345))
  azimuth::assert_eq(12345, azimuth::add(12345, 0))
  
  // 测试大数运算
  let large_num1 = 1000000
  let large_num2 = 2000000
  azimuth::assert_eq(3000000, azimuth::add(large_num1, large_num2))
  azimuth::assert_eq(2000000000000, azimuth::multiply(large_num1, large_num2))
}
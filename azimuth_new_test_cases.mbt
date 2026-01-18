// Azimuth 项目新增测试用例
// 使用标准 MoonBit 测试语法，包含实用的测试场景

test "add_function_large_numbers" {
  // 测试大数加法
  let result = add(1000000, 2000000)
  assert_eq(3000000, result)
}

test "multiply_function_large_numbers" {
  // 测试大数乘法
  let result = multiply(10000, 10000)
  assert_eq(100000000, result)
}

test "mixed_operations" {
  // 测试混合运算
  let a = 10
  let b = 20
  let c = 5
  
  let result1 = add(multiply(a, b), c)  // 10*20+5 = 205
  let result2 = multiply(add(a, b), c)  // (10+20)*5 = 150
  
  assert_eq(205, result1)
  assert_eq(150, result2)
}

test "chain_operations" {
  // 测试链式操作
  let start = 1
  let step1 = add(start, 2)    // 3
  let step2 = multiply(step1, 3)  // 9
  let step3 = add(step2, 4)    // 13
  let step4 = multiply(step3, 2)  // 26
  
  assert_eq(26, step4)
}

test "negative_number_operations" {
  // 测试负数运算
  let a = -10
  let b = 5
  
  let add_result = add(a, b)      // -5
  let mul_result = multiply(a, b)  // -50
  
  assert_eq(-5, add_result)
  assert_eq(-50, mul_result)
}

test "string_concatenation_multiple" {
  // 测试多字符串连接
  let part1 = "Moon"
  let part2 = "Bit"
  let part3 = " "
  let part4 = "Testing"
  
  let result = add(add(add(part1, part2), part3), part4)
  assert_eq_string("MoonBit Testing", result)
}

test "calculation_with_constants" {
  // 测试使用常量的计算
  let PI = 3  // 简化值
  let radius = 10
  
  let circumference = multiply(multiply(2, PI), radius)
  let area = multiply(multiply(radius, radius), PI)
  
  assert_eq(60, circumference)
  assert_eq(300, area)
}

test "business_calculation_complex" {
  // 测试复杂商业计算
  let unit_price = 100
  let quantity = 5
  let discount_rate = 10  // 10%
  let tax_rate = 8        // 8%
  
  let subtotal = multiply(unit_price, quantity)  // 500
  let discount = multiply(subtotal, discount_rate) / 100  // 50
  let after_discount = subtract(subtotal, discount)  // 450
  let tax = multiply(after_discount, tax_rate) / 100  // 36
  let total = add(after_discount, tax)  // 486
  
  assert_eq(500, subtotal)
  assert_eq(50, discount)
  assert_eq(450, after_discount)
  assert_eq(36, tax)
  assert_eq(486, total)
}

test "sequence_pattern" {
  // 测试序列模式计算
  
  // 计算 1 + 2 + 3 + 4 + 5
  let sum1 = add(1, add(2, add(3, add(4, 5))))
  
  // 计算 1*2 + 2*3 + 3*4 + 4*5
  let term1 = multiply(1, 2)
  let term2 = multiply(2, 3)
  let term3 = multiply(3, 4)
  let term4 = multiply(4, 5)
  let sum2 = add(add(term1, term2), add(term3, term4))
  
  assert_eq(15, sum1)
  assert_eq(40, sum2)
}

test "edge_case_operations" {
  // 测试边界情况操作
  let max_small = 127
  let min_small = -128
  
  let result1 = add(max_small, 1)    // 溢出测试
  let result2 = add(min_small, -1)   // 下溢测试
  let result3 = multiply(max_small, 1)
  let result4 = multiply(min_small, 1)
  
  assert_eq(128, result1)
  assert_eq(-129, result2)
  assert_eq(127, result3)
  assert_eq(-128, result4)
}
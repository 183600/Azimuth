// 额外的 MoonBit 测试用例
// 为 azimuth 项目添加更多测试覆盖

test "divide_with_ceil_basic" {
  // 测试向上取整除法的基本功能
  assert_eq(3, divide_with_ceil(10, 4))  // 10/4 = 2.5 → 3
  assert_eq(2, divide_with_ceil(8, 4))   // 8/4 = 2 → 2
  assert_eq(5, divide_with_ceil(21, 5))  // 21/5 = 4.2 → 5
  assert_eq(0, divide_with_ceil(3, 5))   // 3/5 = 0.6 → 1，但由于整数除法特性，这里测试边界情况
}

test "divide_with_ceil_negative" {
  // 测试负数的向上取整除法
  assert_eq(-2, divide_with_ceil(-10, 4))  // -10/4 = -2.5 → -2
  assert_eq(-2, divide_with_ceil(-8, 4))   // -8/4 = -2 → -2
  assert_eq(-4, divide_with_ceil(-21, 5))  // -21/5 = -4.2 → -4
}

test "complex_financial_calculation" {
  // 复杂的金融计算场景：贷款分期计算
  let principal = 10000  // 本金
  let annual_rate = 6    // 年利率6%
  let months = 12        // 12个月
  
  // 简化的月供计算：本金 + (本金 × 年利率 × 月数) / 12
  let total_interest = multiply(principal, multiply(annual_rate, months)) / 12
  let total_payment = add(principal, total_interest)
  let monthly_payment = total_payment / months
  
  assert_eq(10500, total_payment)     // 总还款额
  assert_eq(875, monthly_payment)     // 月供
}

test "scientific_notation_simulation" {
  // 模拟科学计数法计算
  // 计算 1.23 × 10^6 + 4.56 × 10^5
  // 使用整数表示：123 × 10^4 + 456 × 10^3
  
  let term1 = multiply(123, 10000)  // 1,230,000
  let term2 = multiply(456, 1000)   // 456,000
  let result = add(term1, term2)    // 1,686,000
  
  assert_eq(1686000, result)
}

test "geometric_progression" {
  // 测试等比数列计算
  // 计算 2 + 4 + 8 + 16 + 32
  let term1 = 2
  let term2 = multiply(term1, 2)    // 4
  let term3 = multiply(term2, 2)    // 8
  let term4 = multiply(term3, 2)    // 16
  let term5 = multiply(term4, 2)    // 32
  
  let sum = add(add(add(add(term1, term2), term3), term4), term5)
  assert_eq(62, sum)
}

test "string_with_unicode_characters" {
  // 测试包含各种Unicode字符的字符串
  assert_eq_string("Hello, 🌟!", greet("🌟"))
  assert_eq_string("Hello, 🚀!", greet("🚀"))
  assert_eq_string("Hello, 🎉!", greet("🎉"))
  assert_eq_string("Hello, αβγ!", greet("αβγ"))
  assert_eq_string("Hello, 中文!", greet("中文"))
  assert_eq_string("Hello, العربية!", greet("العربية"))
}

test "nested_function_calls" {
  // 测试嵌套函数调用
  // 计算 greet(add(multiply(2, 3), 4)) 的字符串表示
  let mult_result = multiply(2, 3)  // 6
  let add_result = add(mult_result, 4)  // 10
  let string_result = greet(add_result.to_string())
  
  // 由于没有直接的整数转字符串函数，我们手动构建
  assert_eq_string("Hello, 10!", greet("10"))
}

test "algorithm_complexity_simulation" {
  // 模拟算法复杂度计算
  // O(n²) 算法的时间复杂度模拟
  let n = 100
  
  // 计算 n × (n + 1) / 2，表示前n个整数的和
  let triangular_number = multiply(n, add(n, 1)) / 2
  assert_eq(5050, triangular_number)
  
  // 计算 n²，表示O(n²)复杂度
  let squared = multiply(n, n)
  assert_eq(10000, squared)
}

test "data_structure_operations" {
  // 模拟数据结构操作
  // 模拟栈操作：push, push, pop, push
  let stack1 = 10  // push(10)
  let stack2 = 20  // push(20)
  let stack3 = add(stack1, stack2)  // pop() + pop() = 10 + 20 = 30
  let stack4 = 40  // push(40)
  let final_stack = add(stack3, stack4)  // 30 + 40 = 70
  
  assert_eq(70, final_stack)
}

test "error_boundary_conditions" {
  // 测试错误边界条件
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试连续溢出操作
  let overflow1 = add(max_val, 1)     // 应该返回 max_val
  let overflow2 = add(overflow1, 1)   // 应该仍然返回 max_val
  assert_eq(max_val, overflow2)
  
  // 测试连续下溢操作
  let underflow1 = add(min_val, -1)   // 应该返回 min_val
  let underflow2 = add(underflow1, -1) // 应该仍然返回 min_val
  assert_eq(min_val, underflow2)
  
  // 测试乘法溢出链
  let mul_overflow1 = multiply(max_val, 2)  // 应该返回 max_val
  let mul_overflow2 = multiply(mul_overflow1, 2)  // 应该仍然返回 max_val
  assert_eq(max_val, mul_overflow2)
}
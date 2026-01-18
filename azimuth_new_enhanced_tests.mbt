// Azimuth 项目新增 MoonBit 测试用例
// 使用标准 MoonBit 测试语法，覆盖新的测试场景

test "arithmetic_progression_sum" {
  // 测试等差数列求和：1 + 4 + 7 + 10 + 13
  // 使用公式：n × (首项 + 末项) / 2
  let n = 5
  let first = 1
  let last = 13
  let sum = multiply(n, add(first, last)) / 2
  assert_eq(35, sum)
}

test "geometric_progression_product" {
  // 测试等比数列求积：2 × 4 × 8 × 16
  // 通过连续相乘实现
  let term1 = 2
  let term2 = multiply(term1, 2)  // 4
  let term3 = multiply(term2, 2)  // 8
  let term4 = multiply(term3, 2)  // 16
  let product = multiply(multiply(multiply(term1, term2), term3), term4)
  assert_eq(1024, product)
}

test "complex_string_scenarios" {
  // 测试复杂字符串场景
  // 测试包含各种特殊字符的字符串
  let special_chars = "!@#$%^&*()_+-=[]{}|;':\",./<>?"
  let result1 = greet(special_chars)
  assert_eq_string("Hello, !@#$%^&*()_+-=[]{}|;':\",./<>?!", result1)
  
  // 测试包含空格和制表符的字符串
  let whitespace = "  Hello\tWorld  "
  let result2 = greet(whitespace)
  assert_eq_string("Hello,   Hello	World  !", result2)
}

test "mathematical_identities" {
  // 测试数学恒等式
  let a = 12
  let b = 8
  
  // 验证平方和公式：(a + b)² = a² + 2ab + b²
  let left = multiply(add(a, b), add(a, b))
  let right = add(add(multiply(a, a), multiply(2, multiply(a, b))), multiply(b, b))
  assert_eq(left, right)
  
  // 验证平方差公式：(a - b)² = a² - 2ab + b²
  let diff_left = multiply(add(a, -b), add(a, -b))
  let diff_right = add(add(multiply(a, a), multiply(-2, multiply(a, b))), multiply(b, b))
  assert_eq(diff_left, diff_right)
}

test "financial_calculations" {
  // 测试金融计算场景
  let principal = 10000
  let rate = 5  // 5% 利率
  let time = 3  // 3年
  
  // 简单利息：本金 × 利率 × 时间
  let simple_interest = multiply(principal, multiply(rate, time))
  assert_eq(150000, simple_interest)
  
  // 复利（简化版）：本金 × (1 + 利率 × 时间)
  let compound_total = multiply(principal, add(1, multiply(rate, time)))
  assert_eq(160000, compound_total)
}

test "temperature_conversions" {
  // 测试温度转换
  // 摄氏度转华氏度（简化版）：F = C × 2 + 30
  let celsius = 25
  let fahrenheit = add(multiply(celsius, 2), 30)
  assert_eq(80, fahrenheit)
  
  // 华氏度转摄氏度（简化版）：C = (F - 30) / 2
  let f_to_c = add(fahrenheit, -30) / 2
  assert_eq(25, f_to_c)
  
  // 测试极端温度
  let freezing_c = 0
  let freezing_f = add(multiply(freezing_c, 2), 30)
  assert_eq(30, freezing_f)
  
  let boiling_c = 100
  let boiling_f = add(multiply(boiling_c, 2), 30)
  assert_eq(230, boiling_f)
}

test "algorithm_complexity_simulation" {
  // 测试算法复杂度模拟
  let n = 10
  
  // O(n) 复杂度：线性求和 1 + 2 + ... + n
  let linear_sum = multiply(n, add(n, 1)) / 2
  assert_eq(55, linear_sum)
  
  // O(n²) 复杂度：双重循环求和（简化为公式计算）
  let quadratic_sum = multiply(n, multiply(add(n, 1), add(multiply(2, n), 1))) / 6
  assert_eq(385, quadratic_sum)
  
  // O(log n) 复杂度：对数运算模拟（简化为除法）
  let log_approx = n / 2
  assert_eq(5, log_approx)
}

test "combinatorial_calculations" {
  // 测试组合数学计算
  let n = 5
  let r = 3
  
  // 计算排列数 P(n,r) = n! / (n-r)!
  // 简化计算：5! / 2! = 120 / 2 = 60
  let n_factorial = multiply(n, multiply(n - 1, multiply(n - 2, multiply(n - 3, n - 4))))
  let n_minus_r_factorial = multiply(n - r, n - r - 1)
  let permutation = n_factorial / n_minus_r_factorial
  assert_eq(60, permutation)
  
  // 计算组合数 C(n,r) = P(n,r) / r!
  // 简化计算：60 / 6 = 10
  let r_factorial = multiply(r, multiply(r - 1, r - 2))
  let combination = permutation / r_factorial
  assert_eq(10, combination)
}

test "data_structure_operations" {
  // 测试数据结构操作的数学模拟
  let array_size = 10
  
  // 模拟数组访问：索引计算
  let row = 2
  let col = 3
  let width = 5
  let index = add(multiply(row, width), col)
  assert_eq(13, index)
  
  // 模拟栈操作：压栈和弹栈
  let stack_top = 100
  let push_value = 200
  let new_top = add(stack_top, push_value)
  let popped_value = new_top - stack_top
  assert_eq(200, popped_value)
  
  // 模拟队列操作：入队和出队
  let queue_front = 1
  let queue_rear = 5
  let queue_size = add(queue_rear, -queue_front)
  assert_eq(4, queue_size)
}

test "error_boundary_conditions" {
  // 测试错误边界条件
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试溢出保护的正确性
  assert_true(add(max_val, 1) == max_val)
  assert_true(add(min_val, -1) == min_val)
  assert_true(multiply(max_val, 2) == max_val)
  assert_true(multiply(min_val, 2) == min_val)
  
  // 测试特殊值的处理
  assert_true(multiply(min_val, -1) == min_val)
  assert_true(add(max_val, min_val) == -1)
  
  // 测试零值的特殊性质
  assert_true(multiply(0, max_val) == 0)
  assert_true(multiply(0, min_val) == 0)
  assert_true(add(0, max_val) == max_val)
  assert_true(add(0, min_val) == min_val)
}
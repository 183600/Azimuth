// 新增的标准 MoonBit 测试用例 - 10个高质量测试用例
// 针对 azimuth 库的核心功能进行全面测试

// 导入被测试的模块
use azimuth

test addition_identity_properties {
  // 测试加法的单位元性质
  azimuth.assert_eq(5, azimuth.add(5, 0))
  azimuth.assert_eq(-3, azimuth.add(-3, 0))
  azimuth.assert_eq(0, azimuth.add(0, 0))
  azimuth.assert_eq(1000000, azimuth.add(1000000, 0))
}

test multiplication_identity_properties {
  // 测试乘法的单位元性质
  azimuth.assert_eq(7, azimuth.multiply(7, 1))
  azimuth.assert_eq(-10, azimuth.multiply(-10, 1))
  azimuth.assert_eq(0, azimuth.multiply(0, 1))
  azimuth.assert_eq(15, azimuth.multiply(15, 1))
}

test divide_with_ceil_fractional_cases {
  // 测试向上取整除法的小数情况
  azimuth.assert_eq(2, azimuth.divide_with_ceil(3, 2))  // 1.5 -> 2
  azimuth.assert_eq(3, azimuth.divide_with_ceil(8, 3))  // 2.666... -> 3
  azimuth.assert_eq(7, azimuth.divide_with_ceil(13, 2)) // 6.5 -> 7
  azimuth.assert_eq(12, azimuth.divide_with_ceil(100, 9)) // 11.111... -> 12
}

test greet_function_empty_and_whitespace {
  // 测试问候函数的空字符串和空白字符处理
  azimuth.assert_eq_string("Hello, !", azimuth.greet(""))
  azimuth.assert_eq_string("Hello,  !", azimuth.greet(" "))
  azimuth.assert_eq_string("Hello,   !", azimuth.greet("  "))
  azimuth.assert_eq_string("Hello, \t!", azimuth.greet("\t"))
}

test combined_arithmetic_expressions {
  // 测试组合算术表达式
  let x = 10
  let y = 15
  let z = 20
  
  // 测试 (x + y) * z
  let result1 = azimuth.multiply(azimuth.add(x, y), z)
  azimuth.assert_eq(500, result1)  // (10 + 15) * 20 = 25 * 20 = 500
  
  // 测试 x * y + z
  let result2 = azimuth.add(azimuth.multiply(x, y), z)
  azimuth.assert_eq(170, result2)  // 10 * 15 + 20 = 150 + 20 = 170
}

test negative_number_division_ceil {
  // 测试负数的向上取整除法
  azimuth.assert_eq(-2, azimuth.divide_with_ceil(-4, 2))   // -4/2 = -2 -> -2
  azimuth.assert_eq(-1, azimuth.divide_with_ceil(-3, 2))   // -3/2 = -1.5 -> -1
  azimuth.assert_eq(-3, azimuth.divide_with_ceil(-7, 2))   // -7/2 = -3.5 -> -3
  azimuth.assert_eq(-5, azimuth.divide_with_ceil(-11, 2))  // -11/2 = -5.5 -> -5
}

test large_scale_calculations {
  // 测试大规模计算
  let base = 10000
  let multiplier = 100
  let addition = 50000
  
  // 测试大数乘法
  let large_product = azimuth.multiply(base, multiplier)
  azimuth.assert_eq(1000000, large_product)
  
  // 测试大数加法
  let large_sum = azimuth.add(large_product, addition)
  azimuth.assert_eq(1050000, large_sum)
}

test real_world_budget_calculation {
  // 测试实际世界的预算计算
  let monthly_income = 5000
  let rent = 1500
  let utilities = 200
  let food = 600
  let transportation = 300
  let entertainment = 200
  
  // 计算总支出
  let total_expenses = azimuth.add(azimuth.add(azimuth.add(azimuth.add(rent, utilities), food), transportation), entertainment)
  
  // 计算剩余预算
  let remaining_budget = azimuth.add(monthly_income, -total_expenses)
  
  azimuth.assert_eq(2800, total_expenses)
  azimuth.assert_eq(2200, remaining_budget)
}

test string_greeting_with_numbers_and_symbols {
  // 测试包含数字和符号的字符串问候
  azimuth.assert_eq_string("Hello, User123!", azimuth.greet("User123"))
  azimuth.assert_eq_string("Hello, test@example.com!", azimuth.greet("test@example.com"))
  azimuth.assert_eq_string("Hello, A+B=C!", azimuth.greet("A+B=C"))
  azimuth.assert_eq_string("Hello, 100%!", azimuth.greet("100%"))
}

test complex_division_scenarios {
  // 测试复杂的除法场景
  // 测试被除数小于除数的情况
  azimuth.assert_eq(1, azimuth.divide_with_ceil(5, 10))
  azimuth.assert_eq(1, azimuth.divide_with_ceil(1, 100))
  azimuth.assert_eq(1, azimuth.divide_with_ceil(99, 100))
  
  // 测试被除数等于除数的情况
  azimuth.assert_eq(1, azimuth.divide_with_ceil(10, 10))
  azimuth.assert_eq(5, azimuth.divide_with_ceil(5, 1))
  
  // 测试被除数是除数的倍数
  azimuth.assert_eq(3, azimuth.divide_with_ceil(15, 5))
  azimuth.assert_eq(10, azimuth.divide_with_ceil(100, 10))
}
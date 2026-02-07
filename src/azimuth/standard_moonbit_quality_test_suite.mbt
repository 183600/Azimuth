// 标准 MoonBit 质量测试套件
// 包含 8 个高质量测试用例，使用标准 MoonBit 测试语法
// 覆盖核心功能和边界情况

test "add_function_comprehensive" {
  // 加法函数综合测试
  // 测试正数、负数、零值的各种组合
  
  // 基本正数加法
  assert_eq(add(5, 3), 8)
  assert_eq(add(100, 200), 300)
  
  // 包含零的加法
  assert_eq(add(0, 0), 0)
  assert_eq(add(5, 0), 5)
  assert_eq(add(0, 7), 7)
  
  // 负数加法
  assert_eq(add(-5, 3), -2)
  assert_eq(add(5, -3), 2)
  assert_eq(add(-5, -3), -8)
  
  // 大数加法
  assert_eq(add(1000000, 1000000), 2000000)
  assert_eq(add(-1000000, 1000000), 0)
}

test "multiply_function_edge_cases" {
  // 乘法函数边界情况测试
  // 测试各种边界值和特殊情况
  
  // 基本乘法
  assert_eq(multiply(4, 5), 20)
  assert_eq(multiply(-3, 7), -21)
  
  // 零乘法
  assert_eq(multiply(0, 5), 0)
  assert_eq(multiply(5, 0), 0)
  assert_eq(multiply(0, 0), 0)
  
  // 单位元乘法
  assert_eq(multiply(1, 100), 100)
  assert_eq(multiply(100, 1), 100)
  assert_eq(multiply(-1, 50), -50)
  
  // 负数乘法
  assert_eq(multiply(-4, -6), 24)
  assert_eq(multiply(-4, 6), -24)
  assert_eq(multiply(4, -6), -24)
  
  // 大数乘法
  assert_eq(multiply(1000, 1000), 1000000)
}

test "subtract_function_mathematical_properties" {
  // 减法函数数学性质测试
  // 验证减法的数学性质和边界情况
  
  // 基本减法
  assert_eq(subtract(10, 3), 7)
  assert_eq(subtract(3, 10), -7)
  
  // 自减法
  assert_eq(subtract(5, 5), 0)
  assert_eq(subtract(-5, -5), 0)
  
  // 零减法
  assert_eq(subtract(0, 5), -5)
  assert_eq(subtract(5, 0), 5)
  assert_eq(subtract(0, 0), 0)
  
  // 减法的逆运算验证
  let a = 15
  let b = 7
  assert_eq(add(subtract(a, b), b), a)
  
  // 负数减法
  assert_eq(subtract(-5, 3), -8)
  assert_eq(subtract(5, -3), 8)
  assert_eq(subtract(-5, -3), -2)
}

test "divide_with_ceil_comprehensive_analysis" {
  // 向上取整除法综合分析测试
  // 全面测试除法函数的各种情况
  
  // 正常除法（向上取整）
  assert_eq(divide_with_ceil(10, 3), 4)
  assert_eq(divide_with_ceil(15, 5), 3)
  assert_eq(divide_with_ceil(7, 2), 4)
  assert_eq(divide_with_ceil(1, 10), 1)
  
  // 整除情况
  assert_eq(divide_with_ceil(12, 3), 4)
  assert_eq(divide_with_ceil(100, 10), 10)
  assert_eq(divide_with_ceil(0, 5), 0)
  
  // 边界情况：除数为零
  assert_eq(divide_with_ceil(10, 0), 0)
  assert_eq(divide_with_ceil(0, 0), 0)
  assert_eq(divide_with_ceil(-10, 0), 0)
  
  // 负数除法
  assert_eq(divide_with_ceil(-10, 3), -3)
  assert_eq(divide_with_ceil(10, -3), 3)
  assert_eq(divide_with_ceil(-10, -3), 4)
  assert_eq(divide_with_ceil(-7, 2), -3)
  
  // 大数除法
  assert_eq(divide_with_ceil(1000000, 3), 333334)
  assert_eq(divide_with_ceil(999999, 1000), 1000)
}

test "greet_function_string_processing" {
  // 问候函数字符串处理测试
  // 测试字符串连接和各种输入情况
  
  // 标准输入
  assert_eq_string(greet("World"), "Hello, World!")
  assert_eq_string(greet("MoonBit"), "Hello, MoonBit!")
  assert_eq_string(greet("Developer"), "Hello, Developer!")
  
  // 边界情况
  assert_eq_string(greet(""), "Hello, !")
  assert_eq_string(greet(" "), "Hello,  !")
  assert_eq_string(greet("A"), "Hello, A!")
  
  // 特殊字符和数字
  assert_eq_string(greet("123"), "Hello, 123!")
  assert_eq_string(greet("!@#"), "Hello, !@#!")
  assert_eq_string(greet("中文"), "Hello, 中文!")
  
  // 长字符串
  assert_eq_string(greet("VeryLongName"), "Hello, VeryLongName!")
}

test "complex_calculation_scenarios" {
  // 复杂计算场景测试
  // 模拟真实世界中的复杂计算场景
  
  // 购物车计算
  let item1_price = 25
  let item1_qty = 3
  let item2_price = 15
  let item2_qty = 2
  let tax_rate = 10
  
  let subtotal1 = multiply(item1_price, item1_qty)
  let subtotal2 = multiply(item2_price, item2_qty)
  let total_before_tax = add(subtotal1, subtotal2)
  let tax_amount = divide_with_ceil(multiply(total_before_tax, tax_rate), 100)
  let final_total = add(total_before_tax, tax_amount)
  
  assert_eq(subtotal1, 75)
  assert_eq(subtotal2, 30)
  assert_eq(total_before_tax, 105)
  assert_eq(tax_amount, 11)
  assert_eq(final_total, 116)
  
  // 平均值计算（向上取整）
  let values_sum = add(add(add(15, 22), 18), 25)
  let count = 4
  let average = divide_with_ceil(values_sum, count)
  
  assert_eq(values_sum, 80)
  assert_eq(average, 20)
}

test "mathematical_algorithm_simulation" {
  // 数学算法模拟测试
  // 模拟经典数学算法的计算过程
  
  // 等差数列求和：1 + 2 + ... + 10 = 55
  let n = 10
  let sum_arithmetic = divide_with_ceil(multiply(n, add(n, 1)), 2)
  
  // 验证手动计算
  let manual_sum = add(add(add(add(add(add(add(add(add(1, 2), 3), 4), 5), 6), 7), 8), 9), 10)
  
  assert_eq(sum_arithmetic, 55)
  assert_eq(manual_sum, 55)
  
  // 阶乘计算：5! = 120
  let fact1 = 1
  let fact2 = multiply(fact1, 2)
  let fact3 = multiply(fact2, 3)
  let fact4 = multiply(fact3, 4)
  let fact5 = multiply(fact4, 5)
  
  assert_eq(fact1, 1)
  assert_eq(fact2, 2)
  assert_eq(fact3, 6)
  assert_eq(fact4, 24)
  assert_eq(fact5, 120)
  
  // 幂运算模拟：2^4 = 16
  let power_result = multiply(multiply(multiply(2, 2), 2), 2)
  assert_eq(power_result, 16)
}

test "boundary_value_systematic_testing" {
  // 边界值系统测试
  // 系统性测试各种边界条件和极端情况
  
  // 数值边界测试
  let max_positive = 1000000
  let min_negative = -1000000
  let zero = 0
  let one = 1
  let neg_one = -1
  
  // 加法边界测试
  assert_eq(add(max_positive, one), 1000001)
  assert_eq(add(min_negative, neg_one), -1000001)
  assert_eq(add(zero, max_positive), max_positive)
  assert_eq(add(zero, min_negative), min_negative)
  
  // 乘法边界测试
  assert_eq(multiply(max_positive, one), max_positive)
  assert_eq(multiply(min_negative, one), min_negative)
  assert_eq(multiply(max_positive, zero), zero)
  assert_eq(multiply(min_negative, zero), zero)
  
  // 减法边界测试
  assert_eq(subtract(max_positive, one), 999999)
  assert_eq(subtract(min_negative, neg_one), -999999)
  assert_eq(subtract(zero, max_positive), min_negative + 1)
  assert_eq(subtract(zero, min_negative), max_positive - 1)
  
  // 除法边界测试
  assert_eq(divide_with_ceil(max_positive, one), max_positive)
  assert_eq(divide_with_ceil(min_negative, one), min_negative)
  assert_eq(divide_with_ceil(zero, one), zero)
  assert_eq(divide_with_ceil(max_positive, max_positive), one)
  
  // 复合边界测试
  let complex_calc = add(multiply(subtract(max_positive, min_negative), 2), one)
  let expected = add(multiply(2000000, 2), 1)
  assert_eq(complex_calc, expected)
}
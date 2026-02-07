// Azimuth 项目新增标准 MoonBit 测试用例
// 包含10个高质量测试用例，使用标准 MoonBit 测试语法

test "addition_with_various_number_types" {
  // 测试各种数字类型的加法运算
  assert_eq(15, add(7, 8))
  assert_eq(-5, add(-10, 5))
  assert_eq(0, add(-3, 3))
  assert_eq(1000, add(500, 500))
  assert_eq(-100, add(-60, -40))
}

test "multiplication_comprehensive_cases" {
  // 测试乘法的综合情况
  assert_eq(42, multiply(6, 7))
  assert_eq(0, multiply(0, 999))
  assert_eq(-64, multiply(-8, 8))
  assert_eq(64, multiply(-8, -8))
  assert_eq(1000000, multiply(1000, 1000))
}

test "division_ceil_fractions" {
  // 测试分数的向上取整除法
  assert_eq(2, divide_with_ceil(3, 2))
  assert_eq(3, divide_with_ceil(8, 3))
  assert_eq(5, divide_with_ceil(21, 5))
  assert_eq(1, divide_with_ceil(1, 1))
  assert_eq(7, divide_with_ceil(13, 2))
}

test "division_ceil_negative_scenarios" {
  // 测试负数场景的向上取整除法
  assert_eq(-2, divide_with_ceil(-4, 2))
  assert_eq(-1, divide_with_ceil(-3, 4))
  assert_eq(2, divide_with_ceil(-5, -3))
  assert_eq(-3, divide_with_ceil(7, -3))
  assert_eq(0, divide_with_ceil(-1, -2))
}

test "subtraction_edge_conditions" {
  // 测试减法的边界条件
  assert_eq(10, subtract(20, 10))
  assert_eq(-10, subtract(10, 20))
  assert_eq(0, subtract(100, 100))
  assert_eq(50, subtract(0, -50))
  assert_eq(-50, subtract(-50, 0))
}

test "greet_function_special_characters" {
  // 测试问候函数的特殊字符处理
  assert_eq_string("Hello, @#$!", greet("@#$"))
  assert_eq_string("Hello, space test!", greet("space test"))
  assert_eq_string("Hello, 中文!", greet("中文"))
  assert_eq_string("Hello, émoji!", greet("émoji"))
  assert_eq_string("Hello, !", greet(" "))
}

test "complex_multi_step_calculations" {
  // 测试复杂的多步计算
  let x = 12
  let y = 8
  let z = 5
  
  // (x + y) * z - 10
  let step1 = add(x, y)
  let step2 = multiply(step1, z)
  let result = subtract(step2, 10)
  
  assert_eq(20, step1)
  assert_eq(100, step2)
  assert_eq(90, result)
}

test "mathematical_commutative_associative" {
  // 测试数学的交换律和结合律
  let a = 15
  let b = 25
  let c = 10
  
  // 交换律验证
  assert_eq(add(a, b), add(b, a))
  assert_eq(multiply(a, b), multiply(b, a))
  
  // 结合律验证
  assert_eq(add(add(a, b), c), add(a, add(b, c)))
  assert_eq(multiply(multiply(a, b), c), multiply(a, multiply(b, c)))
}

test "financial_calculation_scenario" {
  // 测试金融计算场景
  // 计算投资回报：本金 × (1 + 利率) × 时间 - 本金
  let principal = 1000
  let rate = 5
  let time = 3
  
  let yearly_return = divide_with_ceil(multiply(principal, rate), 100)
  let total_return = multiply(yearly_return, time)
  let final_amount = add(principal, total_return)
  
  assert_eq(50, yearly_return)
  assert_eq(150, total_return)
  assert_eq(1150, final_amount)
}

test "error_handling_boundary_cases" {
  // 测试错误处理的边界情况
  // 除零保护测试
  assert_eq(0, divide_with_ceil(100, 0))
  assert_eq(0, divide_with_ceil(-100, 0))
  assert_eq(0, divide_with_ceil(0, 0))
  
  // 极值测试
  assert_eq(0, divide_with_ceil(0, 1000))
  assert_eq(1, divide_with_ceil(1, 1))
  assert_eq(1, divide_with_ceil(-1, -1))
}

test "advanced_arithmetic_combinations" {
  // 测试高级算术组合
  let base = 5
  let multiplier = 4
  let divisor = 3
  
  // 复合计算：(base * multiplier) / divisor
  let product = multiply(base, multiplier)
  let result = divide_with_ceil(product, divisor)
  
  assert_eq(20, product)
  assert_eq(7, result)
  
  // 链式计算验证
  let chain_result = divide_with_ceil(multiply(6, 7), 4)
  assert_eq(11, chain_result)
}
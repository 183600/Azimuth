// MoonBit 综合测试用例
// 测试 Azimuth 项目的核心功能和实际应用场景

test "add_function_basic_cases" {
  // 测试加法函数的基本情况
  @azimuth.assert_eq(10, @azimuth.add(3, 7))
  @azimuth.assert_eq(0, @azimuth.add(5, -5))
  @azimuth.assert_eq(-8, @azimuth.add(-3, -5))
  @azimuth.assert_eq(100, @azimuth.add(0, 100))
  @azimuth.assert_eq(-50, @azimuth.add(-50, 0))
}

test "add_function_edge_cases" {
  // 测试加法函数的边界情况
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试溢出边界
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 1))
  @azimuth.assert_eq(max_val, @azimuth.add(1000000000, 1500000000))
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, -1))
  @azimuth.assert_eq(min_val, @azimuth.add(-1000000000, -1500000000))
  
  // 测试特殊值组合
  @azimuth.assert_eq(-1, @azimuth.add(max_val, min_val))
}

test "multiply_function_basic_cases" {
  // 测试乘法函数的基本情况
  @azimuth.assert_eq(12, @azimuth.multiply(3, 4))
  @azimuth.assert_eq(0, @azimuth.multiply(5, 0))
  @azimuth.assert_eq(-15, @azimuth.multiply(3, -5))
  @azimuth.assert_eq(16, @azimuth.multiply(-4, -4))
  @azimuth.assert_eq(7, @azimuth.multiply(7, 1))
  @azimuth.assert_eq(-7, @azimuth.multiply(7, -1))
}

test "multiply_function_edge_cases" {
  // 测试乘法函数的边界情况
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试溢出边界
  @azimuth.assert_eq(max_val, @azimuth.multiply(46341, 46341))
  @azimuth.assert_eq(min_val, @azimuth.multiply(-46341, 46341))
  
  // 测试特殊值
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, -1))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, 2))
}

test "greet_function_various_inputs" {
  // 测试问候函数的各种输入
  @azimuth.assert_eq_string("Hello, World!", @azimuth.greet("World"))
  @azimuth.assert_eq_string("Hello, MoonBit!", @azimuth.greet("MoonBit"))
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  @azimuth.assert_eq_string("Hello, A!", @azimuth.greet("A"))
  @azimuth.assert_eq_string("Hello, 123!", @azimuth.greet("123"))
}

test "greet_function_special_characters" {
  // 测试问候函数的特殊字符处理
  @azimuth.assert_eq_string("Hello, 中文!", @azimuth.greet("中文"))
  @azimuth.assert_eq_string("Hello, émoji!", @azimuth.greet("émoji"))
  @azimuth.assert_eq_string("Hello, space !", @azimuth.greet("space "))
  @azimuth.assert_eq_string("Hello, @#$!", @azimuth.greet("@#$"))
}

test "mathematical_calculations" {
  // 测试数学计算场景
  let x = 15
  let y = 25
  
  // 计算表达式：(x + y) * 2 - 10
  let sum = @azimuth.add(x, y)
  let product = @azimuth.multiply(sum, 2)
  let result = @azimuth.add(product, -10)
  @azimuth.assert_eq(70, result)
  
  // 计算平方：x²
  let square = @azimuth.multiply(x, x)
  @azimuth.assert_eq(225, square)
}

test "financial_application" {
  // 测试金融应用场景
  let principal = 10000
  let rate = 5
  let years = 3
  
  // 计算简单利息：本金 × (1 + 利率 × 年数)
  let interest_rate = @azimuth.multiply(rate, years)
  let growth_factor = @azimuth.add(1, interest_rate)
  let final_amount = @azimuth.multiply(principal, growth_factor)
  @azimuth.assert_eq(160000, final_amount)
  
  // 计算月供（简化版）
  let months = @azimuth.multiply(years, 12)
  let monthly_payment = final_amount / months
  @azimuth.assert_eq(4444, monthly_payment)
}

test "physics_calculations" {
  // 测试物理计算场景
  let mass = 10
  let acceleration = 5
  
  // 计算力：F = m × a
  let force = @azimuth.multiply(mass, acceleration)
  @azimuth.assert_eq(50, force)
  
  // 计算动能（简化版）：KE = 0.5 × m × v²
  let velocity = 8
  let velocity_squared = @azimuth.multiply(velocity, velocity)
  let kinetic_energy = @azimuth.multiply(mass, velocity_squared) / 2
  @azimuth.assert_eq(320, kinetic_energy)
}

test "assertion_functions_verification" {
  // 测试断言函数
  @azimuth.assert_true(true)
  @azimuth.assert_false(false)
  @azimuth.assert_eq(42, 42)
  @azimuth.assert_eq_string("test", "test")
  
  // 测试条件断言
  let a = 10
  let b = 20
  @azimuth.assert_true(@azimuth.add(a, b) == 30)
  @azimuth.assert_false(@azimuth.multiply(a, b) == 150)
  @azimuth.assert_eq(200, @azimuth.multiply(a, b))
}
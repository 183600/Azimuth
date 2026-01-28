// 标准 MoonBit 测试用例 - 针对 clean_test 库的核心功能
// 包含 8 个高质量测试用例，覆盖基本功能和边界情况

test "add_function_basic_cases" {
  // 测试加法函数的基本情况
  @clean_test.assert_eq(8, @clean_test.add(5, 3))
  @clean_test.assert_eq(0, @clean_test.add(0, 0))
  @clean_test.assert_eq(-8, @clean_test.add(-5, -3))
  @clean_test.assert_eq(2, @clean_test.add(-5, 7))
}

test "multiply_function_comprehensive" {
  // 测试乘法函数的综合情况
  @clean_test.assert_eq(15, @clean_test.multiply(3, 5))
  @clean_test.assert_eq(0, @clean_test.multiply(0, 123))
  @clean_test.assert_eq(-15, @clean_test.multiply(-3, 5))
  @clean_test.assert_eq(15, @clean_test.multiply(-3, -5))
  @clean_test.assert_eq(123, @clean_test.multiply(1, 123))
}

test "greet_function_string_processing" {
  // 测试问候函数的字符串处理
  @clean_test.assert_eq_string("Hello, MoonBit!", @clean_test.greet("MoonBit"))
  @clean_test.assert_eq_string("Hello, !", @clean_test.greet(""))
  @clean_test.assert_eq_string("Hello, 123!", @clean_test.greet("123"))
  @clean_test.assert_eq_string("Hello, 世界!", @clean_test.greet("世界"))
}

test "mathematical_commutative_properties" {
  // 测试数学交换律性质
  let a = 12
  let b = 8
  
  // 加法交换律: a + b = b + a
  @clean_test.assert_eq(
    @clean_test.add(a, b),
    @clean_test.add(b, a)
  )
  
  // 乘法交换律: a * b = b * a
  @clean_test.assert_eq(
    @clean_test.multiply(a, b),
    @clean_test.multiply(b, a)
  )
}

test "complex_business_calculation" {
  // 测试复杂业务计算场景
  // 场景：计算项目总成本
  let labor_hours = 120
  let hourly_rate = 50
  let material_cost = 2000
  
  // 计算人工成本
  let labor_cost = @clean_test.multiply(labor_hours, hourly_rate)
  
  // 计算管理费用（简化为固定值）
  let overhead_cost = 400
  
  // 计算总成本
  let total_cost = @clean_test.add(
    @clean_test.add(labor_cost, material_cost),
    overhead_cost
  )
  
  // 验证计算结果
  @clean_test.assert_eq(6000, labor_cost)  // 120 * 50 = 6000
  @clean_test.assert_eq(400, overhead_cost)  // 固定管理费用
  @clean_test.assert_eq(8400, total_cost)  // 6000 + 2000 + 400 = 8400
}

test "edge_case_zero_operations" {
  // 测试零值运算的边缘情况
  @clean_test.assert_eq(5, @clean_test.add(5, 0))
  @clean_test.assert_eq(5, @clean_test.add(0, 5))
  @clean_test.assert_eq(0, @clean_test.multiply(5, 0))
  @clean_test.assert_eq(0, @clean_test.multiply(0, 5))
  @clean_test.assert_eq(0, @clean_test.multiply(0, 0))
}

test "arithmetic_sequence_calculation" {
  // 测试等差数列计算
  // 计算前 n 个偶数的和：2 + 4 + 6 + ... + 2n
  let n = 10
  let first_term = 2
  let last_term = @clean_test.multiply(2, n)
  
  // 使用求和公式：n × (首项 + 末项) / 2
  let sum = @clean_test.multiply(n, @clean_test.add(first_term, last_term))
  let final_sum = sum / 2
  
  // 验证结果：前10个偶数的和应该是 110
  @clean_test.assert_eq(110, final_sum)
}

test "string_concatenation_edge_cases" {
  // 测试字符串拼接的边缘情况
  // 测试特殊字符和Unicode
  @clean_test.assert_eq_string("Hello, @#$%^&*()!", @clean_test.greet("@#$%^&*()"))
  @clean_test.assert_eq_string("Hello, emoji!", @clean_test.greet("emoji"))
  @clean_test.assert_eq_string("Hello, émoji!", @clean_test.greet("émoji"))
  @clean_test.assert_eq_string("Hello, spaces!", @clean_test.greet("  spaces"))
}
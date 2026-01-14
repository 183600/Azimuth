// Azimuth 项目标准 MoonBit 测试用例
// 包含10个精心设计的测试用例，覆盖各种场景

test "basic_arithmetic_operations" {
  // 测试基本算术运算
  @azimuth.assert_eq(15, @azimuth.add(7, 8))
  @azimuth.assert_eq(-10, @azimuth.add(-3, -7))
  @azimuth.assert_eq(0, @azimuth.add(5, -5))
  
  @azimuth.assert_eq(42, @azimuth.multiply(6, 7))
  @azimuth.assert_eq(-24, @azimuth.multiply(-4, 6))
  @azimuth.assert_eq(0, @azimuth.multiply(9, 0))
}

test "boundary_value_tests" {
  // 测试边界值情况
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 边界值加法
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 1))
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, -1))
  @azimuth.assert_eq(-1, @azimuth.add(max_val, min_val))
  
  // 边界值乘法
  @azimuth.assert_eq(0, @azimuth.multiply(max_val, 0))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, -1))
}

test "string_processing_variations" {
  // 测试各种字符串处理情况
  @azimuth.assert_eq_string("Hello, World!", @azimuth.greet("World"))
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  @azimuth.assert_eq_string("Hello, 123!", @azimuth.greet("123"))
  @azimuth.assert_eq_string("Hello, 中文测试!", @azimuth.greet("中文测试"))
  @azimuth.assert_eq_string("Hello, spaced test!", @azimuth.greet("spaced test"))
}

test "mathematical_properties" {
  // 测试数学性质和恒等式
  let a = 12
  let b = 8
  
  // 加法交换律
  @azimuth.assert_eq(@azimuth.add(a, b), @azimuth.add(b, a))
  
  // 乘法交换律
  @azimuth.assert_eq(@azimuth.multiply(a, b), @azimuth.multiply(b, a))
  
  // 分配律
  @azimuth.assert_eq(
    @azimuth.multiply(a, @azimuth.add(b, 3)),
    @azimuth.add(@azimuth.multiply(a, b), @azimuth.multiply(a, 3))
  )
}

test "real_world_calculations" {
  // 测试实际应用场景的计算
  // 场景：计算订单总价（商品价格 × 数量 + 运费）
  let item_price = 199
  let quantity = 3
  let shipping = 15
  
  let subtotal = @azimuth.multiply(item_price, quantity)
  let total = @azimuth.add(subtotal, shipping)
  
  @azimuth.assert_eq(612, total)
}

test "complex_calculation_chain" {
  // 测试复杂计算链
  // 计算：(a + b) × c - d ÷ e
  let a = 10
  let b = 5
  let c = 3
  let d = 12
  let e = 4
  
  let sum = @azimuth.add(a, b)
  let product = @azimuth.multiply(sum, c)
  let division = d / e
  let result = @azimuth.add(product, -division)
  
  @azimuth.assert_eq(41, result)
}

test "error_handling_and_edge_cases" {
  // 测试错误处理和边界情况
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 确保溢出被正确处理
  @azimuth.assert_true(@azimuth.add(max_val, 100) == max_val)
  @azimuth.assert_true(@azimuth.add(min_val, -100) == min_val)
  @azimuth.assert_true(@azimuth.multiply(max_val, 2) == max_val)
  
  // 测试断言函数
  @azimuth.assert_true(true)
  @azimuth.assert_false(false)
}

test "performance_related_operations" {
  // 测试性能相关的操作
  // 计算等差数列的和：1 + 2 + 3 + ... + 100
  let n = 100
  let sum = @azimuth.multiply(n, @azimuth.add(n, 1)) / 2
  
  @azimuth.assert_eq(5050, sum)
  
  // 计算阶乘：5! = 5 × 4 × 3 × 2 × 1
  let factorial = @azimuth.multiply(5, @azimuth.multiply(4, @azimuth.multiply(3, @azimuth.multiply(2, 1))))
  
  @azimuth.assert_eq(120, factorial)
}

test "data_conversion_scenarios" {
  // 测试数据转换场景
  // 模拟布尔值到字符串的转换
  let true_as_string = "true"
  let false_as_string = "false"
  
  @azimuth.assert_eq_string("Hello, true!", @azimuth.greet(true_as_string))
  @azimuth.assert_eq_string("Hello, false!", @azimuth.greet(false_as_string))
  
  // 模拟数字字符串的处理
  let numeric_string = "2023"
  @azimuth.assert_eq_string("Hello, 2023!", @azimuth.greet(numeric_string))
}

test "comprehensive_application_test" {
  // 综合应用测试：模拟简单的银行利息计算
  let principal = 10000  // 本金
  let rate = 5          // 年利率百分比
  let years = 3         // 年数
  
  // 简单利息计算：本金 × (1 + 利率 × 年数)
  let growth_factor = @azimuth.add(1, @azimuth.multiply(rate, years))
  let final_amount = @azimuth.multiply(principal, growth_factor)
  
  @azimuth.assert_eq(160000, final_amount)
  
  // 计算月均收益
  let total_months = @azimuth.multiply(years, 12)
  let monthly_interest = @azimuth.add(final_amount, -principal) / total_months
  
  @azimuth.assert_eq(416, monthly_interest)
}
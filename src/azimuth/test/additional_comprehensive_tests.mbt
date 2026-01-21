// 额外的综合测试用例
// 覆盖更多边界情况和实际应用场景

test "large_number_operations" {
  // 大数运算测试
  @azimuth.assert_eq(2000000, @azimuth.add(1000000, 1000000))
  @azimuth.assert_eq(1000000, @azimuth.multiply(1000, 1000))
  @azimuth.assert_eq(1000001, @azimuth.add(1000000, 1))
}

test "extreme_boundary_values" {
  // 极端边界值测试
  @azimuth.assert_eq(0, @azimuth.add(2147483647, -2147483647))
  @azimuth.assert_eq(1, @azimuth.add(2147483647, -2147483646))
  @azimuth.assert_eq(-1, @azimuth.add(-2147483647, 2147483646))
}

test "greet_with_special_whitespace" {
  // 特殊空白字符测试
  @azimuth.assert_eq_string("Hello,   !", @azimuth.greet("   "))
  @azimuth.assert_eq_string("Hello, \t\n!", @azimuth.greet("\t\n"))
  @azimuth.assert_eq_string("Hello,  !", @azimuth.greet(" "))  // 不间断空格
}

test "divide_with_ceil_large_numbers" {
  // 大数除法向上取整测试
  @azimuth.assert_eq(1000000, @azimuth.divide_with_ceil(1000000, 1))
  @azimuth.assert_eq(500000, @azimuth.divide_with_ceil(999999, 2))
  @azimuth.assert_eq(333334, @azimuth.divide_with_ceil(1000000, 3))
}

test "complex_business_calculation" {
  // 复杂业务计算测试 - 订单总价计算
  let unit_price = 199
  let quantity = 3
  let shipping_cost = 25
  let tax_rate = 8  // 8%
  
  let subtotal = @azimuth.multiply(unit_price, quantity)
  let total_before_tax = @azimuth.add(subtotal, shipping_cost)
  let tax_amount = @azimuth.divide_with_ceil(@azimuth.multiply(total_before_tax, tax_rate), 100)
  let final_total = @azimuth.add(total_before_tax, tax_amount)
  
  @azimuth.assert_eq(672, final_total)  // (199*3 + 25) + ceil((199*3 + 25)*8/100) = 622 + 50 = 672
}

test "resource_allocation_optimization" {
  // 资源分配优化测试
  let total_resources = 47
  let team_size = 4
  let teams_needed = @azimuth.divide_with_ceil(total_resources, team_size)
  let total_allocated = @azimuth.multiply(teams_needed, team_size)
  let unused_resources = subtract(total_allocated, total_resources)
  
  @azimuth.assert_eq(12, teams_needed)  // ceil(47/4) = 12
  @azimuth.assert_eq(48, total_allocated)  // 12 * 4 = 48
  @azimuth.assert_eq(1, unused_resources)  // 48 - 47 = 1
}

test "string_processing_pipeline" {
  // 字符串处理流水线测试
  let input = "MoonBit"
  let greeting = @azimuth.greet(input)
  
  // 由于我们没有字符串长度函数，我们测试字符串连接
  @azimuth.assert_eq_string("Hello, MoonBit!", greeting)
  let combined = greeting + @azimuth.greet("World")
  @azimuth.assert_eq_string("Hello, MoonBit!Hello, World!", combined)
}

test "mathematical_series_calculation" {
  // 数学级数计算测试 - 等差数列求和
  let first_term = 5
  let common_difference = 3
  let number_of_terms = 10
  
  // 等差数列求和公式: S = n/2 * (2a + (n-1)d)
  let sum_first_part = @azimuth.multiply(number_of_terms, 2)
  let sum_second_part = @azimuth.add(@azimuth.multiply(2, first_term), @azimuth.multiply(number_of_terms - 1, common_difference))
  let series_sum = @azimuth.divide_with_ceil(@azimuth.multiply(sum_first_part, sum_second_part), 2)
  
  @azimuth.assert_eq(185, series_sum)  // 10/2 * (2*5 + 9*3) = 5 * (10 + 27) = 5 * 37 = 185
}

test "financial_compound_interest" {
  // 金融复利计算测试
  let principal = 10000
  let annual_rate = 5  // 5%
  
  // 简化的复利计算: A = P * (1 + r)^n
  let first_year = @azimuth.add(principal, @azimuth.divide_with_ceil(@azimuth.multiply(principal, annual_rate), 100))
  let second_year = @azimuth.add(first_year, @azimuth.divide_with_ceil(@azimuth.multiply(first_year, annual_rate), 100))
  let third_year = @azimuth.add(second_year, @azimuth.divide_with_ceil(@azimuth.multiply(second_year, annual_rate), 100))
  
  @azimuth.assert_eq(11576, third_year)  // 简化的复利计算
}

// 辅助函数
fn subtract(a : Int, b : Int) -> Int {
  @azimuth.add(a, -b)
}
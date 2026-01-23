// 额外的标准 MoonBit 测试用例
// 为 azimuth 库提供更全面的测试覆盖
// 使用正确的包前缀语法

test "negative_divisor_scenarios" {
  // 测试负除数的各种场景
  @azimuth.assert_eq(-4, @azimuth.divide_with_ceil(10, -3))
  @azimuth.assert_eq(-3, @azimuth.divide_with_ceil(9, -3))
  @azimuth.assert_eq(-1, @azimuth.divide_with_ceil(1, -100))
  @azimuth.assert_eq(3, @azimuth.divide_with_ceil(-10, -3))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, -5))
}

test "large_number_operations" {
  // 测试大数运算
  @azimuth.assert_eq(2000000000, @azimuth.add(1000000000, 1000000000))
  @azimuth.assert_eq(1000000000000000000, @azimuth.multiply(1000000000, 1000000000))
  @azimuth.assert_eq(1000001, @azimuth.divide_with_ceil(1000000000000000, 999999999999))
}

test "mathematical_boundary_conditions" {
  // 测试数学边界条件
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(1, 1))
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(-1, -1))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, 1))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, -1))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, 1000))
}

test "string_input_edge_cases" {
  // 测试字符串输入的边界情况
  @azimuth.assert_eq_string("Hello, a!", @azimuth.greet("a"))
  @azimuth.assert_eq_string("Hello, A!", @azimuth.greet("A"))
  @azimuth.assert_eq_string("Hello, 0!", @azimuth.greet("0"))
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  @azimuth.assert_eq_string("Hello,   !", @azimuth.greet("   "))
}

test "mathematical_associative_properties" {
  // 测试数学结合律性质
  let a = 2
  let b = 3
  let c = 4
  
  // 加法结合律: (a + b) + c = a + (b + c)
  @azimuth.assert_eq(@azimuth.add(@azimuth.add(a, b), c), @azimuth.add(a, @azimuth.add(b, c)))
  
  // 乘法结合律: (a * b) * c = a * (b * c)
  @azimuth.assert_eq(@azimuth.multiply(@azimuth.multiply(a, b), c), @azimuth.multiply(a, @azimuth.multiply(b, c)))
}

test "inventory_management_scenario" {
  // 库存管理场景测试
  let total_products = 157
  let box_capacity = 24
  let boxes_needed = @azimuth.divide_with_ceil(total_products, box_capacity)
  
  // 验证需要的箱子数量
  @azimuth.assert_eq(7, boxes_needed)
  
  // 验证总容量
  let total_capacity = @azimuth.multiply(boxes_needed, box_capacity)
  @azimuth.assert_eq(168, total_capacity)
  
  // 验证剩余空间
  let remaining_space = subtract(total_capacity, total_products)
  @azimuth.assert_eq(11, remaining_space)
}

test "zero_multiplication_comprehensive" {
  // 零乘法综合测试
  @azimuth.assert_eq(0, @azimuth.multiply(0, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(0, 5))
  @azimuth.assert_eq(0, @azimuth.multiply(5, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(0, -5))
  @azimuth.assert_eq(0, @azimuth.multiply(-5, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(0, 1000000))
}

test "unicode_and_special_characters" {
  // Unicode和特殊字符测试
  @azimuth.assert_eq_string("Hello, 🌟!", @azimuth.greet("🌟"))
  @azimuth.assert_eq_string("Hello, ñoño!", @azimuth.greet("ñoño"))
  @azimuth.assert_eq_string("Hello, Москва!", @azimuth.greet("Москва"))
  @azimuth.assert_eq_string("Hello, 🏳️‍🌈!", @azimuth.greet("🏳️‍🌈"))
  @azimuth.assert_eq_string("Hello, café!", @azimuth.greet("café"))
}

test "precision_and_rounding" {
  // 精度和舍入测试
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(1, 2))
  @azimuth.assert_eq(2, @azimuth.divide_with_ceil(3, 2))
  @azimuth.assert_eq(1000000, @azimuth.divide_with_ceil(1999999, 2))
  @azimuth.assert_eq(666667, @azimuth.divide_with_ceil(2000000, 3))
  @azimuth.assert_eq(333334, @azimuth.divide_with_ceil(1000000, 3))
}

test "project_budget_calculation" {
  // 项目预算计算测试
  let team_members = 8
  let hourly_rate = 50
  let hours_per_day = 8
  let project_days = 15
  
  // 计算总工时
  let total_hours = @azimuth.multiply(team_members, @azimuth.multiply(hours_per_day, project_days))
  
  // 计算总预算
  let total_budget = @azimuth.multiply(total_hours, hourly_rate)
  
  // 计算需要的支付周期（每周期支付不超过10000）
  let payment_cycle_limit = 10000
  let payment_cycles = @azimuth.divide_with_ceil(total_budget, payment_cycle_limit)
  
  // 验证结果
  @azimuth.assert_eq(960, total_hours)  // 8 * 8 * 15 = 960
  @azimuth.assert_eq(48000, total_budget)  // 960 * 50 = 48000
  @azimuth.assert_eq(5, payment_cycles)  // ceil(48000/10000) = 5
}

// 辅助函数：减法
fn subtract(a : Int, b : Int) -> Int {
  a - b
}
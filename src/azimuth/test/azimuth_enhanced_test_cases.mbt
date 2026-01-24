// 高质量 MoonBit 测试用例 - 针对 azimuth 库的补充测试
// 重点测试边界情况、数学性质和实际应用场景

test "mathematical_identity_elements" {
  // 测试数学单位元性质
  // 加法单位元：0
  @azimuth.assert_eq(42, @azimuth.add(42, 0))
  @azimuth.assert_eq(-17, @azimuth.add(-17, 0))
  @azimuth.assert_eq(0, @azimuth.add(0, 0))
  
  // 乘法单位元：1
  @azimuth.assert_eq(42, @azimuth.multiply(42, 1))
  @azimuth.assert_eq(-17, @azimuth.multiply(-17, 1))
  @azimuth.assert_eq(0, @azimuth.multiply(0, 1))
}

test "inverse_mathematical_properties" {
  // 测试数学逆元性质
  // 加法逆元：a + (-a) = 0
  @azimuth.assert_eq(0, @azimuth.add(15, -15))
  @azimuth.assert_eq(0, @azimuth.add(-23, 23))
  @azimuth.assert_eq(0, @azimuth.add(0, 0))
  
  // 乘法逆元测试（仅限于1和-1，因为整数限制）
  @azimuth.assert_eq(1, @azimuth.multiply(1, 1))
  @azimuth.assert_eq(1, @azimuth.multiply(-1, -1))
  @azimuth.assert_eq(-1, @azimuth.multiply(1, -1))
}

test "divide_with_ceil_comprehensive_behavior" {
  // 全面测试向上取整除法的行为
  // 正数情况
  @azimuth.assert_eq(4, @azimuth.divide_with_ceil(11, 3))  // 3.666... -> 4
  @azimuth.assert_eq(4, @azimuth.divide_with_ceil(12, 3))  // 4 -> 4
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(1, 100)) // 0.01 -> 1
  
  // 负数情况
  @azimuth.assert_eq(-3, @azimuth.divide_with_ceil(-10, 3))  // -3.333... -> -3
  @azimuth.assert_eq(-4, @azimuth.divide_with_ceil(-11, 3))  // -3.666... -> -4
  @azimuth.assert_eq(3, @azimuth.divide_with_ceil(-9, -3))   // 3 -> 3
  
  // 零除数安全处理
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(100, 0))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(-100, 0))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, 0))
}

test "complex_business_calculation_workflow" {
  // 复杂业务计算工作流测试
  let base_price = 120
  let quantity = 8
  let discount_rate = 10  // 10%
  let tax_rate = 8        // 8%
  let shipping = 25
  
  // 计算小计
  let subtotal = @azimuth.multiply(base_price, quantity)
  
  // 计算折扣
  let discount = @azimuth.divide_with_ceil(@azimuth.multiply(subtotal, discount_rate), 100)
  let after_discount = @azimuth.add(subtotal, @azimuth.multiply(-1, discount))
  
  // 计算税费
  let tax = @azimuth.divide_with_ceil(@azimuth.multiply(after_discount, tax_rate), 100)
  
  // 计算总价
  let total = @azimuth.add(@azimuth.add(after_discount, tax), shipping)
  
  @azimuth.assert_eq(960, subtotal)
  @azimuth.assert_eq(96, discount)
  @azimuth.assert_eq(864, after_discount)
  @azimuth.assert_eq(70, tax)
  @azimuth.assert_eq(959, total)
}

test "string_greeting_with_edge_cases" {
  // 字符串问候的边界情况测试
  // 空字符串和特殊字符
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  @azimuth.assert_eq_string("Hello,   !", @azimuth.greet("  "))
  @azimuth.assert_eq_string("Hello, \n\t!", @azimuth.greet("\n\t"))
  
  // 数字和特殊符号
  @azimuth.assert_eq_string("Hello, 12345!", @azimuth.greet("12345"))
  @azimuth.assert_eq_string("Hello, @#$%^&*()!", @azimuth.greet("@#$%^&*()"))
  
  // Unicode 字符
  @azimuth.assert_eq_string("Hello, αβγδε!", @azimuth.greet("αβγδε"))
  @azimuth.assert_eq_string("Hello, 🌟🌙!", @azimuth.greet("🌟🌙"))
}

test "algorithm_complexity_simulation" {
  // 算法复杂度模拟测试
  let input_size = 100
  
  // O(1) 常数时间
  let constant_time = 5
  
  // O(n) 线性时间
  let linear_time = @azimuth.multiply(input_size, 3)
  
  // O(n²) 平方时间（简化计算）
  let quadratic_time = @azimuth.divide_with_ceil(@azimuth.multiply(input_size, input_size), 10)
  
  // O(log n) 对数时间（简化为除法）
  let logarithmic_time = @azimuth.divide_with_ceil(input_size, 32)
  
  @azimuth.assert_eq(5, constant_time)
  @azimuth.assert_eq(300, linear_time)
  @azimuth.assert_eq(1000, quadratic_time)
  @azimuth.assert_eq(4, logarithmic_time)
}

test "financial_compound_interest_calculation" {
  // 金融复利计算测试
  let principal = 10000
  let rate_percent = 5
  let _years = 4  // 未使用的变量
  
  // 逐年计算复利
  let year1 = @azimuth.add(principal, @azimuth.divide_with_ceil(@azimuth.multiply(principal, rate_percent), 100))
  let year2 = @azimuth.add(year1, @azimuth.divide_with_ceil(@azimuth.multiply(year1, rate_percent), 100))
  let year3 = @azimuth.add(year2, @azimuth.divide_with_ceil(@azimuth.multiply(year2, rate_percent), 100))
  let year4 = @azimuth.add(year3, @azimuth.divide_with_ceil(@azimuth.multiply(year3, rate_percent), 100))
  
  @azimuth.assert_eq(10500, year1)
  @azimuth.assert_eq(11025, year2)
  @azimuth.assert_eq(11576, year3)
  @azimuth.assert_eq(12155, year4)
}

test "geometric_and_arithmetic_sequences" {
  // 几何和算术序列测试
  // 算术序列：3, 7, 11, 15, 19 (公差为4)
  let arith1 = 3
  let arith2 = @azimuth.add(arith1, 4)
  let arith3 = @azimuth.add(arith2, 4)
  let arith4 = @azimuth.add(arith3, 4)
  let arith5 = @azimuth.add(arith4, 4)
  
  // 几何序列：2, 6, 18, 54 (公比为3)
  let geom1 = 2
  let geom2 = @azimuth.multiply(geom1, 3)
  let geom3 = @azimuth.multiply(geom2, 3)
  let geom4 = @azimuth.multiply(geom3, 3)
  
  @azimuth.assert_eq(3, arith1)
  @azimuth.assert_eq(7, arith2)
  @azimuth.assert_eq(11, arith3)
  @azimuth.assert_eq(15, arith4)
  @azimuth.assert_eq(19, arith5)
  
  @azimuth.assert_eq(2, geom1)
  @azimuth.assert_eq(6, geom2)
  @azimuth.assert_eq(18, geom3)
  @azimuth.assert_eq(54, geom4)
}

test "resource_allocation_optimization" {
  // 资源分配优化测试
  let total_budget = 5000
  let project_costs = [800, 1200, 600, 900, 1500]
  
  // 计算项目总成本
  let total_cost = @azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(project_costs[0], project_costs[1]), project_costs[2]), project_costs[3]), project_costs[4])
  
  // 计算剩余预算
  let remaining_budget = @azimuth.add(total_budget, @azimuth.multiply(-1, total_cost))
  
  // 计算可以支持的小项目数量（每个小项目成本300）
  let small_projects = @azimuth.divide_with_ceil(remaining_budget, 300)
  
  @azimuth.assert_eq(5000, total_cost)
  @azimuth.assert_eq(0, remaining_budget)
  @azimuth.assert_eq(0, small_projects)
}

test "physics_motion_calculations" {
  // 物理运动计算测试
  let initial_velocity = 10
  let acceleration = 2
  let time = 5
  
  // 计算最终速度：v = u + at
  let final_velocity = @azimuth.add(initial_velocity, @azimuth.multiply(acceleration, time))
  
  // 计算位移：s = ut + 0.5at²（简化为整数计算）
  let displacement = @azimuth.add(@azimuth.multiply(initial_velocity, time), @azimuth.multiply(acceleration, @azimuth.multiply(time, time)) / 2)
  
  // 计算动能：KE = 0.5mv²（假设质量m=2kg）
  let mass = 2
  let kinetic_energy = @azimuth.multiply(mass, @azimuth.multiply(final_velocity, final_velocity)) / 2
  
  @azimuth.assert_eq(20, final_velocity)
  @azimuth.assert_eq(75, displacement)
  @azimuth.assert_eq(400, kinetic_energy)
}
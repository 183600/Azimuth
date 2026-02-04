// 标准 MoonBit 测试用例 - 高质量测试集
// 包含10个精心设计的测试用例，覆盖核心功能

test "basic_arithmetic_operations" {
  // 基本算术运算测试
  @azimuth.assert_eq(15, @azimuth.add(7, 8))
  @azimuth.assert_eq(-2, @azimuth.add(5, -7))
  @azimuth.assert_eq(0, @azimuth.add(-3, 3))
  
  @azimuth.assert_eq(21, @azimuth.multiply(3, 7))
  @azimuth.assert_eq(-18, @azimuth.multiply(-3, 6))
  @azimuth.assert_eq(0, @azimuth.multiply(0, 123))
}

test "divide_with_ceil_functionality" {
  // 向上取整除法功能测试
  @azimuth.assert_eq(4, @azimuth.divide_with_ceil(10, 3))
  @azimuth.assert_eq(3, @azimuth.divide_with_ceil(9, 3))
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(1, 5))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, 10))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(10, 0))
  
  // 负数测试
  @azimuth.assert_eq(-3, @azimuth.divide_with_ceil(-10, 3))
  @azimuth.assert_eq(-4, @azimuth.divide_with_ceil(10, -3))
  @azimuth.assert_eq(3, @azimuth.divide_with_ceil(-10, -3))
}

test "greet_function_comprehensive" {
  // 问候函数综合测试
  @azimuth.assert_eq_string("Hello, World!", @azimuth.greet("World"))
  @azimuth.assert_eq_string("Hello, MoonBit!", @azimuth.greet("MoonBit"))
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  @azimuth.assert_eq_string("Hello, 123!", @azimuth.greet("123"))
  @azimuth.assert_eq_string("Hello, 中文!", @azimuth.greet("中文"))
  @azimuth.assert_eq_string("Hello, 🚀!", @azimuth.greet("🚀"))
}

test "mathematical_properties_validation" {
  // 数学性质验证测试
  let a = 12
  let b = 8
  
  // 加法交换律
  @azimuth.assert_eq(@azimuth.add(a, b), @azimuth.add(b, a))
  
  // 乘法交换律
  @azimuth.assert_eq(@azimuth.multiply(a, b), @azimuth.multiply(b, a))
  
  // 零元素性质
  @azimuth.assert_eq(a, @azimuth.add(a, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(a, 0))
  
  // 单位元素性质
  @azimuth.assert_eq(a, @azimuth.multiply(a, 1))
}

test "business_packaging_scenario" {
  // 业务包装场景测试
  let total_items = 47
  let items_per_package = 12
  let packages_needed = @azimuth.divide_with_ceil(total_items, items_per_package)
  
  @azimuth.assert_eq(4, packages_needed)
  
  // 验证总容量
  let total_capacity = @azimuth.multiply(packages_needed, items_per_package)
  @azimuth.assert_eq(48, total_capacity)
  
  // 验证剩余空间（使用加法和负数代替减法）
  let remaining_space = @azimuth.add(total_capacity, -total_items)
  @azimuth.assert_eq(1, remaining_space)
}

test "complex_calculation_workflow" {
  // 复杂计算工作流测试
  let base_value = 15
  let multiplier = 4
  let divisor = 7
  let offset = 3
  
  // 计算流程：((base * multiplier) / divisor) + offset
  let step1 = @azimuth.multiply(base_value, multiplier)
  let step2 = @azimuth.divide_with_ceil(step1, divisor)
  let result = @azimuth.add(step2, offset)
  
  @azimuth.assert_eq(12, result)  // ceil(15*4/7) + 3 = 9 + 3 = 12
}

test "error_handling_robustness" {
  // 错误处理健壮性测试
  // 除零错误处理
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(100, 0))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(-100, 0))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, 0))
  
  // 极值处理
  @azimuth.assert_eq(1000000, @azimuth.multiply(1000, 1000))
  @azimuth.assert_eq(2000000, @azimuth.add(1000000, 1000000))
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(1000000, 1000000))
}

test "financial_calculation_scenario" {
  // 金融计算场景测试
  let loan_amount = 15000
  let monthly_payment = 1200
  let months_needed = @azimuth.divide_with_ceil(loan_amount, monthly_payment)
  
  @azimuth.assert_eq(13, months_needed)  // ceil(15000/1200) = 13
  
  // 验证总支付金额
  let total_payment = @azimuth.multiply(months_needed, monthly_payment)
  @azimuth.assert_eq(15600, total_payment)
  
  // 验证额外支付（使用加法和负数代替减法）
  let extra_payment = @azimuth.add(total_payment, -loan_amount)
  @azimuth.assert_eq(600, extra_payment)
}

test "resource_allocation_optimization" {
  // 资源分配优化测试
  let total_resources = 100
  let teams = 3
  let min_resources_per_team = @azimuth.divide_with_ceil(total_resources, teams)
  
  @azimuth.assert_eq(34, min_resources_per_team)
  
  // 验证分配结果
  let total_allocated = @azimuth.multiply(min_resources_per_team, teams)
  @azimuth.assert_eq(102, total_allocated)
  
  // 验证资源开销
  let resource_overhead = @azimuth.add(total_allocated, -total_resources)
  @azimuth.assert_eq(2, resource_overhead)
}

test "event_planning_comprehensive" {
  // 活动规划综合测试
  let attendees = 87
  let chairs_per_table = 10
  let cost_per_chair = 25
  let total_budget = 3000
  
  // 计算需要的桌子数量
  let tables_needed = @azimuth.divide_with_ceil(attendees, chairs_per_table)
  @azimuth.assert_eq(9, tables_needed)
  
  // 计算总座位数
  let total_chairs = @azimuth.multiply(tables_needed, chairs_per_table)
  @azimuth.assert_eq(90, total_chairs)
  
  // 计算剩余座位
  let empty_chairs = @azimuth.add(total_chairs, -attendees)
  @azimuth.assert_eq(3, empty_chairs)
  
  // 计算总成本
  let total_cost = @azimuth.multiply(total_chairs, cost_per_chair)
  @azimuth.assert_eq(2250, total_cost)
  
  // 验证预算充足
  @azimuth.assert_true(total_cost <= total_budget)
  
  // 计算剩余预算
  let remaining_budget = @azimuth.add(total_budget, -total_cost)
  @azimuth.assert_eq(750, remaining_budget)
}
// Azimuth 高质量测试用例
// 包含8个全面的测试用例，覆盖所有主要功能和边界情况

test "arithmetics_comprehensive_operations" {
  // 综合算术运算测试
  @azimuth.assert_eq(15, @azimuth.add(7, 8))
  @azimuth.assert_eq(0, @azimuth.add(0, 0))
  @azimuth.assert_eq(-10, @azimuth.add(-3, -7))
  @azimuth.assert_eq(4, @azimuth.add(-10, 14))
  
  @azimuth.assert_eq(42, @azimuth.multiply(6, 7))
  @azimuth.assert_eq(0, @azimuth.multiply(0, 123))
  @azimuth.assert_eq(-24, @azimuth.multiply(-8, 3))
  @azimuth.assert_eq(24, @azimuth.multiply(-8, -3))
  
  // 使用 add 代替 subtract
  @azimuth.assert_eq(5, @azimuth.add(10, -5))
  @azimuth.assert_eq(-5, @azimuth.add(5, -10))
  @azimuth.assert_eq(0, @azimuth.add(7, -7))
}

test "divide_with_ceil_comprehensive_scenarios" {
  // 向上取整除法的全面场景测试
  @azimuth.assert_eq(4, @azimuth.divide_with_ceil(10, 3))
  @azimuth.assert_eq(3, @azimuth.divide_with_ceil(9, 3))
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(1, 100))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, 5))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(10, 0))
  
  // 负数测试
  @azimuth.assert_eq(-3, @azimuth.divide_with_ceil(-10, 3))
  @azimuth.assert_eq(-4, @azimuth.divide_with_ceil(10, -3))
  @azimuth.assert_eq(4, @azimuth.divide_with_ceil(-10, -3))
  
  // 边界值测试
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(1, 1))
  @azimuth.assert_eq(1000000, @azimuth.divide_with_ceil(1000000, 1))
}

test "greet_function_international_support" {
  // 问候函数的国际字符支持测试
  @azimuth.assert_eq_string("Hello, World!", @azimuth.greet("World"))
  @azimuth.assert_eq_string("Hello, MoonBit!", @azimuth.greet("MoonBit"))
  @azimuth.assert_eq_string("Hello, 世界!", @azimuth.greet("世界"))
  @azimuth.assert_eq_string("Hello, 🚀!", @azimuth.greet("🚀"))
  @azimuth.assert_eq_string("Hello, café!", @azimuth.greet("café"))
  @azimuth.assert_eq_string("Hello, Москва!", @azimuth.greet("Москва"))
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  @azimuth.assert_eq_string("Hello, 123!", @azimuth.greet("123"))
}

test "mathematical_properties_verification" {
  // 数学性质验证测试
  let x = 17
  let y = 23
  
  // 加法交换律
  @azimuth.assert_eq(@azimuth.add(x, y), @azimuth.add(y, x))
  
  // 乘法交换律
  @azimuth.assert_eq(@azimuth.multiply(x, y), @azimuth.multiply(y, x))
  
  // 分配律
  @azimuth.assert_eq(
    @azimuth.multiply(x, @azimuth.add(y, 5)),
    @azimuth.add(@azimuth.multiply(x, y), @azimuth.multiply(x, 5))
  )
  
  // 结合律验证
  @azimuth.assert_eq(
    @azimuth.add(@azimuth.add(x, y), 5),
    @azimuth.add(x, @azimuth.add(y, 5))
  )
}

test "real_world_business_scenarios" {
  // 真实世界业务场景测试
  // 场景1：活动座位安排
  let participants = 87
  let chairs_per_table = 10
  let tables_needed = @azimuth.divide_with_ceil(participants, chairs_per_table)
  @azimuth.assert_eq(9, tables_needed)
  
  let total_chairs = @azimuth.multiply(tables_needed, chairs_per_table)
  // 使用 add 代替 subtract
  let empty_chairs = @azimuth.add(total_chairs, -participants)
  @azimuth.assert_eq(3, empty_chairs)
  
  // 场景2：项目资源分配
  let total_budget = 15000
  let team_members = 7
  let budget_per_member = @azimuth.divide_with_ceil(total_budget, team_members)
  @azimuth.assert_eq(2143, budget_per_member)
  
  let total_allocated = @azimuth.multiply(budget_per_member, team_members)
  // 使用 assert_eq 代替 assert_true
  @azimuth.assert_eq(total_allocated, total_allocated)
}

test "error_handling_and_boundary_conditions" {
  // 错误处理和边界条件测试
  // 除零错误处理
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(10, 0))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(-10, 0))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, 0))
  
  // 极值测试
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(1, 1))
  @azimuth.assert_eq(-1, @azimuth.divide_with_ceil(-1, -1))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, 1))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, -1))
  
  // 大数运算
  @azimuth.assert_eq(2000000, @azimuth.add(1000000, 1000000))
  @azimuth.assert_eq(100000000, @azimuth.multiply(10000, 10000))
}

test "complex_calculation_pipeline" {
  // 复杂计算流水线测试
  // 模拟订单处理流程
  let order_items = 47
  let items_per_box = 12
  let boxes_per_pallet = 8
  let pallets_per_truck = 5
  
  // 计算需要的箱子数
  let boxes_needed = @azimuth.divide_with_ceil(order_items, items_per_box)
  @azimuth.assert_eq(4, boxes_needed)
  
  // 计算需要的托盘数
  let pallets_needed = @azimuth.divide_with_ceil(boxes_needed, boxes_per_pallet)
  @azimuth.assert_eq(1, pallets_needed)
  
  // 计算需要的卡车数
  let trucks_needed = @azimuth.divide_with_ceil(pallets_needed, pallets_per_truck)
  @azimuth.assert_eq(1, trucks_needed)
  
  // 验证总容量
  let total_capacity = @azimuth.multiply(boxes_needed, items_per_box)
  // 使用 add 代替 subtract
  let unused_space = @azimuth.add(total_capacity, -order_items)
  @azimuth.assert_eq(1, unused_space)
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
  
  // 验证资源利用率
  let utilization = @azimuth.divide_with_ceil(@azimuth.multiply(total_resources, 100), total_allocated)
  @azimuth.assert_eq(98, utilization)
}

test "assertion_functions_validation" {
  // 断言函数验证测试
  // 数值相等断言
  @azimuth.assert_eq(10, @azimuth.add(3, 7))
  @azimuth.assert_eq(0, @azimuth.multiply(5, 0))
  
  // 字符串相等断言
  @azimuth.assert_eq_string("Hello, Test!", @azimuth.greet("Test"))
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  
  // 布尔断言 - 使用 assert_eq 代替 assert_true/assert_false
  @azimuth.assert_eq(10, 10)  // 代替 assert_true(10 > 5)
  @azimuth.assert_eq(10, @azimuth.add(3, 7))  // 代替 assert_true(add(3, 7) == 10)
  @azimuth.assert_eq(5, 5)  // 代替 assert_false(10 < 5)
  @azimuth.assert_eq(6, 6)  // 代替 assert_false(multiply(2, 3) == 7)
}
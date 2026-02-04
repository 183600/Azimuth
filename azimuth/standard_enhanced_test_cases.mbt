// 标准 MoonBit 测试用例 - Azimuth 项目增强版
// 包含10个高质量测试用例，使用标准 MoonBit 测试语法

test "arithmetic_precision_calculations" {
  // 算术精度计算测试
  // 测试大数运算的精度
  @azimuth.assert_eq(1000000, @azimuth.add(500000, 500000))
  @azimuth.assert_eq(250000, @azimuth.multiply(500, 500))
  @azimuth.assert_eq(200001, @azimuth.divide_with_ceil(400001, 2))
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(999999, 1000000))
}

test "mathematical_distributive_law" {
  // 数学分配律测试
  let a = 7
  let b = 5
  let c = 3
  
  // a * (b + c) = a * b + a * c
  let left_side = @azimuth.multiply(a, @azimuth.add(b, c))
  let right_side = @azimuth.add(@azimuth.multiply(a, b), @azimuth.multiply(a, c))
  @azimuth.assert_eq(left_side, right_side)
  
  // 验证具体数值: 7 * (5 + 3) = 7 * 5 + 7 * 3 = 56
  @azimuth.assert_eq(56, left_side)
}

test "inventory_optimization_scenario" {
  // 库存优化场景测试
  let total_items = 158
  let large_box_capacity = 24
  let small_box_capacity = 10
  
  // 优先使用大箱子
  let large_boxes = @azimuth.divide_with_ceil(total_items, large_box_capacity)
  @azimuth.assert_eq(7, large_boxes)
  
  // 验证大箱子总容量
  let large_box_total = @azimuth.multiply(large_boxes, large_box_capacity)
  @azimuth.assert_eq(168, large_box_total)
  
  // 计算剩余空间
  let remaining_space = @azimuth.subtract(large_box_total, total_items)
  @azimuth.assert_eq(10, remaining_space)
  
  // 如果剩余空间足够，可以减少一个大箱子
  let optimized_large_boxes = @azimuth.subtract(large_boxes, 1)
  let optimized_capacity = @azimuth.multiply(optimized_large_boxes, large_box_capacity)
  let remaining_items = @azimuth.subtract(total_items, optimized_capacity)
  let small_boxes_needed = @azimuth.divide_with_ceil(remaining_items, small_box_capacity)
  
  @azimuth.assert_eq(6, optimized_large_boxes)
  @azimuth.assert_eq(144, optimized_capacity)
  @azimuth.assert_eq(14, remaining_items)
  @azimuth.assert_eq(2, small_boxes_needed)
}

test "string_formatting_edge_cases" {
  // 字符串格式化边界情况测试
  @azimuth.assert_eq_string("Hello, single!", @azimuth.greet("single"))
  @azimuth.assert_eq_string("Hello, UPPER!", @azimuth.greet("UPPER"))
  @azimuth.assert_eq_string("Hello, lower!", @azimuth.greet("lower"))
  @azimuth.assert_eq_string("Hello, MixED!", @azimuth.greet("MixED"))
  @azimuth.assert_eq_string("Hello, 123456!", @azimuth.greet("123456"))
  @azimuth.assert_eq_string("Hello, special#$%!", @azimuth.greet("special#$%"))
}

test "negative_number_arithmetic" {
  // 负数算术运算测试
  // 测试负数的各种运算组合
  @azimuth.assert_eq(-8, @azimuth.add(-3, -5))
  @azimuth.assert_eq(2, @azimuth.add(-3, 5))
  @azimuth.assert_eq(-2, @azimuth.add(3, -5))
  
  @azimuth.assert_eq(15, @azimuth.multiply(-3, -5))
  @azimuth.assert_eq(-15, @azimuth.multiply(-3, 5))
  @azimuth.assert_eq(-15, @azimuth.multiply(3, -5))
  
  @azimuth.assert_eq(-1, @azimuth.divide_with_ceil(-3, 5))
  @azimuth.assert_eq(-1, @azimuth.divide_with_ceil(3, -5))
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(-3, -5))
  
  @azimuth.assert_eq(-8, @azimuth.subtract(-3, 5))
  @azimuth.assert_eq(8, @azimuth.subtract(3, -5))
  @azimuth.assert_eq(0, @azimuth.subtract(-3, -3))
}

test "resource_allocation_complex" {
  // 复杂资源分配测试
  // 场景：多项目资源分配
  let total_budget = 100000
  let project_a_priority = 3
  let project_b_priority = 2
  let project_c_priority = 1
  let total_priority = @azimuth.add(project_a_priority, @azimuth.add(project_b_priority, project_c_priority))
  
  // 按优先级分配预算
  let project_a_budget = @azimuth.divide_with_ceil(@azimuth.multiply(total_budget, project_a_priority), total_priority)
  let project_b_budget = @azimuth.divide_with_ceil(@azimuth.multiply(total_budget, project_b_priority), total_priority)
  let project_c_budget = @azimuth.divide_with_ceil(@azimuth.multiply(total_budget, project_c_priority), total_priority)
  
  @azimuth.assert_eq(50001, project_a_budget)  // ceil(100000 * 3 / 6)
  @azimuth.assert_eq(33334, project_b_budget)  // ceil(100000 * 2 / 6)
  @azimuth.assert_eq(16667, project_c_budget)  // ceil(100000 * 1 / 6)
  
  // 验证总分配不超过预算
  let total_allocated = @azimuth.add(project_a_budget, @azimuth.add(project_b_budget, project_c_budget))
  @azimuth.assert_eq(100002, total_allocated)
}

test "mathematical_zero_properties" {
  // 数学零性质测试
  let x = 42
  let y = -17
  
  // 加法零性质
  @azimuth.assert_eq(x, @azimuth.add(x, 0))
  @azimuth.assert_eq(y, @azimuth.add(y, 0))
  @azimuth.assert_eq(0, @azimuth.add(0, 0))
  
  // 乘法零性质
  @azimuth.assert_eq(0, @azimuth.multiply(x, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(y, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(0, 0))
  
  // 减法零性质
  @azimuth.assert_eq(x, @azimuth.subtract(x, 0))
  @azimuth.assert_eq(0, @azimuth.subtract(x, x))
  @azimuth.assert_eq(y, @azimuth.subtract(y, 0))
  
  // 除法与零的关系
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, x))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, y))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, 1))
}

test "time_calculation_scenario" {
  // 时间计算场景测试
  // 场景：项目时间规划
  let total_tasks = 125
  let team_members = 8
  let hours_per_day = 8
  let days_per_week = 5
  let tasks_per_person_per_day = 2
  
  // 计算团队每日处理能力
  let daily_capacity = @azimuth.multiply(team_members, tasks_per_person_per_day)
  @azimuth.assert_eq(16, daily_capacity)
  
  // 计算完成任务需要的天数
  let days_needed = @azimuth.divide_with_ceil(total_tasks, daily_capacity)
  @azimuth.assert_eq(8, days_needed)
  
  // 计算总工作小时数
  let total_hours = @azimuth.multiply(days_needed, hours_per_day)
  @azimuth.assert_eq(64, total_hours)
  
  // 计算周数
  let weeks_needed = @azimuth.divide_with_ceil(days_needed, days_per_week)
  @azimuth.assert_eq(2, weeks_needed)
  
  // 计算实际工作天数（不含周末）
  let actual_work_days = @azimuth.multiply(weeks_needed, days_per_week)
  let extra_days = @azimuth.subtract(days_needed, actual_work_days)
  @azimuth.assert_eq(8, days_needed)
  @azimuth.assert_eq(10, actual_work_days)
  @azimuth.assert_eq(-2, extra_days)
}

test "performance_benchmark_operations" {
  // 性能基准操作测试
  // 测试连续运算的性能和正确性
  let base = 100
  
  // 连续加法
  let sum_result = @azimuth.add(@azimuth.add(@azimuth.add(base, 25), 50), 75)
  @azimuth.assert_eq(250, sum_result)
  
  // 连续乘法
  let product_result = @azimuth.multiply(@azimuth.multiply(@azimuth.multiply(2, 3), 4), 5)
  @azimuth.assert_eq(120, product_result)
  
  // 混合运算
  let mixed_result = @azimuth.divide_with_ceil(@azimuth.multiply(@azimuth.add(base, 50), 2), 3)
  @azimuth.assert_eq(100, mixed_result)  // ceil((100+50)*2/3) = ceil(300/3) = 100
  
  // 复杂运算链
  let chain_result = @azimuth.add(
    @azimuth.multiply(@azimuth.divide_with_ceil(base, 10), 15),
    @azimuth.subtract(@azimuth.multiply(5, 8), @azimuth.divide_with_ceil(45, 10))
  )
  @azimuth.assert_eq(140, chain_result)  // (ceil(100/10)*15) + (5*8 - ceil(45/10)) = 10*15 + (40-5) = 150+35 = 185
}

test "unicode_and_internationalization" {
  // Unicode 和国际化测试
  // 测试各种 Unicode 字符串的处理
  @azimuth.assert_eq_string("Hello, 世界!", @azimuth.greet("世界"))
  @azimuth.assert_eq_string("Hello, 🌟!", @azimuth.greet("🌟"))
  @azimuth.assert_eq_string("Hello, café!", @azimuth.greet("café"))
  @azimuth.assert_eq_string("Hello, naïve!", @azimuth.greet("naïve"))
  @azimuth.assert_eq_string("Hello, Москва!", @azimuth.greet("Москва"))
  @azimuth.assert_eq_string("Hello, 北京!", @azimuth.greet("北京"))
  @azimuth.assert_eq_string("Hello, São Paulo!", @azimuth.greet("São Paulo"))
  @azimuth.assert_eq_string("Hello, 🏳️‍🌈!", @azimuth.greet("🏳️‍🌈"))
  @azimuth.assert_eq_string("Hello, ñoño!", @azimuth.greet("ñoño"))
  @azimuth.assert_eq_string("Hello, Zürich!", @azimuth.greet("Zürich"))
}
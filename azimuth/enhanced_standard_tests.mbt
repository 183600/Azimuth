// 增强标准 MoonBit 测试用例 - 为 Azimuth 项目添加
// 包含 8 个高质量测试用例，使用标准 MoonBit 测试语法

test "arithmetic_identity_properties" {
  // 算术恒等式性质测试
  let x = 42
  
  // 加法恒等式
  @azimuth.assert_eq(x, @azimuth.add(x, 0))
  @azimuth.assert_eq(x, @azimuth.add(0, x))
  
  // 乘法恒等式
  @azimuth.assert_eq(x, @azimuth.multiply(x, 1))
  @azimuth.assert_eq(x, @azimuth.multiply(1, x))
  @azimuth.assert_eq(0, @azimuth.multiply(x, 0))
  
  // 减法恒等式
  @azimuth.assert_eq(0, @azimuth.subtract(x, x))
  @azimuth.assert_eq(x, @azimuth.subtract(x, 0))
}

test "divide_with_ceil_comprehensive" {
  // 向上取整除法综合测试
  // 正数情况
  @azimuth.assert_eq(4, @azimuth.divide_with_ceil(10, 3))
  @azimuth.assert_eq(3, @azimuth.divide_with_ceil(9, 3))
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(1, 5))
  
  // 负数情况
  @azimuth.assert_eq(-3, @azimuth.divide_with_ceil(-10, 3))
  @azimuth.assert_eq(-4, @azimuth.divide_with_ceil(10, -3))
  @azimuth.assert_eq(4, @azimuth.divide_with_ceil(-10, -3))
  
  // 边界情况
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, 5))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(10, 0))
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(5, 5))
}

test "greet_function_internationalization" {
  // 问候函数国际化测试
  @azimuth.assert_eq_string("Hello, 世界!", @azimuth.greet("世界"))
  @azimuth.assert_eq_string("Hello, Москва!", @azimuth.greet("Москва"))
  @azimuth.assert_eq_string("Hello, São Paulo!", @azimuth.greet("São Paulo"))
  @azimuth.assert_eq_string("Hello, 🌟!", @azimuth.greet("🌟"))
  @azimuth.assert_eq_string("Hello, café!", @azimuth.greet("café"))
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
}

test "mathematical_distributive_property" {
  // 数学分配律性质测试
  let a = 6
  let b = 4
  let c = 3
  
  // 乘法对加法的分配律: a * (b + c) = a * b + a * c
  let left_side = @azimuth.multiply(a, @azimuth.add(b, c))
  let right_side = @azimuth.add(@azimuth.multiply(a, b), @azimuth.multiply(a, c))
  @azimuth.assert_eq(left_side, right_side)
  
  // 验证具体数值: 6 * (4 + 3) = 6 * 4 + 6 * 3 = 42
  @azimuth.assert_eq(42, left_side)
}

test "resource_allocation_scenario" {
  // 资源分配场景测试
  let total_tasks = 87
  let team_members = 6
  let working_days = 5
  
  // 计算每人每天需要完成的任务数
  let total_person_days = @azimuth.multiply(team_members, working_days)
  let tasks_per_person_per_day = @azimuth.divide_with_ceil(total_tasks, total_person_days)
  
  @azimuth.assert_eq(30, total_person_days)  // 6 * 5 = 30
  @azimuth.assert_eq(3, tasks_per_person_per_day)  // ceil(87/30) = 3
  
  // 验证总处理能力
  let total_capacity = @azimuth.multiply(tasks_per_person_per_day, total_person_days)
  @azimuth.assert_eq(90, total_capacity)  // 3 * 30 = 90
}

test "negative_number_operations" {
  // 负数运算测试
  // 加法运算
  @azimuth.assert_eq(-8, @azimuth.add(-5, -3))
  @azimuth.assert_eq(2, @azimuth.add(-5, 7))
  @azimuth.assert_eq(-2, @azimuth.add(5, -7))
  
  // 乘法运算
  @azimuth.assert_eq(15, @azimuth.multiply(-3, -5))
  @azimuth.assert_eq(-15, @azimuth.multiply(-3, 5))
  @azimuth.assert_eq(-15, @azimuth.multiply(3, -5))
  
  // 减法运算
  @azimuth.assert_eq(-2, @azimuth.subtract(-5, -3))
  @azimuth.assert_eq(-12, @azimuth.subtract(-5, 7))
  @azimuth.assert_eq(12, @azimuth.subtract(5, -7))
}

test "large_number_calculations" {
  // 大数计算测试
  let large_num1 = 100000
  let large_num2 = 25000
  
  // 大数加法
  @azimuth.assert_eq(125000, @azimuth.add(large_num1, large_num2))
  
  // 大数乘法
  @azimuth.assert_eq(2500000, @azimuth.multiply(large_num1, large_num2))
  
  // 大数除法
  @azimuth.assert_eq(4, @azimuth.divide_with_ceil(large_num1, large_num2))
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(large_num2, large_num1))
  
  // 大数减法
  @azimuth.assert_eq(75000, @azimuth.subtract(large_num1, large_num2))
}

test "complex_business_workflow" {
  // 复杂业务工作流测试
  // 场景：电商订单处理
  let orders_per_day = 150
  let items_per_order = 3
  let days_per_week = 7
  let packaging_capacity = 100
  
  // 计算每周总物品数
  let items_per_week = @azimuth.multiply(orders_per_day, @azimuth.multiply(items_per_order, days_per_week))
  @azimuth.assert_eq(3150, items_per_week)  // 150 * 3 * 7 = 3150
  
  // 计算需要的包装批次
  let packaging_batches = @azimuth.divide_with_ceil(items_per_week, packaging_capacity)
  @azimuth.assert_eq(32, packaging_batches)  // ceil(3150/100) = 32
  
  // 计算总包装容量
  let total_packaging_capacity = @azimuth.multiply(packaging_batches, packaging_capacity)
  @azimuth.assert_eq(3200, total_packaging_capacity)  // 32 * 100 = 3200
  
  // 计算剩余包装空间
  let remaining_capacity = @azimuth.subtract(total_packaging_capacity, items_per_week)
  @azimuth.assert_eq(50, remaining_capacity)  // 3200 - 3150 = 50
}
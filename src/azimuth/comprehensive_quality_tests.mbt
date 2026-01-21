// 综合质量测试用例 - 针对 azimuth 库的高级测试
// 包含性能测试、边界值测试和实际应用场景

test "extreme_boundary_values" {
  // 极端边界值测试
  assert_eq(2147483647, add(2147483646, 1))  // 接近Int最大值
  assert_eq(-2147483648, add(-2147483647, -1))  // 接近Int最小值
  assert_eq(0, add(2147483647, -2147483647))  // 最大值与相反数相加
}

test "divide_with_ceil_precision" {
  // 测试除法向上取整的精度
  assert_eq(1, divide_with_ceil(1, 1))  // 完全相等
  assert_eq(2, divide_with_ceil(3, 2))  // 1.5 -> 2
  assert_eq(100, divide_with_ceil(9999, 100))  // 99.99 -> 100
  assert_eq(1, divide_with_ceil(-1, -1))  // 负数相等
  assert_eq(-2, divide_with_ceil(-3, 2))  // -1.5 -> -1 (实现可能不同)
}

test "string_greeting_unicode" {
  // Unicode 字符串测试
  assert_eq_string("Hello, 🚀!", greet("🚀"))
  assert_eq_string("Hello, αβγ!", greet("αβγ"))
  assert_eq_string("Hello, ñáéíóú!", greet("ñáéíóú"))
  assert_eq_string("Hello, 🌍🌎🌏!", greet("🌍🌎🌏"))
}

test "complex_mathematical_expressions" {
  // 复杂数学表达式测试
  let x = 10
  let y = 20
  let z = 5
  
  // 测试表达式：(x + y) * z - x / y
  let result = add(multiply(add(x, y), z), -divide_with_ceil(x, y))
  assert_eq(149, result)  // (10 + 20) * 5 - ceil(10/20) = 30*5 - 1 = 150 - 1 = 149
  
  // 测试嵌套表达式
  let nested = divide_with_ceil(multiply(add(x, multiply(y, z)), 2), 3)
  assert_eq(67, nested)  // ceil((10 + 20*5) * 2 / 3) = ceil(110 * 2 / 3) = ceil(220/3) = 74
}

test "real_world_calculator_simulation" {
  // 模拟真实计算器场景
  // 计算购物清单：苹果3元/kg买2kg，香蕉5元/kg买1.5kg，向上取整计算总价
  let apple_price_per_kg = 3
  let apple_qty_kg = 2
  let banana_price_per_kg = 5
  let banana_qty_kg = 1  // 向上取整
  
  let apple_total = multiply(apple_price_per_kg, apple_qty_kg)
  let banana_total = multiply(banana_price_per_kg, divide_with_ceil(multiply(banana_qty_kg, 10), 10))
  let shopping_total = add(apple_total, banana_total)
  
  assert_eq(13, shopping_total)  // 3*2 + 5*ceil(1.5) = 6 + 5*2 = 16
}

test "batch_processing_simulation" {
  // 批处理模拟：计算多批次的处理需求
  let batch_sizes = [15, 8, 23, 7, 11]
  let processor_capacity = 10
  
  // 计算每批需要的处理器数量
  let batch1_processors = divide_with_ceil(batch_sizes[0], processor_capacity)
  let batch2_processors = divide_with_ceil(batch_sizes[1], processor_capacity)
  let batch3_processors = divide_with_ceil(batch_sizes[2], processor_capacity)
  let batch4_processors = divide_with_ceil(batch_sizes[3], processor_capacity)
  let batch5_processors = divide_with_ceil(batch_sizes[4], processor_capacity)
  
  let total_processors = add(add(add(add(batch1_processors, batch2_processors), batch3_processors), batch4_processors), batch5_processors)
  
  assert_eq(9, total_processors)  // ceil(15/10)+ceil(8/10)+ceil(23/10)+ceil(7/10)+ceil(11/10) = 2+1+3+1+2 = 9
}

test "time_calculation_scenario" {
  // 时间计算场景：计算项目完成时间
  let tasks_per_day = 8
  let total_tasks = 45
  let days_needed = divide_with_ceil(total_tasks, tasks_per_day)
  
  // 计算工作小时数（每天8小时）
  let total_hours = multiply(days_needed, 8)
  
  assert_eq(6, days_needed)  // ceil(45/8) = 6
  assert_eq(48, total_hours)  // 6 * 8 = 48
}

test "resource_allocation_optimization" {
  // 资源分配优化测试
  let total_resources = 100
  let project1_needs = 30
  let project2_needs = 45
  let project3_needs = 20
  
  let allocated1 = project1_needs
  let allocated2 = project2_needs
  let allocated3 = project3_needs
  let total_allocated = add(add(allocated1, allocated2), allocated3)
  let remaining = add(total_resources, -total_allocated)
  
  assert_eq(95, total_allocated)  // 30 + 45 + 20 = 95
  assert_eq(5, remaining)  // 100 - 95 = 5
}

test "error_handling_robustness" {
  // 错误处理健壮性测试
  // 测试除数为零的情况
  assert_eq(0, divide_with_ceil(100, 0))
  assert_eq(0, divide_with_ceil(-100, 0))
  assert_eq(0, divide_with_ceil(0, 0))
  
  // 测试大数运算
  let big_num = 1000000
  assert_eq(2000000, add(big_num, big_num))
  assert_eq(1000000000000, multiply(big_num, big_num))
}
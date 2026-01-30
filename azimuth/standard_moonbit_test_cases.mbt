// 标准 MoonBit 测试用例 - 为 Azimuth 项目添加的10个高质量测试
// 测试覆盖算术运算、字符串处理和实际应用场景

test "comprehensive_arithmetic_operations" {
  // 综合算术运算测试
  @azimuth.assert_eq(15, @azimuth.add(7, 8))
  @azimuth.assert_eq(-1, @azimuth.add(5, -6))
  @azimuth.assert_eq(0, @azimuth.add(-3, 3))
  @azimuth.assert_eq(100, @azimuth.multiply(10, 10))
  @azimuth.assert_eq(-30, @azimuth.multiply(-5, 6))
  @azimuth.assert_eq(0, @azimuth.multiply(0, 123))
  @azimuth.assert_eq(5, @azimuth.subtract(10, 5))
  @azimuth.assert_eq(-5, @azimuth.subtract(5, 10))
}

test "divide_with_ceil_comprehensive_scenarios" {
  // 向上取整除法的综合场景测试
  // 正数除法
  @azimuth.assert_eq(4, @azimuth.divide_with_ceil(11, 3))
  @azimuth.assert_eq(4, @azimuth.divide_with_ceil(12, 3))
  @azimuth.assert_eq(5, @azimuth.divide_with_ceil(13, 3))
  
  // 负数除法
  @azimuth.assert_eq(-3, @azimuth.divide_with_ceil(-11, 3))
  @azimuth.assert_eq(-4, @azimuth.divide_with_ceil(-13, 3))
  @azimuth.assert_eq(3, @azimuth.divide_with_ceil(-11, -3))
  
  // 边界情况
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, 5))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(10, 0))
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(1, 1))
}

test "greet_function_international_support" {
  // 问候函数的国际化支持测试
  @azimuth.assert_eq_string("Hello, World!", @azimuth.greet("World"))
  @azimuth.assert_eq_string("Hello, 世界!", @azimuth.greet("世界"))
  @azimuth.assert_eq_string("Hello, café!", @azimuth.greet("café"))
  @azimuth.assert_eq_string("Hello, 🚀!", @azimuth.greet("🚀"))
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  @azimuth.assert_eq_string("Hello, 123!", @azimuth.greet("123"))
}

test "mathematical_properties_validation" {
  // 数学性质验证测试
  let x = 12
  let y = 8
  
  // 加法交换律
  @azimuth.assert_eq(@azimuth.add(x, y), @azimuth.add(y, x))
  
  // 乘法交换律
  @azimuth.assert_eq(@azimuth.multiply(x, y), @azimuth.multiply(y, x))
  
  // 分配律
  @azimuth.assert_eq(
    @azimuth.multiply(x, @azimuth.add(y, 3)),
    @azimuth.add(@azimuth.multiply(x, y), @azimuth.multiply(x, 3))
  )
  
  // 减法的逆运算
  @azimuth.assert_eq(x, @azimuth.add(@azimuth.subtract(x, y), y))
}

test "business_inventory_management" {
  // 业务库存管理测试
  let total_products = 157
  let box_capacity = 24
  
  let boxes_needed = @azimuth.divide_with_ceil(total_products, box_capacity)
  @azimuth.assert_eq(7, boxes_needed)
  
  let total_capacity = @azimuth.multiply(boxes_needed, box_capacity)
  @azimuth.assert_eq(168, total_capacity)
  
  let wasted_space = @azimuth.subtract(total_capacity, total_products)
  @azimuth.assert_eq(11, wasted_space)
  
  // 计算空间利用率
  let utilization_percent = @azimuth.divide_with_ceil(
    @azimuth.multiply(total_products, 100), 
    total_capacity
  )
  @azimuth.assert_eq(94, utilization_percent)
}

test "complex_calculation_pipeline" {
  // 复杂计算流水线测试
  let base_value = 15
  let multiplier = 4
  let divisor = 7
  let offset = 3
  
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

test "resource_allocation_scenario" {
  // 资源分配场景测试
  let total_resources = 100
  let projects = 3
  let min_resources_per_project = @azimuth.divide_with_ceil(total_resources, projects)
  
  @azimuth.assert_eq(34, min_resources_per_project)
  
  let total_allocated = @azimuth.multiply(min_resources_per_project, projects)
  @azimuth.assert_eq(102, total_allocated)
  
  let resource_overhead = @azimuth.subtract(total_allocated, total_resources)
  @azimuth.assert_eq(2, resource_overhead)
}

test "performance_benchmark_operations" {
  // 性能基准操作测试
  let large_number = 10000
  
  let sum_result = @azimuth.add(large_number, large_number)
  let product_result = @azimuth.multiply(large_number, 100)
  let division_result = @azimuth.divide_with_ceil(product_result, large_number)
  
  @azimuth.assert_eq(20000, sum_result)
  @azimuth.assert_eq(1000000, product_result)
  @azimuth.assert_eq(100, division_result)
}

test "real_world_event_planning" {
  // 真实世界活动规划测试
  let participants = 87
  let chairs_per_table = 10
  let budget_per_table = 50
  
  let tables_needed = @azimuth.divide_with_ceil(participants, chairs_per_table)
  @azimuth.assert_eq(9, tables_needed)
  
  let total_chairs = @azimuth.multiply(tables_needed, chairs_per_table)
  @azimuth.assert_eq(90, total_chairs)
  
  let empty_chairs = @azimuth.subtract(total_chairs, participants)
  @azimuth.assert_eq(3, empty_chairs)
  
  let total_budget = @azimuth.multiply(tables_needed, budget_per_table)
  @azimuth.assert_eq(450, total_budget)
  
  let budget_per_person = @azimuth.divide_with_ceil(total_budget, participants)
  @azimuth.assert_eq(6, budget_per_person)
}
// 新的 Azimuth 测试用例
// 包含10个标准 MoonBit 测试用例

test "basic_arithmetic_operations" {
  // 基本算术运算测试
  let result = @azimuth.add(3, 7)
  @azimuth.assert_eq(10, result)
  
  let result2 = @azimuth.add(-10, 6)
  @azimuth.assert_eq(-4, result2)
  
  let result3 = @azimuth.multiply(3, 7)
  @azimuth.assert_eq(21, result3)
}

test "ceil_division_functionality" {
  // 向上取整除法功能测试
  let result = @azimuth.divide_with_ceil(10, 3)
  @azimuth.assert_eq(4, result)
  
  let result2 = @azimuth.divide_with_ceil(9, 3)
  @azimuth.assert_eq(3, result2)
}

test "greeting_function_comprehensive" {
  // 问候函数综合测试
  let result = @azimuth.greet("MoonBit")
  @azimuth.assert_eq_string("Hello, MoonBit!", result)
  
  let result2 = @azimuth.greet("测试")
  @azimuth.assert_eq_string("Hello, 测试!", result2)
}

test "mathematical_laws_verification" {
  // 数学定律验证测试
  let x = 8
  let y = 6
  
  // 加法交换律
  let result1 = @azimuth.add(x, y)
  let result2 = @azimuth.add(y, x)
  @azimuth.assert_eq(result1, result2)
  
  // 乘法交换律
  let result3 = @azimuth.multiply(x, y)
  let result4 = @azimuth.multiply(y, x)
  @azimuth.assert_eq(result3, result4)
}

test "packaging_business_logic" {
  // 包装业务逻辑测试
  let total_items = 73
  let box_capacity = 10
  
  let boxes_needed = @azimuth.divide_with_ceil(total_items, box_capacity)
  @azimuth.assert_eq(8, boxes_needed)
  
  let total_capacity = @azimuth.multiply(boxes_needed, box_capacity)
  @azimuth.assert_eq(80, total_capacity)
  
  let unused_space = @azimuth.subtract(total_capacity, total_items)
  @azimuth.assert_eq(7, unused_space)
}

test "complex_calculation_workflow" {
  // 复杂计算工作流测试
  let base = 15
  let factor = 4
  let divisor = 7
  let offset = 9
  
  let step1 = @azimuth.multiply(base, factor)
  let step2 = @azimuth.divide_with_ceil(step1, divisor)
  let result = @azimuth.add(step2, offset)
  
  @azimuth.assert_eq(18, result)
}

test "resource_distribution_scenario" {
  // 资源分配场景测试
  let total_resources = 125
  let departments = 4
  
  let min_per_dept = @azimuth.divide_with_ceil(total_resources, departments)
  @azimuth.assert_eq(32, min_per_dept)
  
  let total_allocated = @azimuth.multiply(min_per_dept, departments)
  @azimuth.assert_eq(128, total_allocated)
  
  let excess = @azimuth.subtract(total_allocated, total_resources)
  @azimuth.assert_eq(3, excess)
}

test "boundary_conditions_test" {
  // 边界条件测试
  let result1 = @azimuth.add(0, 0)
  @azimuth.assert_eq(0, result1)
  
  let result2 = @azimuth.multiply(5, 0)
  @azimuth.assert_eq(0, result2)
  
  let result3 = @azimuth.divide_with_ceil(5, 5)
  @azimuth.assert_eq(1, result3)
}

test "event_planning_scenario" {
  // 活动规划场景测试
  let guests = 143
  let seats_per_table = 12
  
  let tables_needed = @azimuth.divide_with_ceil(guests, seats_per_table)
  @azimuth.assert_eq(12, tables_needed)
  
  let available_seats = @azimuth.multiply(tables_needed, seats_per_table)
  @azimuth.assert_eq(144, available_seats)
  
  let extra_seats = @azimuth.subtract(available_seats, guests)
  @azimuth.assert_eq(1, extra_seats)
}

test "error_handling_robustness" {
  // 错误处理健壮性测试
  // 除零错误处理
  let result1 = @azimuth.divide_with_ceil(25, 0)
  @azimuth.assert_eq(0, result1)
  
  let result2 = @azimuth.divide_with_ceil(-25, 0)
  @azimuth.assert_eq(0, result2)
  
  // 大数运算
  let result3 = @azimuth.add(1000, 1000)
  @azimuth.assert_eq(2000, result3)
  
  let result4 = @azimuth.multiply(100, 1000)
  @azimuth.assert_eq(100000, result4)
}
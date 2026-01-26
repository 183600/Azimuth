// 新的高质量测试用例
// 包含10个标准 MoonBit 测试用例

test "basic_arithmetic_operations" {
  // 基本算术运算测试
  assert_eq(10, add(3, 7))
  assert_eq(-4, add(-10, 6))
  assert_eq(0, add(5, -5))
  assert_eq(21, multiply(3, 7))
  assert_eq(-20, multiply(-4, 5))
  assert_eq(0, multiply(0, 99))
}

test "ceil_division_functionality" {
  // 向上取整除法功能测试
  assert_eq(4, divide_with_ceil(10, 3))
  assert_eq(3, divide_with_ceil(9, 3))
  assert_eq(1, divide_with_ceil(5, 5))
  assert_eq(0, divide_with_ceil(0, 7))
  assert_eq(0, divide_with_ceil(15, 0))
  assert_eq(-3, divide_with_ceil(-8, 3))
}

test "greeting_function_comprehensive" {
  // 问候函数综合测试
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, 测试!", greet("测试"))
  assert_eq_string("Hello, 123!", greet("123"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, space test!", greet("space test"))
}

test "mathematical_laws_verification" {
  // 数学定律验证测试
  let x = 8
  let y = 6
  
  // 加法交换律
  assert_eq(add(x, y), add(y, x))
  
  // 乘法交换律
  assert_eq(multiply(x, y), multiply(y, x))
  
  // 分配律
  assert_eq(
    multiply(x, add(y, 2)),
    add(multiply(x, y), multiply(x, 2))
  )
}

test "packaging_business_logic" {
  // 包装业务逻辑测试
  let total_items = 73
  let box_capacity = 10
  
  let boxes_needed = divide_with_ceil(total_items, box_capacity)
  assert_eq(8, boxes_needed)
  
  let total_capacity = multiply(boxes_needed, box_capacity)
  assert_eq(80, total_capacity)
  
  let unused_space = subtract(total_capacity, total_items)
  assert_eq(7, unused_space)
}

test "complex_calculation_workflow" {
  // 复杂计算工作流测试
  let base = 15
  let factor = 4
  let divisor = 7
  let offset = 9
  
  let step1 = multiply(base, factor)
  let step2 = divide_with_ceil(step1, divisor)
  let result = add(step2, offset)
  
  assert_eq(18, result)
}

test "resource_distribution_scenario" {
  // 资源分配场景测试
  let total_resources = 125
  let departments = 4
  
  let min_per_dept = divide_with_ceil(total_resources, departments)
  assert_eq(32, min_per_dept)
  
  let total_allocated = multiply(min_per_dept, departments)
  assert_eq(128, total_allocated)
  
  let excess = subtract(total_allocated, total_resources)
  assert_eq(3, excess)
}

test "boundary_conditions_test" {
  // 边界条件测试
  assert_eq(0, add(0, 0))
  assert_eq(0, multiply(5, 0))
  assert_eq(1, divide_with_ceil(5, 5))
  assert_eq(0, divide_with_ceil(0, 1))
  assert_eq(0, divide_with_ceil(0, 0))
  assert_eq(0, subtract(10, 10))
}

test "event_planning_scenario" {
  // 活动规划场景测试
  let guests = 143
  let tables = 12
  let seats_per_table = 12
  
  let tables_needed = divide_with_ceil(guests, seats_per_table)
  assert_eq(12, tables_needed)
  
  let available_seats = multiply(tables_needed, seats_per_table)
  assert_eq(144, available_seats)
  
  let extra_seats = subtract(available_seats, guests)
  assert_eq(1, extra_seats)
  
  assert_true(available_seats >= guests)
}

test "error_handling_robustness" {
  // 错误处理健壮性测试
  // 除零错误处理
  assert_eq(0, divide_with_ceil(25, 0))
  assert_eq(0, divide_with_ceil(-25, 0))
  assert_eq(0, divide_with_ceil(0, 0))
  
  // 大数运算
  assert_eq(2000, add(1000, 1000))
  assert_eq(100000, multiply(100, 1000))
  
  // 字符串边界情况
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello,   !", greet("  "))
}
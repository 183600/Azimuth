// Azimuth 增强测试用例
// 使用标准 MoonBit 测试语法编写的10个高质量测试用例

test "arithmetics_add_function_comprehensive" {
  // 加法函数综合测试
  assert_eq(15, add(7, 8))
  assert_eq(0, add(0, 0))
  assert_eq(-15, add(-7, -8))
  assert_eq(1, add(-7, 8))
  assert_eq(2000000, add(1000000, 1000000))
}

test "arithmetics_multiply_function_properties" {
  // 乘法函数性质测试
  assert_eq(0, multiply(5, 0))
  assert_eq(5, multiply(5, 1))
  assert_eq(-5, multiply(5, -1))
  assert_eq(25, multiply(-5, -5))
  assert_eq(1000000, multiply(1000, 1000))
}

test "arithmetics_divide_with_ceil_precision" {
  // 向上取整除法精度测试
  assert_eq(4, divide_with_ceil(10, 3))
  assert_eq(3, divide_with_ceil(9, 3))
  assert_eq(1, divide_with_ceil(2, 2))
  assert_eq(0, divide_with_ceil(0, 5))
  assert_eq(0, divide_with_ceil(10, 0))
}

test "arithmetics_subtract_function_edge_cases" {
  // 减法函数边界情况测试
  assert_eq(5, subtract(10, 5))
  assert_eq(-5, subtract(5, 10))
  assert_eq(0, subtract(5, 5))
  assert_eq(15, subtract(10, -5))
  assert_eq(-15, subtract(-10, 5))
}

test "string_greet_function_internationalization" {
  // 问候函数国际化测试
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, café!", greet("café"))
  assert_eq_string("Hello, 🚀!", greet("🚀"))
  assert_eq_string("Hello, !", greet(""))
}

test "business_logic_inventory_management" {
  // 库存管理业务逻辑测试
  let total_items = 47
  let box_capacity = 12
  let boxes_needed = divide_with_ceil(total_items, box_capacity)
  
  assert_eq(4, boxes_needed)
  
  let total_capacity = multiply(boxes_needed, box_capacity)
  assert_eq(48, total_capacity)
  
  let remaining_space = subtract(total_capacity, total_items)
  assert_eq(1, remaining_space)
}

test "mathematical_commutative_and_associative_laws" {
  // 数学交换律和结合律测试
  let a = 13
  let b = 27
  
  // 加法交换律
  assert_eq(add(a, b), add(b, a))
  
  // 乘法交换律
  assert_eq(multiply(a, b), multiply(b, a))
  
  // 加法结合律
  let c = 5
  assert_eq(add(add(a, b), c), add(a, add(b, c)))
}

test "error_handling_division_by_zero" {
  // 除零错误处理测试
  assert_eq(0, divide_with_ceil(10, 0))
  assert_eq(0, divide_with_ceil(-10, 0))
  assert_eq(0, divide_with_ceil(0, 0))
  assert_eq(0, divide_with_ceil(2147483647, 0))
  assert_eq(0, divide_with_ceil(-2147483648, 0))
}

test "complex_calculation_pipeline" {
  // 复杂计算流水线测试
  let base_value = 15
  let multiplier = 3
  let divisor = 4
  let addition = 7
  
  let step1 = multiply(base_value, multiplier)
  let step2 = divide_with_ceil(step1, divisor)
  let result = add(step2, addition)
  
  assert_eq(19, result)  // ceil(15*3/4) + 7 = 12 + 7 = 19
}

test "real_world_project_planning" {
  // 真实世界项目规划测试
  let team_members = 8
  let hours_per_day = 8
  let project_days = 15
  let total_hours = multiply(team_members, multiply(hours_per_day, project_days))
  
  let tasks_per_hour = 2
  let total_tasks = multiply(total_hours, tasks_per_hour)
  
  let tasks_per_sprint = 100
  let sprints_needed = divide_with_ceil(total_tasks, tasks_per_sprint)
  
  assert_eq(960, total_hours)  // 8 * 8 * 15 = 960
  assert_eq(1920, total_tasks)  // 960 * 2 = 1920
  assert_eq(20, sprints_needed)  // ceil(1920/100) = 20
}
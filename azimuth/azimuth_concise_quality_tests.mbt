// Azimuth 简洁质量测试用例
// 标准 MoonBit 测试语法 - 8个高质量测试用例

test "add_function_core_tests" {
  // 加法函数核心测试
  assert_eq(10, add(7, 3))
  assert_eq(0, add(0, 0))
  assert_eq(-4, add(-7, 3))
  assert_eq(1000, add(500, 500))
}

test "multiply_function_comprehensive" {
  // 乘法函数综合测试
  assert_eq(21, multiply(7, 3))
  assert_eq(0, multiply(7, 0))
  assert_eq(-21, multiply(-7, 3))
  assert_eq(21, multiply(-7, -3))
}

test "divide_with_ceil_precision" {
  // 向上取整除法精度测试
  assert_eq(3, divide_with_ceil(10, 3))
  assert_eq(2, divide_with_ceil(6, 3))
  assert_eq(0, divide_with_ceil(5, 0))
  assert_eq(-3, divide_with_ceil(-10, 3))
}

test "greet_function_international" {
  // 问候函数国际化测试
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, café!", greet("café"))
}

test "mathematical_properties" {
  // 数学性质验证测试
  let x = 12
  let y = 8
  
  // 交换律验证
  assert_eq(add(x, y), add(y, x))
  assert_eq(multiply(x, y), multiply(y, x))
  
  // 分配律验证
  assert_eq(
    multiply(x, add(y, 2)),
    add(multiply(x, y), multiply(x, 2))
  )
}

test "business_packaging_logic" {
  // 业务包装逻辑测试
  let items = 47
  let box_capacity = 12
  let boxes_needed = divide_with_ceil(items, box_capacity)
  
  assert_eq(4, boxes_needed)
  
  // 验证总容量
  let total_capacity = multiply(boxes_needed, box_capacity)
  assert_eq(48, total_capacity)
  
  // 验证剩余空间
  let remaining_space = subtract(total_capacity, items)
  assert_eq(1, remaining_space)
}

test "complex_calculation_workflow" {
  // 复杂计算工作流测试
  // 场景：项目资源计算
  let team_size = 5
  let tasks_per_person = 6
  let total_tasks = multiply(team_size, tasks_per_person)
  
  let sprint_capacity = 8
  let sprints_needed = divide_with_ceil(total_tasks, sprint_capacity)
  
  assert_eq(30, total_tasks)
  assert_eq(4, sprints_needed)
}

test "error_handling_boundaries" {
  // 错误处理边界测试
  // 除零错误处理
  assert_eq(0, divide_with_ceil(10, 0))
  assert_eq(0, divide_with_ceil(-10, 0))
  assert_eq(0, divide_with_ceil(0, 0))
  
  // 极值测试
  assert_eq(1, divide_with_ceil(1, 1))
  assert_eq(0, divide_with_ceil(0, 100))
}
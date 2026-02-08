// Azimuth 项目质量测试套件
// 包含8个高质量的标准 MoonBit 测试用例

test "quality_add_comprehensive" {
  // 综合加法测试
  assert_eq(42, add(20, 22))
  assert_eq(-8, add(-15, 7))
  assert_eq(0, add(-100, 100))
  assert_eq(1000, add(500, 500))
  assert_eq(1, add(0, 1))
}

test "quality_multiply_edge_cases" {
  // 乘法边界情况测试
  assert_eq(1, multiply(1, 1))
  assert_eq(-1, multiply(-1, 1))
  assert_eq(1, multiply(-1, -1))
  assert_eq(0, multiply(999, 0))
  assert_eq(1000000, multiply(1000, 1000))
}

test "quality_divide_ceil_precision" {
  // 向上取整除法精度测试
  assert_eq(1, divide_with_ceil(1, 1))
  assert_eq(2, divide_with_ceil(3, 2))
  assert_eq(100, divide_with_ceil(199, 2))
  assert_eq(0, divide_with_ceil(0, 100))
  assert_eq(0, divide_with_ceil(50, 0))
}

test "quality_divide_ceil_negative_comprehensive" {
  // 向上取整除法负数综合测试
  assert_eq(-2, divide_with_ceil(-3, 2))
  assert_eq(-1, divide_with_ceil(3, -2))
  assert_eq(2, divide_with_ceil(-3, -2))
  assert_eq(-50, divide_with_ceil(-100, 2))
  assert_eq(50, divide_with_ceil(-100, -2))
}

test "quality_greet_function_variations" {
  // 问候函数变体测试
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, 123!", greet("123"))
  assert_eq_string("Hello, 中文!", greet("中文"))
}

test "quality_subtract_comprehensive" {
  // 综合减法测试
  assert_eq(10, subtract(20, 10))
  assert_eq(-10, subtract(10, 20))
  assert_eq(0, subtract(100, 100))
  assert_eq(100, subtract(0, -100))
  assert_eq(-100, subtract(-100, 0))
}

test "quality_mathematical_properties" {
  // 数学性质验证测试
  let a = 15
  let b = 25
  
  // 加法交换律
  assert_eq(add(a, b), add(b, a))
  
  // 乘法交换律
  assert_eq(multiply(a, b), multiply(b, a))
  
  // 减法性质
  assert_eq(0, subtract(a, a))
  assert_eq(a, subtract(a, 0))
}

test "quality_business_scenario" {
  // 业务场景综合测试
  // 场景：计算项目资源需求
  let team_members = 7
  let tasks_per_member = 9
  let total_tasks = multiply(team_members, tasks_per_member)
  
  let sprint_capacity = 20
  let sprints_needed = divide_with_ceil(total_tasks, sprint_capacity)
  
  assert_eq(63, total_tasks)  // 7 * 9 = 63
  assert_eq(4, sprints_needed)  // ceil(63/20) = 4
  
  // 验证剩余容量
  let total_capacity = multiply(sprints_needed, sprint_capacity)
  let remaining_capacity = subtract(total_capacity, total_tasks)
  assert_eq(17, remaining_capacity)  // 4 * 20 - 63 = 17
}
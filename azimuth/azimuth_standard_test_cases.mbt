// Azimuth 项目标准测试用例
// 使用标准 MoonBit 测试语法，包含10个高质量测试用例

test "add_function_basic" {
  // 基本加法函数测试
  assert_eq(5, add(2, 3))
  assert_eq(0, add(0, 0))
  assert_eq(-5, add(-2, -3))
  assert_eq(10, add(-5, 15))
}

test "multiply_function_comprehensive" {
  // 全面乘法函数测试
  assert_eq(6, multiply(2, 3))
  assert_eq(0, multiply(5, 0))
  assert_eq(-6, multiply(-2, 3))
  assert_eq(15, multiply(-3, -5))
  assert_eq(100, multiply(10, 10))
}

test "subtract_function_validation" {
  // 减法函数验证测试
  assert_eq(7, subtract(10, 3))
  assert_eq(0, subtract(5, 5))
  assert_eq(-7, subtract(3, 10))
  assert_eq(-2, subtract(-5, -3))
  assert_eq(8, subtract(-5, -13))
}

test "divide_with_ceil_core_functionality" {
  // 向上取整除法核心功能测试
  assert_eq(4, divide_with_ceil(10, 3))
  assert_eq(3, divide_with_ceil(9, 3))
  assert_eq(1, divide_with_ceil(1, 2))
  assert_eq(0, divide_with_ceil(0, 5))
  assert_eq(0, divide_with_ceil(5, 0))
}

test "divide_with_ceil_negative_numbers" {
  // 向上取整除法负数测试
  assert_eq(-3, divide_with_ceil(-10, 3))
  assert_eq(-4, divide_with_ceil(10, -3))
  assert_eq(4, divide_with_ceil(-10, -3))
  assert_eq(-1, divide_with_ceil(-5, 10))
}

test "greet_function_various_inputs" {
  // 问候函数各种输入测试
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, 🚀!", greet("🚀"))
}

test "mathematical_commutative_properties" {
  // 数学交换律性质测试
  // 加法交换律
  assert_eq(add(7, 13), add(13, 7))
  assert_eq(add(-5, 10), add(10, -5))
  
  // 乘法交换律
  assert_eq(multiply(7, 13), multiply(13, 7))
  assert_eq(multiply(-4, 6), multiply(6, -4))
}

test "mathematical_identity_properties" {
  // 数学恒等式性质测试
  let x = 42
  
  // 加法恒等式
  assert_eq(x, add(x, 0))
  assert_eq(x, add(0, x))
  
  // 乘法恒等式
  assert_eq(x, multiply(x, 1))
  assert_eq(x, multiply(1, x))
  assert_eq(0, multiply(x, 0))
  
  // 减法恒等式
  assert_eq(0, subtract(x, x))
  assert_eq(x, subtract(x, 0))
}

test "complex_business_calculation" {
  // 复杂业务计算测试
  // 场景：计算项目资源需求
  let team_size = 8
  let tasks_per_person = 5
  let total_tasks = multiply(team_size, tasks_per_person)
  let tasks_per_sprint = 15
  let sprints_needed = divide_with_ceil(total_tasks, tasks_per_sprint)
  
  // 验证计算结果
  assert_eq(40, total_tasks)  // 8 * 5 = 40
  assert_eq(3, sprints_needed)  // ceil(40/15) = 3
}

test "resource_allocation_packaging" {
  // 资源分配包装测试
  // 场景：计算装盒问题
  let total_items = 25
  let items_per_box = 6
  let boxes_needed = divide_with_ceil(total_items, items_per_box)
  
  // 验证需要的箱子数量
  assert_eq(5, boxes_needed)  // ceil(25/6) = 5
  
  // 验证总容量
  let total_capacity = multiply(boxes_needed, items_per_box)
  assert_eq(30, total_capacity)  // 5 * 6 = 30
  
  // 验证剩余空间
  let remaining_space = subtract(total_capacity, total_items)
  assert_eq(5, remaining_space)  // 30 - 25 = 5
}
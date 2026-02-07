// 标准 MoonBit 测试用例 - 为 Azimuth 项目新增
// 包含10个高质量测试用例，使用标准 MoonBit 测试语法

test "standard_add_comprehensive" {
  // 加法函数综合测试
  assert_eq(25, add(12, 13))
  assert_eq(0, add(0, 0))
  assert_eq(-8, add(-3, -5))
  assert_eq(7, add(-10, 17))
}

test "standard_multiply_comprehensive" {
  // 乘法函数综合测试
  assert_eq(56, multiply(7, 8))
  assert_eq(0, multiply(15, 0))
  assert_eq(-35, multiply(-5, 7))
  assert_eq(48, multiply(-6, -8))
}

test "standard_divide_ceil_comprehensive" {
  // 向上取整除法综合测试
  assert_eq(5, divide_with_ceil(18, 4))
  assert_eq(4, divide_with_ceil(16, 4))
  assert_eq(1, divide_with_ceil(2, 7))
  assert_eq(0, divide_with_ceil(9, 0))
}

test "standard_greet_comprehensive" {
  // 问候函数综合测试
  assert_eq_string("Hello, Standard!", greet("Standard"))
  assert_eq_string("Hello, 测试!", greet("测试"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, 2025!", greet("2025"))
}

test "standard_subtract_comprehensive" {
  // 减法函数综合测试
  assert_eq(9, subtract(15, 6))
  assert_eq(-9, subtract(6, 15))
  assert_eq(0, subtract(11, 11))
  assert_eq(20, subtract(5, -15))
}

test "standard_math_properties_comprehensive" {
  // 数学性质验证测试
  let a = 14
  let b = 9
  
  // 加法交换律
  assert_eq(add(a, b), add(b, a))
  
  // 乘法交换律
  assert_eq(multiply(a, b), multiply(b, a))
  
  // 验证具体数值
  assert_eq(23, add(14, 9))
  assert_eq(126, multiply(14, 9))
}

test "standard_packaging_comprehensive" {
  // 包装计算场景测试
  let total_products = 67
  let products_per_box = 15
  let boxes_needed = divide_with_ceil(total_products, products_per_box)
  
  assert_eq(5, boxes_needed)
  
  // 验证总容量
  let total_capacity = multiply(boxes_needed, products_per_box)
  assert_eq(75, total_capacity)
  
  // 验证剩余空间
  let remaining_space = subtract(total_capacity, total_products)
  assert_eq(8, remaining_space)
}

test "standard_workflow_comprehensive" {
  // 工作流计算测试
  // 场景：项目任务分配
  let developers = 7
  let tasks_per_developer = 6
  let total_tasks = multiply(developers, tasks_per_developer)
  
  let sprint_capacity = 10
  let sprints_needed = divide_with_ceil(total_tasks, sprint_capacity)
  
  assert_eq(42, total_tasks)  // 7 * 6 = 42
  assert_eq(5, sprints_needed)  // ceil(42/10) = 5
}

test "standard_negative_comprehensive" {
  // 负数处理测试
  assert_eq(-15, add(-8, -7))
  assert_eq(3, subtract(-5, -8))
  assert_eq(40, multiply(-5, -8))
  assert_eq(-2, divide_with_ceil(-9, 5))
  assert_eq(2, divide_with_ceil(-9, -5))
}

test "standard_boundary_comprehensive" {
  // 边界值测试
  // 除法边界情况
  assert_eq(1, divide_with_ceil(15, 15))
  assert_eq(1, divide_with_ceil(-15, -15))
  assert_eq(0, divide_with_ceil(0, 15))
  
  // 大数运算
  assert_eq(3000, add(1500, 1500))
  assert_eq(2250000, multiply(1500, 1500))
  
  // 特殊问候情况
  assert_eq_string("Hello,     !", greet("     "))
  assert_eq_string("Hello, symbols#$%!", greet("symbols#$%"))
}
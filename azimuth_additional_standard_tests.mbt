// Azimuth 项目额外的标准 MoonBit 测试用例
// 包含 8 个高质量测试用例，使用标准 MoonBit 测试语法

test "azimuth_add_commutative_property" {
  // 测试加法交换律
  let a = 17
  let b = 29
  assert_eq(add(a, b), add(b, a))
  assert_eq(46, add(17, 29))
  assert_eq(46, add(29, 17))
}

test "azimuth_multiply_distributive_property" {
  // 测试乘法分配律: a * (b + c) = a * b + a * c
  let a = 5
  let b = 7
  let c = 3
  let left = multiply(a, add(b, c))
  let right = add(multiply(a, b), multiply(a, c))
  assert_eq(left, right)
  assert_eq(50, left)
}

test "azimuth_divide_with_ceil_precision" {
  // 测试向上取整除法的精度
  assert_eq(34, divide_with_ceil(100, 3))
  assert_eq(17, divide_with_ceil(50, 3))
  assert_eq(11, divide_with_ceil(32, 3))
  assert_eq(1, divide_with_ceil(2, 2))
  assert_eq(2, divide_with_ceil(3, 2))
}

test "azimuth_complex_workflow_calculation" {
  // 复杂工作流计算：项目资源分配
  let team_size = 7
  let story_points_per_member = 8
  let total_story_points = multiply(team_size, story_points_per_member)
  
  let sprint_capacity = 20
  let sprints_needed = divide_with_ceil(total_story_points, sprint_capacity)
  
  assert_eq(56, total_story_points)
  assert_eq(3, sprints_needed)
}

test "azimuth_greet_multilingual_support" {
  // 测试多语言问候支持
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, München!", greet("München"))
  assert_eq_string("Hello, São Paulo!", greet("São Paulo"))
  assert_eq_string("Hello, 🚀!", greet("🚀"))
}

test "azimuth_mathematical_series_calculation" {
  // 数学级数计算测试
  let n = 5
  let sum = add(1, add(2, add(3, add(4, 5))))
  let product = multiply(1, multiply(2, multiply(3, multiply(4, 5))))
  
  assert_eq(15, sum)
  assert_eq(120, product)
  
  let average = divide_with_ceil(sum, n)
  assert_eq(3, average)
}

test "azimuth_business_pricing_model" {
  // 业务定价模型测试
  let base_price = 100
  let quantity = 17
  let subtotal = multiply(base_price, quantity)
  
  let discount_threshold = 1000
  let discount_applicable = subtotal >= discount_threshold
  let final_price = if (discount_applicable) {
    subtract(subtract(subtotal, 100), 50)
  } else {
    subtotal
  }
  
  assert_eq(1700, subtotal)
  assert_eq(1550, final_price)
}

test "azimuth_edge_case_comprehensive" {
  // 综合边界情况测试
  // 零值测试
  assert_eq(0, add(0, 0))
  assert_eq(0, multiply(0, 5))
  assert_eq(5, subtract(5, 0))
  
  // 负数测试
  assert_eq(-8, add(-3, -5))
  assert_eq(15, multiply(-3, -5))
  assert_eq(-8, subtract(3, 11))
  
  // 除法边界
  assert_eq(1, divide_with_ceil(1, 1))
  assert_eq(0, divide_with_ceil(0, 100))
  assert_eq(0, divide_with_ceil(50, 0))
  
  // 字符串边界
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello,   !", greet("  "))
}
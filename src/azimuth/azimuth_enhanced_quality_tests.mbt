// Enhanced quality tests for azimuth library
// 覆盖更多边界情况和实际应用场景的高质量测试用例

test "large_number_division_precision" {
  // 测试大数除法的精度处理
  assert_eq(1000000, divide_with_ceil(999999, 1))
  assert_eq(500000, divide_with_ceil(999999, 2))
  assert_eq(333334, divide_with_ceil(1000000, 3))
  assert_eq(250000, divide_with_ceil(1000000, 4))
  assert_eq(200000, divide_with_ceil(999999, 5))
}

test "negative_zero_boundary_cases" {
  // 测试负零和零边界的特殊情况
  assert_eq(0, add(0, 0))
  assert_eq(0, multiply(0, 0))
  assert_eq(0, divide_with_ceil(0, 1))
  assert_eq(0, divide_with_ceil(0, -1))
  assert_eq(0, divide_with_ceil(0, 0))
}

test "string_concatenation_edge_cases" {
  // 测试字符串拼接的边界情况
  let empty_greeting = greet("")
  assert_eq_string("Hello, !", empty_greeting)
  
  let space_greeting = greet(" ")
  assert_eq_string("Hello,  !", space_greeting)
  
  let number_greeting = greet("123")
  assert_eq_string("Hello, 123!", number_greeting)
}

test "chained_function_operations" {
  // 测试链式函数操作
  let result1 = add(add(1, 2), add(3, 4))
  assert_eq(10, result1)
  
  let result2 = multiply(multiply(2, 3), multiply(4, 5))
  assert_eq(120, result2)
  
  let result3 = add(multiply(2, 3), divide_with_ceil(10, 3))
  assert_eq(10, result3)
}

test "alternating_sign_operations" {
  // 测试正负交替的运算
  assert_eq(0, add(5, -5))
  assert_eq(-2, add(-3, 1))
  assert_eq(2, add(-5, 7))
  assert_eq(-15, multiply(-3, 5))
  assert_eq(15, multiply(-3, -5))
  assert_eq(-2, divide_with_ceil(-5, 3))
}

test "boundary_value_analysis" {
  // 边界值分析测试
  assert_eq(1, divide_with_ceil(1, 100))
  assert_eq(1, divide_with_ceil(99, 100))
  assert_eq(1, divide_with_ceil(100, 100))
  assert_eq(2, divide_with_ceil(101, 100))
  assert_eq(2, divide_with_ceil(199, 100))
  assert_eq(2, divide_with_ceil(200, 100))
}

test "practical_business_calculations" {
  // 实际业务计算场景
  // 计算订单总数：商品A单价12元数量3个，商品B单价8元数量5个
  let product_a_total = multiply(12, 3)
  let product_b_total = multiply(8, 5)
  let order_total = add(product_a_total, product_b_total)
  assert_eq(76, order_total)
  
  // 计算需要的包装箱数量（每箱装10个）
  let total_items = add(3, 5)
  let boxes_needed = divide_with_ceil(total_items, 10)
  assert_eq(1, boxes_needed)
}

test "mathematical_identities" {
  // 数学恒等式测试
  let x = 7
  let y = 13
  
  // 加法恒等式
  assert_eq(add(x, y), add(y, x))
  assert_eq(add(x, 0), x)
  
  // 乘法恒等式
  assert_eq(multiply(x, y), multiply(y, x))
  assert_eq(multiply(x, 1), x)
  assert_eq(multiply(x, 0), 0)
}

test "complex_nested_calculations" {
  // 复杂嵌套计算测试
  let a = 10
  let b = 20
  let c = 30
  
  // 计算：(a + b) * c / (a + c)
  let numerator = multiply(add(a, b), c)
  let denominator = add(a, c)
  let result = divide_with_ceil(numerator, denominator)
  assert_eq(20, result)  // ceil((10+20)*30/(10+30)) = ceil(900/40) = ceil(22.5) = 23
}

test "resource_allocation_scenario" {
  // 资源分配场景测试
  let total_budget = 1000
  let project_cost = 350
  let remaining_budget = add(total_budget, -project_cost)
  
  // 计算可以支持的小项目数量（每个小项目需要120元）
  let small_projects = divide_with_ceil(remaining_budget, 120)
  assert_eq(6, small_projects)  // ceil(650/120) = ceil(5.416) = 6
  
  // 验证总成本
  let total_cost = add(project_cost, multiply(small_projects, 120))
  assert_eq(1070, total_cost)
}
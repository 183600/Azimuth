// Azimuth 库的标准 MoonBit 测试用例
// 包含 8 个高质量测试用例，覆盖核心功能和边缘情况

test "subtract_function_comprehensive" {
  // 全面测试减法函数
  assert_eq(5, subtract(10, 5))
  assert_eq(-5, subtract(5, 10))
  assert_eq(0, subtract(5, 5))
  assert_eq(15, subtract(10, -5))
  assert_eq(-15, subtract(-10, 5))
}

test "divide_with_ceil_precision_edge_cases" {
  // 测试向上取整除法的精度边缘情况
  assert_eq(1, divide_with_ceil(1, 2))
  assert_eq(2, divide_with_ceil(3, 2))
  assert_eq(100, divide_with_ceil(199, 2))
  assert_eq(67, divide_with_ceil(200, 3))
  assert_eq(34, divide_with_ceil(100, 3))
}

test "mathematical_identity_properties_extended" {
  // 扩展的数学恒等式测试
  let x = 42
  
  // 加法恒等式
  assert_eq(x, add(x, 0))
  assert_eq(x, add(0, x))
  
  // 乘法恒等式
  assert_eq(x, multiply(x, 1))
  assert_eq(x, multiply(1, x))
  assert_eq(0, multiply(x, 0))
  assert_eq(0, multiply(0, x))
  
  // 减法恒等式
  assert_eq(0, subtract(x, x))
  assert_eq(x, subtract(x, 0))
}

test "complex_business_logistics" {
  // 复杂业务物流场景测试
  // 场景：仓库库存管理
  let product_a_qty = 125
  let product_b_qty = 87
  let product_c_qty = 156
  
  let box_capacity_a = 24
  let box_capacity_b = 18
  let box_capacity_c = 30
  
  // 计算每种产品需要的箱子数量
  let boxes_a = divide_with_ceil(product_a_qty, box_capacity_a)
  let boxes_b = divide_with_ceil(product_b_qty, box_capacity_b)
  let boxes_c = divide_with_ceil(product_c_qty, box_capacity_c)
  
  assert_eq(6, boxes_a)   // ceil(125/24) = 6
  assert_eq(5, boxes_b)   // ceil(87/18) = 5
  assert_eq(6, boxes_c)   // ceil(156/30) = 6
  
  // 计算总箱子数和总容量
  let total_boxes = add(add(boxes_a, boxes_b), boxes_c)
  let total_capacity = add(
    multiply(boxes_a, box_capacity_a),
    add(multiply(boxes_b, box_capacity_b), multiply(boxes_c, box_capacity_c))
  )
  
  assert_eq(17, total_boxes)
  assert_eq(408, total_capacity)
}

test "string_formatting_special_characters" {
  // 字符串格式化特殊字符测试
  assert_eq_string("Hello, newline\n!", greet("newline\n"))
  assert_eq_string("Hello, tab\t!", greet("tab\t"))
  assert_eq_string("Hello, quote\"!", greet("quote\""))
  assert_eq_string("Hello, apostrophe'!", greet("apostrophe'"))
  assert_eq_string("Hello, backslash\\!", greet("backslash\\"))
}

test "arithmetic_operation_chain" {
  // 算术运算链测试
  let initial = 100
  
  // 运算链：((initial - 20) * 3) / 25 + 10
  let step1 = subtract(initial, 20)        // 80
  let step2 = multiply(step1, 3)           // 240
  let step3 = divide_with_ceil(step2, 25)  // ceil(240/25) = 10
  let result = add(step3, 10)              // 20
  
  assert_eq(20, result)
}

test "resource_planning_scenario" {
  // 资源规划场景测试
  // 场景：项目团队资源分配
  let total_tasks = 150
  let senior_developers = 3
  let junior_developers = 5
  let senior_capacity = 8
  let junior_capacity = 5
  
  // 计算团队总容量
  let daily_capacity = add(
    multiply(senior_developers, senior_capacity),
    multiply(junior_developers, junior_capacity)
  )
  
  assert_eq(49, daily_capacity)  // 3*8 + 5*5 = 24 + 25 = 49
  
  // 计算完成项目所需天数
  let days_needed = divide_with_ceil(total_tasks, daily_capacity)
  assert_eq(4, days_needed)  // ceil(150/49) = 4
  
  // 计算实际完成任务数
  let total_completed = multiply(days_needed, daily_capacity)
  assert_eq(196, total_completed)
  
  // 计算额外容量
  let extra_capacity = subtract(total_completed, total_tasks)
  assert_eq(46, extra_capacity)
}

test "error_handling_comprehensive" {
  // 综合错误处理测试
  // 除零错误的各种情况
  assert_eq(0, divide_with_ceil(0, 0))
  assert_eq(0, divide_with_ceil(100, 0))
  assert_eq(0, divide_with_ceil(-100, 0))
  assert_eq(0, divide_with_ceil(2147483647, 0))
  assert_eq(0, divide_with_ceil(-2147483648, 0))
  
  // 边界值测试
  assert_eq(1, divide_with_ceil(1, 1))
  assert_eq(1, divide_with_ceil(-1, -1))
  assert_eq(0, divide_with_ceil(0, 1))
  assert_eq(0, divide_with_ceil(0, -1))
  
  // 极值减法测试
  assert_eq(2147483647, subtract(2147483647, 0))
  assert_eq(-2147483648, subtract(-2147483648, 0))
  assert_eq(0, subtract(2147483647, 2147483647))
}